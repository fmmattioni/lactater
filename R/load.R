#' Training load and intensity distribution from the lactate zones
#'
#' Sorts the samples of training sessions (heart rate, or power or speed) into the zones of [lactate_zones()], and
#' returns the time in each zone and the training intensity distribution with its polarization index. It prints a
#' short summary and can be plotted with [autoplot()].
#'
#' * **Time in zone**: every sample counts for `interval` seconds in the zone of its value. Missing samples are left
#'   out. Heart rate lags behind intensity in short intervals, so heart-rate time in zone under-counts the highest
#'   zones in interval sessions; power or speed samples avoid that (Seiler, 2010).
#' * **Intensity distribution** across the three domains (moderate below LT1, heavy from LT1 to LT2, severe above
#'   LT2; with five zones, zones 1-2 and 4-5 are added up). The polarization index is
#'   `log10((moderate / heavy) * severe * 100)`, with each domain as a fraction of the total time; when the heavy domain
#'   is empty it is set to 0.01 and 0.01 is taken from the severe domain; it is 0 when the severe domain is empty, and
#'   not computed (`NA`) when the severe domain exceeds the moderate one. A distribution is `"polarized"` when the
#'   index is above 2.00 and moderate > severe > heavy (Treff et al., 2019). Otherwise it is labeled `"pyramidal"`
#'   (moderate > heavy > severe), `"threshold"` (heavy largest), `"high intensity"` (severe largest), or `"other"`.
#'   Treff et al. (2019) note that a threshold distribution can also be pyramidal.
#'
#' The distribution describes a training period, so give it the samples of all the sessions of that period (e.g.
#' `.by = "week"`).
#'
#' @param data A data frame with one row per sample, or a numeric vector of samples.
#' @param zones The result of [lactate_zones()], with three or five zones.
#' @param column The column of `data` with the samples. Defaults to the name of `measure`.
#' @param measure What the samples are: `"heart_rate"` (bpm; uses the heart rate at the zone boundaries) or
#' `"intensity"` (power or speed in the unit of the test).
#' @param interval The time between two samples, in seconds. Defaults to `1`.
#' @param .by Names of columns that group the samples (e.g. `"session"` or `"week"`), kept in the order they appear.
#'
#' @return a `lactate_load` tibble with one row per group: `minutes`, the minutes in each zone (`zone_1`, ...), the
#' percentage of time in each zone (`zone_1_percent`, ...), with five zones the percentage in each domain
#' (`moderate_percent`, `heavy_percent`, `severe_percent`), `polarization_index`, and `distribution`.
#'
#' @references
#' Seiler S. What is best practice for training intensity and duration distribution in endurance athletes?
#' International Journal of Sports Physiology and Performance. 2010;5(3):276-291.
#'
#' Treff G, Winkert K, Sareban M, Steinacker JM, Sperlich B. The polarization-index: a simple calculation to
#' distinguish polarized from non-polarized training intensity distributions. Frontiers in Physiology.
#' 2019;10:707.
#'
#' @export
#'
#' @examples
#' results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
#' zones <- lactate_zones(results, lt1 = "Log-log", lt2 = "ModDmax")
#' ## two sessions of heart rate sampled every 5 s
#' sessions <- data.frame(
#'   session = rep(c("easy", "intervals"), each = 720),
#'   heart_rate = c(rep(140, 720), rep(c(150, 185), times = 360))
#' )
#' load <- training_load(sessions, zones, interval = 5, .by = "session")
#' load
#' autoplot(load)
training_load <- function(data, zones, column = measure, measure = c("heart_rate", "intensity"),
                          interval = 1, .by = NULL) {
  measure <- match.arg(measure)
  if(!inherits(zones, "lactate_zones")) stop("`zones` must be the result of `lactate_zones()`.", call. = FALSE)
  if(!is.numeric(interval) || length(interval) != 1 || is.na(interval) || interval <= 0)
    stop("`interval` must be a single positive number of seconds.", call. = FALSE)
  if(is.numeric(data)) data <- stats::setNames(data.frame(data), column)
  if(!column %in% names(data)) stop(sprintf("Column `%s` not found in `data`.", column), call. = FALSE)
  if(!is.null(.by) && !all(.by %in% names(data))) stop("`.by` must name columns of `data`.", call. = FALSE)

  n <- nrow(zones)
  bounds <- if(measure == "heart_rate") {
    if(is.null(zones$heart_rate_to))
      stop("These zones have no heart rate; use `measure = \"intensity\"`.", call. = FALSE)
    zones$heart_rate_to[-n]
  } else {
    attr(zones, "lactater_zones")$bounds
  }
  if(anyNA(bounds)) stop("A zone boundary has no value for this measure.", call. = FALSE)

  groups <- if(is.null(.by)) {
    list(data)
  } else {
    key <- do.call(paste, c(data[.by], sep = "\r"))
    split(data, factor(key, levels = unique(key)))
  }
  minutes <- lapply(groups, function(group) {
    values <- group[[column]]
    tabulate(findInterval(values[!is.na(values)], bounds) + 1, n) * interval / 60
  })
  rows <- Map(function(group, minutes) {
    keys <- if(!is.null(.by)) tibble::as_tibble(group[1, .by, drop = FALSE])
    vctrs::vec_cbind(keys, zone_summary(minutes, zones$domain))
  }, groups, minutes)
  out <- tibble::as_tibble(vctrs::vec_rbind(!!!unname(rows)))
  ## the overall summary from the unrounded minutes, so it matches a single group exactly
  attr(out, "lactater_load") <- list(
    zones = zones, measure = measure, by = .by, overall = zone_summary(Reduce(`+`, minutes, numeric(n)), zones$domain)
  )
  class(out) <- c("lactate_load", class(out))
  out
}

#' Time in zone and intensity distribution from the minutes in each zone
#'
#' @param minutes The minutes in each zone.
#' @param domain The domain of each zone (`"moderate"`, `"heavy"`, `"severe"`).
#' @return a one-row data frame.
#' @keywords internal
zone_summary <- function(minutes, domain) {
  total <- sum(minutes)
  n <- length(minutes)
  share <- if(total > 0) minutes / total else rep(NA_real_, n)
  domains <- vapply(c("moderate", "heavy", "severe"), function(d) sum(share[domain == d]), numeric(1))
  out <- data.frame(minutes = round(total, 1))
  out[paste0("zone_", seq_len(n))] <- as.list(round(minutes, 1))
  out[paste0("zone_", seq_len(n), "_percent")] <- as.list(round(100 * share, 1))
  if(n > 3) out[paste0(names(domains), "_percent")] <- as.list(round(100 * domains, 1))
  out$polarization_index <- round(polarization_index(domains), 2)
  out$distribution <- intensity_distribution(domains)
  out
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.lactate_load <- function(x, ...) {
  load <- attr(x, "lactater_load")
  if(is.null(load)) return(NextMethod())
  overall <- load$overall
  domains <- if(!is.null(overall$moderate_percent)) {
    unlist(overall[c("moderate_percent", "heavy_percent", "severe_percent")])
  } else {
    unlist(overall[c("zone_1_percent", "zone_2_percent", "zone_3_percent")])
  }
  c(
    "Training load" = sprintf(
      "%s, %.1f h in %d zones (%s)", if(nrow(x) > 1) paste(nrow(x), "groups") else "1 group",
      overall$minutes / 60, nrow(load$zones), sub("_", " ", load$measure)
    ),
    "Distribution" = sprintf(
      "%s: %.0f%% moderate, %.0f%% heavy, %.0f%% severe (polarization index %s)", overall$distribution,
      domains[1], domains[2], domains[3], format(overall$polarization_index)
    )
  )
}

#' @rdname training_load
#' @param object The result of [training_load()].
#' @param ... Not used.
#' @export
autoplot.lactate_load <- function(object, ...) {
  load <- attr(object, "lactater_load")
  colours <- lactater_colours()
  n <- nrow(load$zones)
  keys <- if(is.null(load$by)) "All" else do.call(paste, c(as.list(object[load$by]), sep = " "))
  ## minutes, not shares, so that a long session looks long; the shares of the whole are in the subtitle
  minutes <- as.matrix(as.data.frame(object)[paste0("zone_", seq_len(n))])
  long <- data.frame(
    group = factor(rep(keys, n), levels = rev(unique(keys))),
    zone = factor(rep(paste("Zone", seq_len(n)), each = length(keys)), levels = rev(paste("Zone", seq_len(n)))),
    minutes = as.vector(minutes)
  )
  widest <- max(rowSums(minutes))
  long$text <- ifelse(
    long$minutes > 0.18 * widest, sprintf("%s  %.0f min", long$zone, long$minutes),
    ifelse(long$minutes > 0.05 * widest, sprintf("%.0f", long$minutes), "")
  )
  totals <- data.frame(group = factor(keys, levels = levels(long$group)), minutes = rowSums(minutes))
  overall <- load$overall
  domains <- if(n > 3) {
    unlist(overall[c("moderate_percent", "heavy_percent", "severe_percent")])
  } else {
    unlist(overall[c("zone_1_percent", "zone_2_percent", "zone_3_percent")])
  }

  ggplot2::ggplot(long, ggplot2::aes(minutes, group)) +
    ggplot2::geom_col(ggplot2::aes(fill = zone), width = 0.66, colour = colours[["paper"]], linewidth = 0.6, alpha = 0.55) +
    ggplot2::geom_text(
      ggplot2::aes(label = text, group = zone), position = ggplot2::position_stack(vjust = 0.5), size = 3,
      colour = colours[["ink"]]
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f min", minutes)), totals, hjust = -0.2, size = 3, colour = colours[["muted"]]
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(zone_colours[[as.character(n)]], paste("Zone", seq_len(n))), guide = "none"
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      title = sprintf(
        "%s%s: %.0f%% of %.0f min below LT1", toupper(substr(overall$distribution, 1, 1)),
        substring(overall$distribution, 2), domains[1], overall$minutes
      ),
      subtitle = sprintf(
        "Minutes in each zone (%s): %.0f%% below LT1, %.0f%% between, %.0f%% above LT2; polarization index %s",
        sub("_", " ", load$measure), domains[1], domains[2], domains[3], format(overall$polarization_index)
      ),
      x = "Minutes", y = NULL, fill = NULL
    ) +
    theme_lactater() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(colour = colours[["ink"]])
    )
}

#' Polarization index (Treff et al., 2019)
#'
#' @param share The fractions of time in zones 1, 2 and 3.
#' @return the index, 0 when zone 3 is empty, and `NA` when zone 3 exceeds zone 1.
#' @keywords internal
polarization_index <- function(share) {
  if(anyNA(share) || share[3] > share[1]) return(NA_real_)
  if(share[3] == 0) return(0)
  ## Eq. 2: an empty zone 2 becomes 0.01, taken from zone 3
  if(share[2] == 0) share[2:3] <- c(0.01, share[3] - 0.01)
  log10(share[1] / share[2] * share[3] * 100)
}

#' Training intensity distribution label
#'
#' @param share The fractions of time in zones 1, 2 and 3.
#' @return `"polarized"`, `"pyramidal"`, `"threshold"`, `"high intensity"`, or `"other"`.
#' @keywords internal
intensity_distribution <- function(share) {
  if(anyNA(share)) return(NA_character_)
  index <- polarization_index(share)
  if(!is.na(index) && index > 2 && share[1] > share[3] && share[3] > share[2]) return("polarized")
  if(share[1] > share[2] && share[2] > share[3]) return("pyramidal")
  largest <- which.max(share)
  c("other", "threshold", "high intensity")[largest]
}
