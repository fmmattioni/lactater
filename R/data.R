#' Demo data
#'
#' A dataset containing the lactate and heart rate data collected from a step-incremental
#' test as an example
#'
#' @format A data frame with 8 rows and 5 variables:
#' \describe{
#'   \item{step}{the number of the step, starting at zero for the baseline.}
#'   \item{length}{the length of each step.}
#'   \item{intensity}{the intensity performed on each step (in this case in watts).}
#'   \item{lactate}{the blood lactate concentration.}
#'   \item{heart_rate}{the heart rate associated with that step.}
#' }
"demo_data"

#' Lactate step tests from different sports, athletes and scenarios
#'
#' Fifteen incremental step tests in long format, one row per stage, to try every part of lactater on
#' real data: cycling, running, rowing and swimming; elite, trained and untrained athletes; one athlete
#' tested six times over five years; and the cases that need care (an early lactate dip, no resting
#' sample, 5-min steps, a submaximal test). Analyze one test with `subset()`, or several tests of one
#' sport with `.by = test`.
#'
#' Twelve tests are real: eleven from the Trinity College Dublin graded exercise test data (3-min stages,
#' lactate sampled at 2 min, YSI analyser; running on a treadmill at 0% grade; rowing with 1 min of rest
#' between stages) and one from Heuberger et al. (2018) (5-min steps from 175 W). Names, birth dates and
#' test dates were removed. Three tests are simulated, where no open data exists: a swimmer's 7 x 200 m
#' step test (+0.05 m/s per step; Pelarigo et al., 2017), and an untrained client tested to exhaustion and
#' stopped early (25 W steps; untrained men peaked near 250 W in Messonnier et al., 2013). Their lactate
#' rises exponentially through about 2.5 mmol/L near the first threshold, with analyser scatter of 3% from
#' 2 mmol/L and 5% below (Bonaventura et al., 2015). See `data-raw/lactate_tests.R`.
#'
#' @format A tibble with one row per stage and 12 columns:
#' \describe{
#'   \item{test}{the test: `cycling_elite`, `cycling_female`, `cycling_early_dip`, `cycling_series_1` to
#'   `cycling_series_6`, `running`, `rowing`, `cycling_5min_no_rest`, `swimming`, `untrained`,
#'   `submaximal`.}
#'   \item{athlete}{the athlete; the six `cycling_series` tests share `cyclist_series`.}
#'   \item{sport}{`cycling`, `running`, `rowing`, or `swimming`, as in `lactate_threshold(sport = )`.}
#'   \item{sex}{`M` or `F`.}
#'   \item{body_mass}{body mass (kg) on the test day, `NA` when not reported.}
#'   \item{day}{days since the athlete's first test (0 for single tests).}
#'   \item{description}{who was tested, and how.}
#'   \item{source}{`Trinity College Dublin (CC BY 4.0)`, `Heuberger et al. 2018 (CC BY 4.0)`, or
#'   `simulated`.}
#'   \item{step}{the stage, 0 for the resting sample.}
#'   \item{intensity}{power (W) for cycling and rowing, speed (km/h) for running, speed (m/s) for swimming;
#'   0 for the resting sample.}
#'   \item{lactate}{blood lactate (mmol/L).}
#'   \item{heart_rate}{heart rate (bpm), `NA` when not recorded.}
#' }
#'
#' @source
#' Trinity College Dublin graded exercise test data. Zenodo. \doi{10.5281/zenodo.10841412}. License CC BY 4.0.
#'
#' Heuberger JAAC, Gal P, Stuurman FE, de Muinck Keizer WAS, Mejia Miranda Y, Cohen AF. Repeatability and
#' predictive value of lactate threshold concepts in endurance sports. PLOS ONE. 2018;13(11):e0206846.
#' Data: figshare, \doi{10.6084/m9.figshare.7240571}. License CC BY 4.0.
#'
#' @references
#' Bonaventura JM, Sharpe K, Knight E, Fuller KL, Tanner RK, Gore CJ. Reliability and accuracy of six hand-held
#' blood lactate analysers. Journal of Sports Science and Medicine. 2015;14:203-214.
#'
#' Messonnier LA, Emhoff CAW, Fattor JA, Horning MA, Carlson TJ, Brooks GA. Lactate kinetics at the lactate
#' threshold in trained and untrained men. Journal of Applied Physiology. 2013;114(11):1593-1602.
#'
#' Pelarigo JG, Machado L, Fernandes RJ, Greco CC, Vilas-Boas JP. Oxygen uptake kinetics and energy system's
#' contribution around maximal lactate steady state swimming intensity. PLOS ONE. 2017;12(2):e0167263.
#'
#' @examples
#' unique(lactate_tests[c("test", "sport", "description")])
#' rowing <- subset(lactate_tests, test == "rowing")
#' lactate_threshold(rowing, intensity, lactate, heart_rate, sport = "rowing", plot = FALSE)
"lactate_tests"
