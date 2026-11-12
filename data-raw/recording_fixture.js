// Builds tests/testthat/testdata/recording.xlsx with the builder of the recording form, outside the browser, so the
// tests of read_recording() read the file the form writes. Run from the package root after changing the form:
//
//   node data-raw/recording_fixture.js inst/sheets/step-test-form.html tests/testthat/testdata/recording
//
// It also writes the .csv of the stages next to it, to look at; only the .xlsx is used.
const fs = require("fs");
const [form, prefix] = process.argv.slice(2);
const html = fs.readFileSync(form, "utf8");
const code = html.split(/\/\/ BEGIN builder[^\n]*\n/)[1].split("// END builder")[0];
const { csvStages, recordSheets, xlsxBytes } = new Function(code + "; return { csvStages, recordSheets, xlsxBytes };")();

const fields = {
  name: "Ana Silva & <Co>", date: "2026-11-04", time_of_day: "09:30", sport: "cycling", sex: "female", age: 27,
  body_mass: 61.5, last_meal: "07:00, oats", caffeine: null, training_48h: "easy 1 h", other: null,
  ergometer: "Lode Excalibur", start: 50, increment: 25, stage_duration: 3, pause: 30, treadmill_grade: null,
  site: "earlobe", sample: "blood", analyzer: "Biosen C-Line, 08:45", room: "21 C, 45%", rpe_scale: "6-20",
  tester: "F. Mattioni Maturana", end_reason: "volitional exhaustion", last_stage_s: 120, peak_heart_rate: 198,
  notes: "felt good; \"tough\" last stage"
};
const lactate = [0.93, 0.98, 1.23, 1.88, 2.8, 4.22, 6.66, 8.64];
const intensity = [0, 50, 75, 100, 125, 150, 175, 191];
const heart = [96, 136, 146, 154, 170, 182, 191, 198];
const rpe = [6, 8, 10, 11, 13, 15, 17, 19];
const record = {
  fields,
  stages: intensity.map((v, i) => ({ intensity: v, lactate: lactate[i], heart_rate: heart[i], rpe: rpe[i],
    clock: i ? `09:${String(30 + 3 * i).padStart(2, "0")}` : null, notes: i === 7 ? "stopped at 2 min" : null }))
    .concat([{ intensity: null, lactate: null, heart_rate: null, rpe: null, clock: null, notes: null }]),
  recovery: [[1, 9.3], [3, 9.6], [5, 9.0], [7, 8.2], [10, 7.1], [15, null]].map(([t, l]) => ({ time: t, lactate: l }))
};
fs.writeFileSync(prefix + ".xlsx", Buffer.from(xlsxBytes(recordSheets(record))));
fs.writeFileSync(prefix + ".csv", csvStages(record));
console.log("wrote", prefix + ".xlsx", fs.statSync(prefix + ".xlsx").size, "bytes and", prefix + ".csv");
