env <- new.env(parent = globalenv())
sys.source(file.path("R", "1.helper_plot_funcs.R"), envir = env)

tmp_pkg <- tempfile("cme-plot-data-update-")
dir.create(tmp_pkg)
on.exit(unlink(tmp_pkg, recursive = TRUE), add = TRUE)

writeLines(
  c(
    "Package: cmeplotprobe",
    "Title: CME Plot Probe",
    "Version: 0.0.0",
    "Authors@R: person('A', 'B', role = 'cre', email = 'a@example.com')",
    "Description: Temporary package used by tests.",
    "License: MIT",
    "Encoding: UTF-8"
  ),
  file.path(tmp_pkg, "DESCRIPTION")
)

env$captured_year <- NULL
env$get.workdir.sharepoint <- function(year) {
  captured_year <<- year
  "unused"
}
environment(env$get.workdir.sharepoint) <- env
env$get.new_cnames <- function(workdir, filename) {
  data.frame(
    ISO3Code = c("AAA", "XKX"),
    UNCode = c(1, 412),
    OfficialName = c("Alpha", "Kosovo (UNSCR 1244)"),
    stringsAsFactors = FALSE
  )
}

usethis::local_project(tmp_pkg)
env$update.data.new_cnames(
  IGME_round_input = 2026,
  country.info.file.name = "country.info.CME.csv"
)

stopifnot(env$captured_year == 2026)
stopifnot(file.exists(file.path(tmp_pkg, "data", "new_cnames.rda")))
stopifnot(file.exists(file.path(tmp_pkg, "data", "u5mr.iso.c.rda")))

load(file.path(tmp_pkg, "data", "new_cnames.rda"))
load(file.path(tmp_pkg, "data", "u5mr.iso.c.rda"))

stopifnot(identical(new_cnames$ISO3Code, c("AAA", "XKX")))
stopifnot(identical(u5mr.iso.c, c("AAA", "XKX")))
