# `CME.plot` examples
devtools::load_all()

workdir_current <- get.workdir(2026)
workdir_previous <- get.workdir(2025)

dir_IGME_out_folder <- file.path(workdir_current, "output")
dir_IGME_out_folder_last <- file.path(workdir_previous, "output")

output.dir <- file.path(dir_IGME_out_folder, "GR20250530_all")
output.dir.for.comparison <- file.path(dir_IGME_out_folder_last, "GR20240516_all")

fig.dir <- file.path(workdir_current, "fig")

has_model_output <- function(output_dir) {
  dir.exists(output_dir) &&
    file.exists(file.path(output_dir, "iso.c.rda")) &&
    file.exists(file.path(output_dir, "year.t.rda")) &&
    length(get.res.cqt.rda.diffname(output_dir, name_only = TRUE)) > 0
}

if (has_model_output(output.dir) && has_model_output(output.dir.for.comparison)) {
  cqt_last_year <- obtain.matched.cqt(
    output.dir1 = output.dir,
    output.dir2 = output.dir.for.comparison,
    pooling_weight = 0.5
  )
} else {
  message("Skipping matched CQT example because required model-output files are not available.")
}

if (dir.exists(output.dir)) {
  # review the entries' date in "data_CMEInfo.csv"
  review.date.of.dataentry(output.dir = output.dir)
} else {
  message("Skipping data-entry review because output.dir does not exist: ", output.dir)
}

# The difference in VR is highlighted by comparing a new dataset to an old one.
new_dirs <- tryCatch(
  find.dir.for.VR.comparison(
    IGME_year_new = 2026,
    IGME_year_old = 2025,
    filename_new = NULL,
    filename_old = NULL
  ),
  error = function(e) {
    message("Skipping VR comparison setup: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(new_dirs)) {
  # Supply in the global environment to overwrite:
  # if `dir_new_data_U5MR` is supplied, it will be used as the new file to compare VR
  # used in function `get.diff.dt.WHOVR`
  dir_new_data_U5MR <- new_dirs$dir_new_data_U5MR
  dir_old_data_U5MR <- new_dirs$dir_old_data_U5MR
}

run_plot_examples <- isTRUE(getOption("CME.plot.run_data_raw_plot_examples", FALSE))

if (run_plot_examples && dir.exists(output.dir)) {
  # minimum results/data plot:
  year.lastestimatepublished = 2026
  savePlotResults(runname = "UNIGME2025",
                  output.dir = output.dir,
                  legend1 = "UN IGME 2026", # if set as NULL, suppress estimates 1
                  fig.dir = fig.dir,
                  new_entry_date = "2025-12",
                  wpp.cqt = get.wpp.cqt("U5MR"),
                  ihme.cqt = get.gbd.cqt("U5MR"))

  # add more comparison:
  savePlotResults(runname = "GR20250530",
                  output.dir = output.dir,
                  legend1 = "UN IGME 2026",
                  legend2 = "GBD 2021",
                  res.cqt2 = get.gbd.cqt("U5MR"),
                  # col.CI4 = NULL,
                  fig.dir = fig.dir,
                  wpp.cqt = get.wpp.cqt("U5MR"),
                  legend_IHME = "GBD 2019",
                  ihme.cqt = u5mr.ihme.cqt.2019,
                  new_entry_date = "2025-12")

  # using a specific iso order
  savePlotResults(runname = "test",
                  output.dir = output.dir,
                  pdf.or.png = "pdf",
                  fig.dir = fig.dir,
                  iso.subset.c = c("CAF", "CIV", "AFG"),
                  sort_the_isos = FALSE)

  # show wpp and ihme lines:
  savePlotResults(runname = "GR20250530",
                  filename = "extra note",
                  output.dir = output.dir,
                  fig.dir = fig.dir,
                  wpp.cqt = get.wpp.cqt("U5MR"),
                  ihme.cqt = u5mr.ihme.cqt.2019,
                  n.countries = 1:10) # save only 10 countries

  # show all lines (and make comparison to WPP and GBD):
  savePlotResults(runname = "GR20250530",
                  new_entry_date = "2025-12",
                  output.dir = output.dir,
                  fig.dir = fig.dir,
                  legend1 = "UN IGME estimates 2026",
                  legend2 = "WPP 2025",
                  res.cqt2 = get.wpp.cqt("U5MR", wpp_round = 2025),
                  # legend4 = "GBD 2021",
                  # res.cqt4 = get.gbd.cqt("U5MR"),
                  legend_WPP = "WPP 2025",
                  wpp.cqt = get.wpp.cqt("U5MR", wpp_round = 2025),
                  # legend_IHME = "IHME GBD 2019",
                  # ihme.cqt = u5mr.ihme.cqt.2019,
                  n.countries = NULL) # save all countries
} else {
  message(
    "Skipping plot examples. Set options(CME.plot.run_data_raw_plot_examples = TRUE) ",
    "and provide output.dir to run them."
  )
}
