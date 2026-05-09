if (requireNamespace("CME.plot", quietly = TRUE)) {
  revise_path <- CME.plot::revise.path
  get_plot_country_indices <- CME.plot:::get.plot.country.indices
  get_dir_NMR <- CME.plot::get.dir_NMR
  get_dir_gender <- CME.plot::get.dir_gender
  check_cqt_vs_results_csv <- CME.plot::check.cqt.vs.results.csv
} else {
  suppressPackageStartupMessages(library(data.table))
  suppressPackageStartupMessages(library(reshape2))
  sys.source(file.path("R", "funcs_find_directories.R"), envir = environment())
  sys.source(file.path("R", "1.helper_plot_funcs.R"), envir = environment())
  revise_path <- revise.path
  get_plot_country_indices <- get.plot.country.indices
  get_dir_NMR <- get.dir_NMR
  get_dir_gender <- get.dir_gender
  check_cqt_vs_results_csv <- check.cqt.vs.results.csv
}

forward_slash_dir <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
stopifnot(identical(revise_path(forward_slash_dir), forward_slash_dir))

stopifnot(identical(
  get_plot_country_indices(
    iso.c = c("AAA", "BBB", "CCC"),
    n.countries = 1:2,
    sort_the_isos = FALSE
  ),
  1:2
))

stopifnot(identical(
  get_plot_country_indices(
    iso.c = c("AAA", "BBB", "CCC"),
    iso.subset.c = c("CCC", "AAA"),
    sort_the_isos = FALSE
  ),
  c(3L, 1L)
))

empty_dir <- tempfile("empty-data-")
dir.create(empty_dir)
stopifnot(is.null(get_dir_NMR(dir_IGME_NMR = empty_dir)))
stopifnot(is.null(get_dir_gender(dir_IGME_gender = empty_dir)))

tmp <- tempfile("cqt-check-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

res.cqt.Lw <- list("0.5" = array(
  c(1, 2, 3),
  dim = c(1, 3, 1),
  dimnames = list("AAA", c("0.05", "0.5", "0.95"), "2020.5")
))
save(res.cqt.Lw, file = file.path(tmp, "res.cqt.Lw.rda"))

results <- data.frame(
  ISO.Code = rep("AAA", 3),
  Quantile = c("Lower", "Median", "Upper"),
  X2020.5 = c(2, 2, 2),
  check.names = FALSE
)
data.table::fwrite(results, file.path(tmp, "Results.csv"))

dt_diff <- check_cqt_vs_results_csv(tmp, tmp)
stopifnot(nrow(dt_diff) == 2)
stopifnot(all(dt_diff$Quantile %in% c("Lower", "Upper")))
