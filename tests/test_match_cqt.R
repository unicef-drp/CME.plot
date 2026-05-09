if (requireNamespace("CME.plot", quietly = TRUE)) {
  match_cqt_core <- CME.plot:::match.cqt.core
  obtain_matched_cqt <- CME.plot::obtain.matched.cqt
} else {
  sys.source(file.path("R", "1.helper_plot_funcs.R"), envir = environment())
  match_cqt_core <- match.cqt.core
  obtain_matched_cqt <- obtain.matched.cqt
}

make_cqt <- function(iso, years, q = c(0.05, 0.5, 0.95)) {
  vals <- seq_len(length(iso) * length(q) * length(years))
  array(vals, dim = c(length(iso), length(q), length(years)),
        dimnames = list(iso, as.character(q), as.character(years)))
}

target_iso <- c("AAA", "BBB")
target_year <- c(2023.5, 2024.5, 2025.5)
comparison_cqt <- make_cqt(
  iso = c("BBB", "AAA", "CCC"),
  years = c(2024.5, 2025.5, 2026.5)
)

matched <- match_cqt_core(target_iso, target_year, comparison_cqt)

stopifnot(identical(dim(matched), c(2L, 3L, 3L)))
stopifnot(identical(dimnames(matched),
                    list(target_iso, c("0.05", "0.5", "0.95"), as.character(target_year))))
stopifnot(all(is.na(matched[, , "2023.5"])))
stopifnot(identical(
  matched["AAA", , "2024.5"],
  comparison_cqt["AAA", , "2024.5"]
))

median_only_cqt <- make_cqt(
  iso = c("BBB", "AAA", "CCC"),
  years = c(2024.5, 2025.5, 2026.5),
  q = 0.5
)
matched_median_only <- match_cqt_core(target_iso, target_year, median_only_cqt)
stopifnot(identical(dimnames(matched_median_only),
                    list(target_iso, c("0.05", "0.5", "0.95"), as.character(target_year))))
stopifnot(all(matched_median_only["AAA", , "2024.5"] ==
              median_only_cqt["AAA", "0.5", "2024.5"]))

tmp <- tempfile("cme-plot-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

output_dir1 <- file.path(tmp, "run1")
output_dir2 <- file.path(tmp, "run2")
dir.create(output_dir1)
dir.create(output_dir2)

iso.c <- target_iso
year.t <- target_year
res.cqt.Lw <- list("0.5" = make_cqt(target_iso, target_year))
save(iso.c, file = file.path(output_dir1, "iso.c.rda"))
save(year.t, file = file.path(output_dir1, "year.t.rda"))
save(res.cqt.Lw, file = file.path(output_dir1, "res.cqt.Lw.rda"))

iso.c <- c("BBB", "AAA", "CCC")
year.t <- c(2024.5, 2025.5, 2026.5)
res.cqt.Lw <- list("0.5" = comparison_cqt)
save(iso.c, file = file.path(output_dir2, "iso.c.rda"))
save(year.t, file = file.path(output_dir2, "year.t.rda"))
save(res.cqt.Lw, file = file.path(output_dir2, "res.cqt.Lw.rda"))

matched_from_dirs <- obtain_matched_cqt(
  output.dir1 = output_dir1,
  output.dir2 = output_dir2,
  pooling_weight = "0.5"
)

stopifnot(identical(matched_from_dirs, matched))
