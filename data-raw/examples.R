# `CME.plot` examples
devtools::load_all()

get.workdir(2023)
dir_IGME_out_folder <- file.path(get.workdir(2023), "output")
dir_IGME_out_folder_last <- file.path(get.workdir(2022), "output")


output.dir <- file.path(dir_IGME_out_folder, "GR20230530_all")
output.dir.for.comparison <- file.path(dir_IGME_out_folder_last, "GR20220516_all")

fig.dir <- file.path(get.workdir(2023), "fig")

dir.exists(output.dir.for.comparison)
cqt_last_year <- obtain.matched.cqt(output.dir1 = output.dir,
                                    output.dir2 = output.dir.for.comparison, pooling_weight = 0.5)


# review the entries' date in "data_CMEInfo.csv"
review.date.of.dataentry(output.dir = output.dir)

# the difference in VR is highlighted by comparing an new dataset to and old one
# by default the datasets used are:
new_dirs <- find.dir.for.VR.comparison(IGME_year_new = 2023, IGME_year_old = 2022,
                                       filename_new = NULL, filename_old = NULL)
# supply in the global environment to overwrite:
# if `dir_new_data_U5MR` is supplied, it will be used as the new file to compare VR
# used in function `get.diff.dt.WHOVR`
dir_new_data_U5MR <- new_dirs$dir_new_data_U5MR
dir_old_data_U5MR <- new_dirs$dir_old_data_U5MR

# minimum results/data plot:
year.lastestimatepublished = 2023
savePlotResults(runname = "UNIGME2023",
                     output.dir = output.dir,
                     legend1 = "UN IGME 2023", # if set as NULL, suppress estimates 1
                     fig.dir = fig.dir,
                     new_entry_date = "2022-12",
                     wpp.cqt = get.wpp.cqt("U5MR"),
                     ihme.cqt = get.gbd.cqt("U5MR"))

# add more comparison:
savePlotResults(runname = "GR20230530",
                output.dir = output.dir,
                legend1 = "UN IGME 2023",
                legend2 = "GBD 2021",
                res.cqt2 = get.gbd.cqt("U5MR", gbd_round = 2021),
                # col.CI4 = NULL,
                fig.dir = fig.dir,
                wpp.cqt = get.wpp.cqt("U5MR"),
                legend_IHME = "GBD 2019",
                ihme.cqt = u5mr.ihme.cqt.2019,
                new_entry_date = "2022-12"
                )


# using a specific iso order
savePlotResults(runname = "test",
                output.dir = output.dir,
                pdf.or.png = "pdf",
                fig.dir = fig.dir,
                iso.subset.c = c("CAF", "CIV", "AFG"),
                sort_the_isos = FALSE
)

# show wpp and ihme lines:
savePlotResults(runname = "GR20230530",
                     filename = "extra note",
                     output.dir = output.dir,
                     fig.dir = fig.dir,
                     wpp.cqt = get.wpp.cqt("U5MR"),
                     ihme.cqt = u5mr.ihme.cqt.2019,
                     n.countries = 1:10 # save only 10 countries
)

# show all lines (and make comparison to WPP and GBD):
savePlotResults(runname = "GR20230530",
                     new_entry_date = "2022-12",
                     output.dir = output.dir,
                     fig.dir = fig.dir,
                     legend1 = "UN IGME estimates 2023",
                     legend2 = "WPP 2024",
                     res.cqt2 = get.wpp.cqt("U5MR", wpp_round = 2024),
                     # legend4 = "GBD 2021",
                     # res.cqt4 = get.gbd.cqt("U5MR", gbd_round = 2021),
                     legend_WPP = "WPP 2022",
                     wpp.cqt = get.wpp.cqt("U5MR", wpp_round = 2022),
                     # legend_IHME = "IHME GBD 2019",
                     # ihme.cqt = u5mr.ihme.cqt.2019,
                     n.countries = NULL # save only 5 countries
)
