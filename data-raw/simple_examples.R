# `CME.plot` simple examples
library(CME.plot)
dir_IGME_out_folder <- get.IGMEoutput.dir(2020)
runname <- "GR20200214_all"
runname_19 <- "IGME2019"
output.dir <- file.path(dir_IGME_out_folder, runname) # 2020 on dropbox
output.dir.19 <- file.path(dir_IGME_out_folder, runname_19)

cqt_last_year <- obtain.matched.cqt(output.dir1 = output.dir,
                                 output.dir2 = output.dir.19)

cqt3 <- cqt_last_year*0.7
cqt4 <- cqt_last_year*1.2
cqt5 <- cqt_last_year*1.5

# review the entries' date in "data_CMEInfo.csv"
review.date.of.dataentry(output.dir = output.dir)

# the difference in VR is highlighted by comparing an new dataset to and old one
# by default the datasets used are:
new_dirs <- find.dir.for.VR.comparison(IGME_year_new = 2021, IGME_year_old = 2020,
                                       filename_new = NULL, filename_old = NULL)
# supply in the global environment to overwrite:
# if `dir_new_data_U5MR` is supplied, it will be used as the new file to compare VR
# used in function `get.diff.dt.WHOVR`
dir_new_data_U5MR <- new_dirs$dir_new_data_U5MR
dir_old_data_U5MR <- new_dirs$dir_old_data_U5MR

# minimum results/data plot:
savePlotResults(runname = "GR20200214",
                     output.dir = output.dir,
                     legend1 = NULL,
                     new_entry_date = "2020-01",
                     n.countries = 1:10)

# sort iso order
savePlotResults(runname = "test",
                output.dir = output.dir,
                pdf.or.png = "pdf",
                fig.dir = "fig2020",
                iso.subset.c = c("CAF", "CIV", "AFG"),
                sort_the_isos = FALSE
)

# minimum comparison plot:
# supply either runname or legend
savePlotResults(runname = "GR20200214",
                     legend2 = "UN IGME 2019_2",
                     legend3 = "UN IGME 2019_3",
                     legend4 = "UN IGME 2019_4",
                     output.dir = output.dir,
                     output.dir2 = output.dir.19,
                     output.dir3 = output.dir.19,
                     output.dir4 = output.dir.19,
                     n.countries = 1:5)

savePlotResults(runname = "GR20200214",
                     runname2 = "UN IGME 2019_2",
                     runname3 = "UN IGME 2019_3",
                     runname4 = "UN IGME 2019_4",
                     output.dir = output.dir,
                     output.dir2 = output.dir.19,
                     output.dir3 = output.dir.19,
                     output.dir4 = output.dir.19,
                     n.countries = 1:5)

# show wpp and ihme lines:
savePlotResults(runname = "GR20200214",
                     filename = "mytest",
                     output.dir = output.dir,
                     fig.dir = "fig",
                     wpp.cqt = u5mr.wpp.cqt.2019,
                     ihme.cqt = u5mr.ihme.cqt.2019,
                     n.countries = 1:10 # save only 5 countries
)

# show all lines:
savePlotResults(runname = "GR20200214",
                     filename = "mytest",
                     # new_entry_date = "2020-01",
                     output.dir = output.dir,
                     fig.dir = "fig",
                     zoom.year.end = 2020,
                     legend2 = "exp2",
                     res.cqt2 = cqt_last_year,
                     legend3 = "exp3",
                     res.cqt3 = cqt3,
                     legend4 = "exp4",
                     res.cqt4 = cqt4,
                     legend_ex = "Expected",
                     res_ex.cqt = cqt5,
                     wpp.cqt = u5mr.wpp.cqt.2019,
                     ihme.cqt = u5mr.ihme.cqt.2017,
                     n.countries = 1:5 # save only 5 countries
)
