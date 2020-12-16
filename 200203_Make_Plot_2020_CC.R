# "Make CME 2020 Plots"
# incl plots for CC
# author: "Yang Liu"
# date: "2/3/2020 - ..."

library("CME.assistant")
library("CME.plot")

# how many countries to save, set `NULL` for all
n_c  <-  1:10 # for trial
png_or_pdf0 <- "pdf" # set for the whole markdown (md) document, pdf is much faster to create

dir_IGME_out_folder <- CME.assistant::get.IGMEoutput.dir(2020)
dir_IGME_fig_folder <- CME.assistant::get.IGMEfig.dir(2020)

runname_CC <- "GR20200214_all_CC"
runname <- "GR20200214_all"
runname_19 <- "IGME2019"

my_fig_dir <- "fig/"
# U5MR --------------------------------------------------------------------

# where the model "output" is stored
output.dir.CC <- file.path(dir_IGME_out_folder, runname_CC) # 2020 CC on dropbox
output.dir <- file.path(dir_IGME_out_folder, runname) # 2020 on dropbox
# output.dir <- file.path(getwd(), "output", runname) # 2020 local, used when we need to make a temp. adjustment using the dataset directly
output.dir.19 <- file.path(dir_IGME_out_folder, runname_19)

load(file.path(output.dir, "mcmc.meta.rda"))
load(file.path(output.dir, "res.cqt.Lw.rda"))
load(file.path(output.dir, "year.t.rda")) # -> year.t
res.cqt <- res.cqt.Lw[['0.5']]
# remove two VR points for Samoa
mcmc.meta$data.all$uvr.Lcs.j[[98]][[1]][4] <- NA
mcmc.meta$data.all$uvr.Lcs.j[[98]][[1]][5] <- NA
# output.dir.for.hivremoved.cqt <- file.path(dir_IGME_19, "output", "GR20190311_all_2019-05-07")
# load(file = file.path(output.dir.for.hivremoved.cqt, "res.hivremoved.cqt.Lw.rda"))
PlotDataAndEstimates2020(
  data = NULL,
  data.all = mcmc.meta$data.all,
  c = 57,
  zoom = TRUE,
  legendfull = "UN IGME Estimate")

p <- savePlotResults(runname = "test",
                     output.dir = output.dir,
                     pdf.or.png = "pdf",
                     fig.dir = "fig/",
                     n.countries = c(57)
)

# after CC compared to CC ----
p <- savePlotResults(runname = runname,
                     output.dir = output.dir,
                     output.dir2 = output.dir.CC,
                     legend1 = "UN IGME 2020 ",
                     legend2 = "UN IGME 2020 CC",
                     fig.dir = "fig/",
                     pdf.or.png = "pdf"
)
# 1.0. U5MR CC ----
p <- savePlotResults(runname = "U5MR_Total_CC",
                     output.dir = output.dir,
                     pdf.or.png = "pdf",
                     legend1 = "B3",
                     fig.dir = "fig/",
                     n.countries = NULL,
                     save_cqt_copy = TRUE
)

p <- savePlotResults(runname = "U5MR_Total_CC",
                     output.dir = output.dir,
                     pdf.or.png = "png",
                     legend1 = "B3",
                     fig.dir = "fig/",
                     n.countries = 98
)


# 1.1.U5MR-Results ------------------------------------------------------------
p <- savePlotResults(runname = runname,
                     new_entry_date = last.October(),
                     output.dir = output.dir,
                     pdf.or.png = png_or_pdf0,
                     fig.dir = my_fig_dir,
                     n.countries = n_c
)
p$time_spent

# 1.1.HIV-removed results for HIV countries -----------------------------------
# I did not find `res.hivremoved.cqt.Lw.rda` in "GR20190311_all" so I just used this for now.
p <- savePlotResults(runname = runname,
                     new_entry_date = last.October(),
                     output.dir = output.dir,
                     fig.dir = my_fig_dir,
                     HIV_removed = TRUE,
                     legend1 = "Draft UN IGME 2020 (HIV-removed)",
                     n.countries = n_c
)

# compared to 2019 (in this case, it is easier to supply res.cqt2 directly)
load(file.path(dir_IGME_out_folder, "IGME2019/res.hivremoved.cqt.Lw.rda"))
hivremoved.cqt.2019 <- res.hivremoved.cqt.Lw$`0.5`
p <- savePlotResults(runname = runname,
                     new_entry_date = last.October(),
                     output.dir = output.dir,
                     fig.dir = my_fig_dir,
                     HIV_removed = TRUE,
                     legend1 = "Draft UN IGME 2020 (HIV-removed)",
                     res.cqt2 = hivremoved.cqt.2019,
                     legend2 = "UN IGME 2019 (HIV-removed)",
                     n.countries = n_c
)


# 1.2.U5MR-Data ------------------------------------------------------------
p <- savePlotResults(runname = runname,
                     legend1 = NULL,
                     new_entry_date = last.October(),
                     output.dir = output.dir,
                     pdf.or.png = png_or_pdf0,
                     fig.dir = my_fig_dir,
                     n.countries = n_c
)


# 1.3.Compared to last year ----------------------------------------------
p <- savePlotResults(runname = runname,
                     new_entry_date = last.October(),
                     output.dir = output.dir,
                     output.dir2 = output.dir.19,
                     pdf.or.png = png_or_pdf0,
                     legend2 = "UN IGME 2019",
                     fig.dir = my_fig_dir,
                     n.countries = n_c
)

# 1.4. WPP IHME ---------------------------------------------------------
p <- savePlotResults(runname = runname,
                     new_entry_date = last.October(),
                     output.dir = output.dir,
                     pdf.or.png = png_or_pdf0,
                     # wpp only has estimates
                     wpp.cqt = u5mr.wpp.cqt.2019,
                     ihme.cqt = u5mr.ihme.cqt.2017,
                     fig.dir = "fig",
                     # col.ihme = "white" # if want to hide CI for IHME
                     n.countries = n_c
)

# 1.5 Add all together (4 series)  ----------------------------------------
p <- savePlotResults(runname = runname,
                     filename = "GMB",
                     new_entry_date = last.October(),
                     res.cqt2 = cqt_last_year,
                     legend2 = "UN IGME 2019",
                     output.dir = output.dir,
                     pdf.or.png = png_or_pdf0,
                     wpp.cqt = u5mr.wpp.cqt.2019,
                     ihme.cqt = u5mr.ihme.cqt.2017,
                     fig.dir = my_fig_dir,
                     # col.ihme = "white" # if want to hide CI for IHME
                     n.countries = n_c,
                     iso.subset.c = "GMB"
)


# IMR ---------------------------------------------------------------------
# Notice: update both runname and output.dir!! (compared to U5MR)
runname_IMR <- "IMR20200219_all"
runname_IMR_19 <- "IMR20190314_all"
output.dir.IMR <- file.path(dir_IGME_out_folder, runname_IMR) # 2020 on dropbox
output.dir.IMR.19 <- file.path(dir_IGME_out_folder, runname_IMR_19) # 2019 on dropbox

load(file.path(output.dir.IMR, "mcmc.meta.rda"))
mcmc.meta$data.all$imrmethod.c
dtmethodimr <- data.table(ISO.Code = mcmc.meta$data.all$iso.c, Method.IMR = mcmc.meta$data.all$imrmethod.c)
# fwrite(dtmethodimr, "ModelMethod/dtmethodimr.csv")

PlotDataAndEstimates2020(
  data = NULL,
  data.all = mcmc.meta$data.all,
  plot.se = F,
  c = 57,
  zoom = TRUE,
  legendfull = "UN IGME Estimate")


# 5/22 remove two points for Samoa (WSM) VR
mcmc.meta$data.all$name.c[98]
# remove two VR points for Samoa
mcmc.meta$data.all$uvr.Lcs.j[[98]][[2]][4] <- NA
mcmc.meta$data.all$uvr.Lcs.j[[98]][[2]][5] <- NA
# 2.0. IMR CC ----
p <- savePlotResults(runname = "IMR_Total_CC",
                     output.dir = output.dir.IMR,
                     pdf.or.png = "pdf",
                     legend1 = mcmc.meta$data.all$imrmethod.c,
                     fig.dir = "fig/",
                     n.countries = NULL,
                     save_cqt_copy = TRUE
)
p <- savePlotResults(runname = "IMR_Total_CC",
                     output.dir = output.dir.IMR,
                     pdf.or.png = "png",
                     legend1 = mcmc.meta$data.all$imrmethod.c,
                     fig.dir = "fig/",
                     n.countries = 98
)


# 1. Results
p <- savePlotResults(runname = runname_IMR,
                     new_entry_date = last.October(),
                     output.dir = output.dir.IMR,
                     pdf.or.png = png_or_pdf0,
                     fig.dir = my_fig_dir,
                     n.countries = n_c
)


# 2.1 HIV-removed -------------------------------------------------------------
p <- savePlotResults(runname = runname_IMR,
                     new_entry_date = last.October(),
                     output.dir = output.dir.IMR,
                     fig.dir = my_fig_dir,
                     HIV_removed = TRUE,
                     legend1 = "Draft UN IGME 2020 (HIV-removed)",
                     n.countries = n_c
)
# compared to 2019
load(file.path(output.dir.IMR.19, "res.hivremoved.cqt.Lw.rda"))
hivremoved.cqt.imr2019 <- res.hivremoved.cqt.Lw$`0.5`
p <- savePlotResults(runname = runname_IMR,
                     new_entry_date = last.October(),
                     output.dir = output.dir.IMR,
                     fig.dir = my_fig_dir,
                     HIV_removed = TRUE,
                     legend1 = "Draft UN IGME 2020 (HIV-removed)",
                     legend2 = "UN IGME 2019 (HIV-removed)",
                     res.cqt2 = hivremoved.cqt.imr2019,
                     n.countries = n_c
)


# 2. Data
p <- savePlotResults(runname = runname_IMR,
                     legend1 = NULL,
                     new_entry_date = last.October(),
                     output.dir = output.dir.IMR,
                     pdf.or.png = png_or_pdf0,
                     fig.dir = my_fig_dir,
                     n.countries = n_c
)

# 3. Compared to last year
p <- savePlotResults(runname = runname_IMR,
                     new_entry_date = last.October(),
                     output.dir = output.dir.IMR,
                     output.dir2 = output.dir.IMR.19,
                     pdf.or.png = png_or_pdf0,
                     legend2 = "UN IGME 2019",
                     fig.dir = my_fig_dir,
                     n.countries = n_c
                     # col.CI3 = "white"
                     # if don't want to show SE for the comparison series ,set `col.CI`# "white"
)

# 4. WPP IHME
# wpp_and_completeihme <-
#   list(wpp.cqt = get.wpp.cqt(ind_name = "IMR"),
#        ihme.cqt = get.sex.ihme.cqt(ind_name = "IMR",
#                                    gender0 = "both",
#                                    iso_order = u5mr.iso.c))

p <- savePlotResults(runname = runname_IMR,
                     new_entry_date = last.October(),
                     output.dir = output.dir.IMR,
                     pdf.or.png = png_or_pdf0,
                     wpp.cqt = imr.wpp.cqt.2019,
                     ihme.cqt = imr.ihme.cqt.2017,
                     # legend_WPP = NULL,
                     # legend_IHME = NULL,
                     fig.dir = my_fig_dir,
                     n.countries = n_c,
)

# 5 Add all together (4 series)
p <- savePlotResults(runname = runname_IMR,
                     new_entry_date = last.October(),
                     res.cqt2 = cqt_last_year,
                     legend2 = "UN IGME 2019",
                     output.dir = output.dir.IMR,
                     pdf.or.png = png_or_pdf0,
                     # wpp only has estimates
                     wpp.cqt = imr.wpp.cqt.2019,
                     ihme.cqt = imr.ihme.cqt.2017,
                     fig.dir = my_fig_dir,
                     # col.ihme = "white" # if want to hide CI for IHME
                     n.countries = n_c
)



# NMR ---------------------------------------------------------------------
workdir <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/NMR/")

filerate <- CME.assistant::get.dir_NMR(y5 = TRUE)
fileratio <- CME.assistant::get.dir_NMR(y5 = FALSE)

# u5median.crisisandhivfree.rda required by `GetDataU5MRNMR`
u5median.crisisandhivfree_file <- file.path(workdir, "data/u5median.crisisandhivfree.rda")
# crisis adjusts
crisisadjfile <- file.path(workdir, "data/reference/dataPostAdj_U5MR.csv")


# Results files -----------------------------------------------------------
# need 4 files from output/final
# NMR results
resultsfile = file.path(workdir, "output/final/finalresults.jtc.Rda")
expectedresultsfile = file.path(workdir, "output/final/finalresults_expected.jtc.Rda")
# ratio results
resultsfile_ratio =file.path(workdir, "output/final/finalresults_ratio.jtc.Rda")
expectedresultsfile_ratio = file.path(workdir, "output/final/finalresults_expectedratio.jtc.Rda")

# results 2019
resultsfile_2019 = file.path(workdir, "output/2019/final2019/finalresults.jtc.Rda")
resultsfile_ratio_2019 =file.path(workdir, "output/2019/final2019/finalresults_ratio.jtc.Rda")

# New entries marked
review.date.of.dataentry(dir_file = filerate)
# by default w/o results file, ISO follows d$iso.code: "AFG" "AGO" "ALB" "AND" ...
# d <- GetDataGlobalNMR(file = filerate, include_excluded = TRUE,
#                       crisisadjfile = crisisadjfile,  new_entry_date = last.October())

#' get resl file
#'
#' @param scale0 "NMR" or "ratio"
get.nmr.resl <- function(scale0){
  transformdataforNMR(
    file = filerate,
    resultsfile = if(scale0=="NMR") resultsfile else resultsfile_ratio,
    resultsfile2 = if(scale0=="NMR") resultsfile_2019 else resultsfile_ratio_2019,
    resultsfile3 = NULL,
    new.cname.df = new_cnames,
    expectedresultsfile = if(scale0=="NMR") expectedresultsfile else expectedresultsfile_ratio,
    igmefile = NULL,
    crisisadjfile = crisisadjfile,
    scale = scale0
  )
}
# output_dir_nmr <- file.path(workdir, "figData")
output_dir_nmr <- "figData"
# just run once and save, also if NMR results files have been updated, run again
resl_rate <- get.nmr.resl("NMR")
resl_ratio <- get.nmr.resl("ratio")
saveRDS(resl_rate, file.path(output_dir_nmr, "mcmc.meta.nmr.rda"))
saveRDS(resl_ratio, file.path(output_dir_nmr, "mcmc.meta.nmr.ratio.rda"))

# p <- savePlotResults(runname = "test",
#                      NMR_metafile = resl_rate,
#                      pdf.or.png = "pdf",
#                      legend1 = "BHSR",
#                      fig.dir = "fig/",
#                      n.countries = 74
# )
# 3.0. NMR CC ----
p <- savePlotResults(runname = "NMR_Total_CC",
                     NMR_metafile = resl_rate,
                     pdf.or.png = "pdf",
                     legend1 = "BHSR",
                     fig.dir = "fig/",
                     n.countries = NULL,
                     save_cqt_copy = TRUE
)
p <- savePlotResults(runname = "NMR_Total_CC",
                     NMR_metafile = resl_rate,
                     pdf.or.png = "png",
                     legend1 = "BHSR",
                     fig.dir = "fig/",
                     n.countries = NULL
)

# 3.1 Rate and Ratio Results ----------------------------------------------
# The results are saved in `my_fig_dir`
# dir_IGME_fig_folder <- file.path(workdir, "fig")
# assign.my.fig.dir(subfolder = runname.NMR)
runname.NMR <- "NMR_20200430" #
# my_fig_dir <- file.path(workdir, "fig/NMR_20200430")

p <- savePlotResults(runname = runname.NMR,
                     pdf.or.png = png_or_pdf0,
                     NMR_metafile = resl_rate,
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)

p <- savePlotResults(runname = runname.NMR,
                     pdf.or.png = png_or_pdf0,
                     NMR_metafile = resl_ratio,
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)


# 3.2 Compared to expected  -------------------------------------------
p <- savePlotResults(runname = runname.NMR,
                     output.dir = output_dir_nmr,
                     legend_ex = "Expected", # turn on the expected series
                     pdf.or.png = png_or_pdf0,
                     NMR_scale = "NMR",
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)
p <- savePlotResults(runname = runname.NMR,
                     output.dir = output_dir_nmr,
                     legend_ex = "Expected",
                     pdf.or.png = png_or_pdf0,
                     NMR_scale = "ratio",
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)

# 3.2 compared to last year --------------------------------------------------
p <- savePlotResults(runname = runname.NMR,
                     output.dir = output_dir_nmr,
                     legend2 = "UN IGME 2019",
                     pdf.or.png = png_or_pdf0,
                     NMR_scale = "NMR",
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)


p <- savePlotResults(runname = runname.NMR,
                     output.dir = output_dir_nmr,
                     legend2 = "UN IGME 2019",
                     pdf.or.png = png_or_pdf0,
                     NMR_scale = "ratio",
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)


# 3.3 Compared to IHME --------------------------------------------------------
p <- savePlotResults(runname = runname.NMR,
                     output.dir = output_dir_nmr,
                     ihme.cqt = nmr.ihme.cqt.2017,
                     pdf.or.png = png_or_pdf0,
                     NMR_scale = "NMR",
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)

# all three series together
p <- savePlotResults(runname = runname.NMR,
                     output.dir = output_dir_nmr,
                     legend2 = "UN IGME 2019",
                     ihme.cqt = nmr.ihme.cqt.2017,
                     pdf.or.png = png_or_pdf0,
                     NMR_scale = "NMR",
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)

# all four series + expected
p <- savePlotResults(runname = runname.NMR,
                     output.dir = output_dir_nmr,
                     legend2 = "UN IGME 2019",
                     legend_ex = "Expected",
                     ihme.cqt = nmr.ihme.cqt.2017,
                     pdf.or.png = png_or_pdf0,
                     NMR_scale = "NMR",
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)

# Ratio:
p <- savePlotResults(runname = runname.NMR,
                     output.dir = output_dir_nmr,
                     legend2 = "UN IGME 2019",
                     legend_ex = "Expected",
                     pdf.or.png = png_or_pdf0,
                     NMR_scale = "ratio",
                     fig.dir = my_fig_dir,
                     new_entry_date = last.October()
)


# Sex-specific ---------------------------------------------------------
workdir <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/CMEgender2015")
dir_sex_specific_fig <- file.path(workdir, "fig/") # save place on Dropbox
# dataset for plotting
# filerate <- file.path(workdir, "Database/dataset_forplotting_2020-05-18.csv")
filerate <- CME.assistant::get.dir_gender(plotting = TRUE)
dt_gender <- fread(filerate)
dt_gender <- fix.entries.dt_gender(dt_gender)

# New entries marked
review.date.of.dataentry(dir_file = filerate)
# add crisisadjfile to exclude some data points like Japan 2011 and 2015
crisisadjfile <- file.path(workdir, "data/crisis mortality/dataPostAdj_U5MR.csv")
# model life table information to be used in IMR VR inclusion criteria
mlinfo <- file.path(workdir, "data/input/MLinfo.csv")


# results files that provide cqt  (suppose we have two here)
# resultsfile1 <- file.path(workdir, "data/output/M49/cis_M49_full.rda") # updated to:
resultsfile1 <- file.path(workdir, "data/output/M49_one/cis_M49_one_full.rda")
resultsfile_2019 <- file.path(workdir, "data/output/2019/M49_one/cis_M49_one_full.rda")
if(!file.exists(crisisadjfile)|!file.exists(mlinfo)) message("check if filerate and fileratio exists")
if(!file.exists(resultsfile1)|!file.exists(resultsfile_2019)) message("check if filerate and fileratio exists")

load(resultsfile1) # loads the list `res.full` to get `isos_results`
isos_results <- res.full$iso.c # same as sexspecific.iso.c

message("These expected files exist in resultsfile1: ",
        paste0(paste0("exp", get.gender.cqt.grid()$name)[paste0("exp", get.gender.cqt.grid()$name)%in%grep("exp", names(res.full), value = TRUE)], collapse = ", ")
)
# expQ5f.cqt, expQ1f.cqt, expQ4f.cqt

# for example:
# cqt0 <- get.gender.cqt("Q5", "f", TRUE)
# cqt0 <- get.gender.cqt("Q5", "ratio", resultsfile = resultsfile_2019)
# used to match indicator, and "Sex" or "Sex Ratio" (used in title)
indicator_group <- c("Child Mortality Rate", "Infant Mortality Rate",
                     "Under-five Mortality Rate")
ind0_name_list <- list(
  "Q5" = indicator_group[3],
  "Q1" = indicator_group[2],
  "Q4" = indicator_group[1]
)
# for the label used in title
title_name_list <- list(
  "f"  = "Female",
  "m" = "Male",
  "ratio" = "Sex Ratio"
)
# for ylab
ylab_name_list <- list(
  "Q5" = "U5MR",
  "Q1" = "IMR",
  "Q4" = "CMR"
)
# get.match("f", new_list = title_name_list)
output_dir_gender <- "figData"

# produce a grid of ind x gender
cqt_files <- get.gender.cqt.grid()
get.resl <- function(ind0, gender0){
  resl1 <- transformdataSexSpecific(
    dt_gender = dt_gender,
    new_entry_date = last.October(),
    indicator = get.match(ind0, new_list = ind0_name_list),
    indicator_label = get.match(ind0, new_list = ylab_name_list),
    sex = get.match(gender0, new_list = title_name_list),
    iso = isos_results,
    # to supply the following series:
    # the cqts from resultsfile1
    resultsfile_cqt = get.gender.cqt(ind0, gender0, resultsfile = resultsfile1),
    # the expected cqts from resultsfile1
    expectedresultsfile_cqt =  get.gender.cqt(ind0, gender0, get_expected = TRUE, resultsfile = resultsfile1),
    # the cqts from resultsfile_2019
    resultsfile_cqt2 = get.gender.cqt(ind0, gender0, resultsfile = resultsfile_2019),
    new.cname.df = new_cnames,
    resultsiso = isos_results,
    mlinfo = mlinfo,
    crisisadjfile = crisisadjfile)
  # return(resl1)
  saveRDS(resl1, file.path(output_dir_gender,
                           paste0("mcmc.meta.gender.",ind0,".", gender0,".rda")))
}
get.resl("Q5", "f")
# To create all output resls:
invisible(Map(get.resl, ind0 = cqt_files$ind, gender0 = cqt_files$gender))
# to preview a mcmc.meta created:
# resl0 <- readRDS(file.path(output_dir_gender,
#                            paste0("mcmc.meta.gender.","Q5",".", "f",".rda")))
# resl0$res.cqt[67,,]
# resl0$data$source.Lc.s[[1]]
# resl0$data$sourcetype.Lc.s[[1]]
# resl0$data$method.Lc.s[[1]]
# resl0$data$newentry.Lc.s[[1]]
# resl0$new_entry_date
#
# mcmc.meta <- list()
# mcmc.meta$data.all <- resl0$data
# mcmc.meta$data.all$C <- resl0$data$c
# mcmc.meta$data.all$iso.c <- resl0$iso
# PlotDataAndEstimates2020(
#   data = NULL,
#   data.all = mcmc.meta$data.all,
#   CIs.cqt = resl0$res.cqt,
#   plot.se = F,
#   c = 67,
#   legendfull = "UN IGME Estimate")
# test
p = savePlotResults(
  filename = "test",
  output.dir = output_dir_gender,
  pdf.or.png = png_or_pdf0,
  fig.dir = file.path("fig"),
  # new_entry_date = last.October(),
  gender_ind = "Q5", # takes either U5MR or Q5, case-insensitive
  gender = 'f', # takes f or m
  iso.subset.c = "IND"
)
# also be careful with MCO # MRT

# We have saved in `output_dir_gender` 9 mcmc.meta files corresponding to `resultsfile1`:
list.files(output_dir_gender)


# 4.0 Sex-specific CC ----

#` save.all helps to save all the combinations provided
save.all <- function(ind0, gender0, pdf_or_png){
  p <- savePlotResults(
    filename = "CC_Results",
    output.dir = output_dir_gender,
    pdf.or.png = pdf_or_png,
    legend1 = "B3SR",
    fig.dir = "fig/",
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    gender = gender0, # takes f or m
    # n.countries = NULL,
    # iso.subset.c = "LUX",
    save_cqt_copy = TRUE
  )
  return(p)
}
p <- save.all("Q5", "f", "pdf")
p <- save.all("Q5", "m", "pdf")
p <- save.all("Q1", "f", "pdf")
p <- save.all("Q1", "m", "pdf")

p <- save.all("Q5", "f", "png")
p <- save.all("Q5", "m", "png")
p <- save.all("Q1", "f", "png")
p <- save.all("Q1", "m", "png")


# 4.1 Sex-specific-Results --------------------------------------------------
my_fig_dir <- file.path("fig/Sex-specific")
#` save.all helps to save all the combinations provided
save.all <- function(ind0, gender0){
  p <- savePlotResults(
    # filename = "test",
    output.dir = output_dir_gender,
    pdf.or.png = png_or_pdf0,
    fig.dir = my_fig_dir,
    new_entry_date = last.October(),
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    gender = gender0, # takes f or m
    n.countries = NULL
  )
  return(p)
}
# # e.g.: for testing
p <- save.all("Q5", "f")
p_list <- invisible(Map(save.all, ind0 = cqt_files$ind, gender0 = cqt_files$gender))


# 4.2 compared to expected (only exist for gender = `f`) ----------------------
save.all <- function(ind0){
  p <- savePlotResults(
    # runname = "vs_Expected",
    legend_ex = "Expected",
    pdf.or.png = png_or_pdf0,
    output.dir = output_dir_gender,
    fig.dir = my_fig_dir,
    new_entry_date = last.October(),
    gender = "f", #
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    n.countries = NULL
  )
  return(p)
}
# # e.g.:
p <- save.all("Q5")
p_list <- invisible(lapply(c("Q5", "Q4", "Q1"), save.all))



# 4.3 Compared to previous year   --------------------------------------
save.all <- function(ind0, gender0){
  p <- savePlotResults(
    # runname = "vs_UN IGME 2019",
    legend2 = "UN IGME 2019",
    pdf.or.png = png_or_pdf0,
    output.dir = output_dir_gender,
    fig.dir = file.path(my_fig_dir, "Compared to UN IGME 2019"),
    new_entry_date = last.October(),
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    gender = gender0, #
    n.countries = n_c
  )
  return(p)
}
# # e.g.:
p_list <- invisible(Map(save.all, ind0 = cqt_files$ind, gender0 = cqt_files$gender))


# 4.4 Compared to IHME and WPP   ---------------------------------------
dir_wpp_female <- file.path("data-raw/WPP2019", "WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx")
dir_wpp_male <- file.path("data-raw/WPP2019", "WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx")
dir_IHME_sex <- file.path("data-raw/IHME2018", "IHME_ProbabilityOfDeath_estimates_200130.csv")
wpp_f <- read.wpp.v3(dir_wpp_female)
wpp_m <- read.wpp.v3(dir_wpp_male)
#
save.all <- function(ind0, gender0){
  p <- savePlotResults(
    output.dir = output_dir_gender,
    pdf.or.png = png_or_pdf0,
    fig.dir = file.path(my_fig_dir, "Compared to IHME and WPP and 2019"),
    wpp.cqt = get.sex.wpp.cqt(ind0, gender0),
    ihme.cqt = get.sex.ihme.cqt(ind_name = ind0, gender0 = gender0),
    new_entry_date = last.October(),
    gender = gender0, #
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    n.countries = n_c)
  return(p)
}
p <- save.all("Q5", "f")
p_list <- invisible(Map(save.all, ind0 = c("Q5", "Q5", "Q1", "Q1"), gender0 = c("f", "m","f", "m")))

save.all <- function(ind0, gender0){
  p <- savePlotResults(
    legend2 = "UN IGME 2019",
    output.dir = output_dir_gender,
    pdf.or.png = png_or_pdf0,
    fig.dir = file.path(my_fig_dir, "Compared to IHME and WPP and 2019"),
    wpp.cqt = get.sex.wpp.cqt(ind0, gender0),
    ihme.cqt = get.sex.ihme.cqt(ind_name = ind0, gender0 = gender0),
    new_entry_date = last.October(),
    gender = gender0, #
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    n.countries = n_c)
  return(p)
}
p_list <- invisible(Map(save.all, ind0 = c("Q5", "Q5", "Q1", "Q1"), gender0 = c("f", "m","f", "m")))



# Female U5MR and IMR comparison plots with the expected rate (dashed line), Draft UN IGME 2020, UN IGME 2019 and GBD IGME 2018

save.all <- function(ind0, gender0){
  p <- savePlotResults(
    legend_ex = "Expected",
    legend2 = "UN IGME 2019",
    output.dir = output_dir_gender,
    pdf.or.png = png_or_pdf0,
    fig.dir = file.path(my_fig_dir, "Compared to Expected and 2019 and IHME and WPP"),
    wpp.cqt = get.sex.wpp.cqt(ind0, gender0),
    ihme.cqt = get.sex.ihme.cqt(ind_name = ind0, gender0 = gender0),
    new_entry_date = last.October(),
    gender = gender0, #
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    n.countries = n_c)
  return(p)
}
p_list <- invisible(Map(save.all, ind0 = c("Q5", "Q1"), gender0 = c("f", "f")))



# 5. 5-14 ----
output.dir.19 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2019 Round Estimation/output")
# for 10q5 country consultation
output.dir.20 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2020 Round Estimation/Country consultation/")

runname5_14_2019 <- "10q5-IGME20190114_all"
runname5_14 <- "MR5-14_Total_CC"

#
# get.cqt.from.results(output_dir = "output/10q5-Plot0429_CC/")
# check.cqt.vs.results.csv(dir_res.cqt_file = "output/10q5-Plot0429_CC/",
#                          dir_results.csv_file = "output/10q5-Plot0429_CC/", alpha_weight = "0.5")

# remove derived from U5 series
# load(file.path(output.dir.19, runname5_14, "mcmc.meta.rda"))
# making these plots requires modifying and saving the mcmc.meta.rda in a local `output.dir`
# the mcmc.meta.rda file should be a local copy:
load("../2020_CME_Plots/output/10q5-Plot0429_CC/mcmc.meta.0429.rda")
special_legend <- get.special.isos(mcmc.meta)$special_legend_vector
special_isos <- get.special.isos(mcmc.meta)$special_isos
# saveRDS(special_isos, "figData/5-14/special_legend_isos_2020")
# saveRDS(special_legend, "figData/5-14/special_legend_2020")

# note: remove subnational series for Somalia
mcmc.meta <- remove.specific.series(mcmc.meta, remove_pattern = "Derived from 5q0|Subnational")
data.all <- mcmc.meta$data.all
save(mcmc.meta, file = "output/10q5-Plot0429_CC/mcmc.meta.rda")

PlotDataAndEstimates2020(
  data = NULL,
  data.all = mcmc.meta$data.all,
  plot.se = T,
  c = 2,
  year.start = 1990,
  legendfull = "UN IGME Estimate")

# Mark special legend

# find 2019 series
load(file.path(output.dir.19, runname5_14_2019, "res.cqt.Lw.rda"))
res.cqt.19 <- res.cqt.Lw[["0.5"]]

load(file.path("output/10q5-Plot0429_CC/res.cqt.Lw.rda"))
res.cqt.20 <- res.cqt.Lw[["0.5"]]

iso.c1 <- mcmc.meta$data$iso.c # 2020
year.t1 <- dimnames(res.cqt.20)[[3]] # 2020
iso.c2 <- dimnames(res.cqt.19)[[1]]
year.t2 <- dimnames(res.cqt.19)[[3]]
resfinal.cqt.19 <- array(NA, c(length(iso.c1), 3, length(year.t1)))
resfinal.cqt.19[, , is.element(year.t1, year.t2)] <-
  res.cqt.19[match(iso.c1, iso.c2), , is.element(year.t2, year.t1)]
dimnames(resfinal.cqt.19)[[3]] <- year.t1


# fig_dir_CC <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/UN IGME data/2020 Round Estimation/Consultation profiles/Results2020/Plots")

# 1.1.-Results
p <- savePlotResults(runname = runname5_14,
                     output.dir = "output/10q5-Plot0429_CC/",
                     year.start = 1990,
                     # legend1 = "2020 Country Consultation",
                     legend1 = special_legend,
                     # res.cqt2 = resfinal.cqt.19,
                     # legend2 = "2019_IGME20190114_all",
                     ylab = "10q5",
                     pdf.or.png = "pdf",
                     fig.dir = "fig",
                     n.countries = NULL,
                     save_cqt_copy = TRUE
)
p$time_spent
p <- savePlotResults(runname = runname5_14,
                     output.dir = "output/10q5-Plot0429_CC/",
                     year.start = 1990,
                     # legend1 = "2020 Country Consultation",
                     legend1 = special_legend,
                     # res.cqt2 = resfinal.cqt.19,
                     # legend2 = "2019_IGME20190114_all",
                     ylab = "10q5",
                     pdf.or.png = "png",
                     fig.dir = "fig",
                     n.countries = NULL
)

# 6. 15-24 ----
# runname15_24_2019 <- "10q5-IGME20190114_all"
runname15_24 <- "MR15-24_Total_CC"
output.dir15_24.20 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/Estimates 10q15/Country consultation/")


# remove derived from U5 series
# load(file.path(output.dir.19, runname5_14, "mcmc.meta.rda"))
get.cqt.from.results(output_dir = "output/10q15-Plot0429_CC/")
check.cqt.vs.results.csv(dir_res.cqt_file = output.dir15_24.20,
                         dir_results.csv_file = output.dir15_24.20, alpha_weight = "0.8")
check.cqt.vs.results.csv(dir_res.cqt_file = "output/10q15-Plot0429_CC/",
                         dir_results.csv_file = output.dir15_24.20, alpha_weight = "0.5")

load("output/10q15-Plot0429_CC/mcmc.meta.0429.rda")
load("output/10q15-Plot0429_CC/res.cqt.Lw.rda")
res.cqt.20 <- res.cqt.Lw[["0.5"]]
special_legend <- get.special.isos(mcmc.meta)$special_legend_vector
special_isos <- get.special.isos(mcmc.meta)$special_isos
# saveRDS(special_isos, "figData/15-24/special_legend_isos_2020")
# saveRDS(special_legend, "figData/15-24/special_legend_2020")
mcmc.meta <- remove.specific.series.15_24(mcmc.meta, remove_pattern = "Derived from 5q0")
mcmc.meta <- remove.specific.series(mcmc.meta, remove_date = 1990)
data.all <- mcmc.meta$data.all
save(mcmc.meta, file = "output/10q15-Plot0429_CC/mcmc.meta.rda")

PlotDataAndEstimates2020(
  data = NULL,
  data.all = mcmc.meta$data.all,
  plot.se = F,
  c = 126,
  # year.start = 1990,
  legendfull = "UN IGME Estimate")

# 1.1.-Results
p <- savePlotResults(runname = runname15_24,
                     # new_entry_date = last.October(),
                     output.dir = "output/10q15-Plot0429_CC/",
                     year.start = 1990,
                     legend1 = special_legend,
                     # res.cqt2 = resfinal.cqt.19,
                     # legend2 = "2019_IGME20190114_all",
                     ylab = "10q15",
                     pdf.or.png = "pdf",
                     fig.dir = "fig",
                     n.countries = NULL,
                     save_cqt_copy = TRUE
)
p$time_spent

p <- savePlotResults(runname = runname15_24,
                     # new_entry_date = last.October(),
                     output.dir = "output/10q15-Plot0429_CC/",
                     year.start = 1990,
                     legend1 = special_legend,
                     # res.cqt2 = resfinal.cqt.19,
                     # legend2 = "2019_IGME20190114_all",
                     ylab = "10q15",
                     pdf.or.png = "png",
                     fig.dir = "fig",
                     n.countries = NULL
)
