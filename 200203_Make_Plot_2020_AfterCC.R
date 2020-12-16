# Make CME 2020 Plots - After CC
# Compare final to CC
# author: "Yang Liu"
# date: "2/3/2020 - 8/31/2020"

library("CME.assistant")
library("CME.plot")

# save to dropbox Code/fig or not
dir_IGME_out_folder <- CME.assistant::get.IGMEoutput.dir(2020)
dir_IGME_fig_folder <- CME.assistant::get.IGMEfig.dir(2020)

runname_CC <- "GR20200214_all_CC"    # Pre_CC
runname <- "GR20200214_all"    # post CC

png_or_pdf0 <- "pdf"
iso.subset.c0 <- NULL

# 1.U5MR --------------------------------------------------------------------

# where the model "output" is stored
output.dir.CC <- file.path(dir_IGME_out_folder, runname_CC) # 2020 CC on dropbox
output.dir <-    file.path(dir_IGME_out_folder, runname) # 2020 Final on dropbox
# manually made:
load(file.path(output.dir, "res.cqt.Lw.new.rda"))
res.cqt.AggFinal <- res.cqt.Lw.new[['0.5']]
load(file.path(output.dir, "mcmc.meta.rda")) # to output model method for all ind
dtmethodu5mr <- data.table(ISO.Code = mcmc.meta$data.all$iso.c,
                           Method.U5MR = mcmc.meta$data.all$method.c,
                           Method.IMR = mcmc.meta$data.all$imrmethod.c,
                           Method.NMR = "BNUR",
                           Method.5_14 = "B3", Method.5_9 = "B3",
                           Method.15_24 = "B3", Method.15_19 = "B3") # I will revise value later and output
dtmethodu5mr[Method.IMR=="Sahel", Method.IMR:= "Sahel equation"]
# # testing data plot
# load(file.path(output.dir, "mcmc.meta.rda"))
# # note: don't use resall.cqt.Lw.rda
# load(file.path(output.dir, "res.cqt.Lw.rda"))
# load(file.path(output.dir, "year.t.rda")) # -> year.t
# res.cqt <- res.cqt.Lw[['0.5']]
# PlotDataAndEstimates2020(
#                      data = NULL,
#                      data.all = mcmc.meta$data.all,
#                      plot.se = F,
#                      c = 188,
#                      legendfull = NULL)

# save plots for IGME report method part
# nc_u5 <- which(mcmc.meta$data.all$name.c %in% c("Nigeria", "Senegal"))
# ylab0 = "Under-five mortality rate (deaths per 1,000 live births)"
# p <- savePlotResults(runname = "IGME_Report",
#                      legend1 = NULL,
#                      output.dir = output.dir,
#                      pdf.or.png = "png",
#                      zoom = F,
#                      add.legend = F,
#                      ylab = ylab0,
#                      fig.dir = my_fig_dir,
#                      n.countries = nc_u5
# )

# > After CC ----
# p <- savePlotResults(runname = runname,
#                      legend1 = "UN IGME 2020",
#                      output.dir = output.dir,
#                      pdf.or.png = "pdf",
#                      fig.dir = my_fig_dir
# )
# After CC compared to CC
dim(res.cqt.CC) # 195,3,78 --- filter to just one country
p <- savePlotResults(runname = "U5MR_Total_AfterCC",
                     After_CC = TRUE,
                     output.dir = output.dir,
                     output.dir2 = output.dir.CC,
                     res.cqt1 = res.cqt.AggFinal,
                     legend1 = "UN IGME 2020",
                     legend2 = "Country consultation",
                     fig.dir = "fig2020AfterCC/",
                     pdf.or.png = png_or_pdf0,
                     iso.subset.c = iso.subset.c0,
                     save_cqt_copy = TRUE
)

# just one country
p <- savePlotResults(runname = NULL,
                     filename = "TEST",
                     After_CC = TRUE,
                     output.dir = output.dir,
                     res.cqt2 = res.cqt.CC,
                     legend1 = "UN IGME 2020",
                     legend2 = "Country consultation",
                     fig.dir = "fig/",
                     pdf.or.png = "pdf",
                     iso.subset.c = "TKM"
)
p <- savePlotResults(runname = "TKM_U5MR",
                     output.dir = output.dir,
                     legend1 = NULL,
                     fig.dir = "fig/",
                     pdf.or.png = "png",
                     iso.subset.c = "TKM"
)

# 2.IMR ---------------------------------------------------------------------
runname_IMR <- "IMR20200219_all"
runname_IMR_CC <- "IMR20200219_all_CC"
output.dir.IMR <- file.path(dir_IGME_out_folder, runname_IMR) # 2020 after CC on dropbox
output.dir.IMR.CC <- file.path(dir_IGME_out_folder, runname_IMR_CC) # 2020 CC on dropbox
# 2020 CC with updated mcmc.meta
# output.dir.IMR <- file.path(dir_IGME_out_folder, "IMR_20200814") # 2020 CC on dropbox
# assign.my.fig.dir(subfolder = runname)
# my_fig_dir

p <- savePlotResults(runname = "IMR_Total_AfterCC",
                     After_CC = TRUE,
                     output.dir = output.dir.IMR,
                     output.dir2 = output.dir.IMR.CC,
                     legend1 = "UN IGME 2020",
                     legend2 = "Country consultation",
                     fig.dir = "fig2020AfterCC/",
                     pdf.or.png = png_or_pdf0,
                     iso.subset.c = iso.subset.c0,
                     save_cqt_copy = TRUE
)

# just one country
p <- savePlotResults(runname = "TKM_IMR",
                     After_CC = TRUE,
                     output.dir = output.dir.IMR,
                     res.cqt2 = res.cqt.IMR.CC,
                     legend1 = "UN IGME 2020",
                     legend2 = "Country consultation",
                     fig.dir = "fig/",
                     pdf.or.png = "png",
                     iso.subset.c = "TKM"
)
p <- savePlotResults(runname = "TKM_IMR",
                     output.dir = output.dir.IMR,
                     legend1 = NULL,
                     fig.dir = "fig/",
                     pdf.or.png = "png",
                     iso.subset.c = "TKM"
)

# 3.NMR ---------------------------------------------------------------------
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
# results CC (used for CC)
resultsfile_CC = file.path(workdir, "output/final/CC/finalresults.jtc.Rda")
resultsfile_ratio_CC = file.path(workdir, "output/final/CC/finalresults_ratio.jtc.Rda")

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
    resultsfile2 = if(scale0=="NMR") resultsfile_CC else resultsfile_ratio_CC,
    resultsfile3 = if(scale0=="NMR") resultsfile_2019 else resultsfile_ratio_2019,
    new.cname.df = new_cnames,
    expectedresultsfile = if(scale0=="NMR") expectedresultsfile else expectedresultsfile_ratio,
    igmefile = NULL,
    crisisadjfile = crisisadjfile,
    scale = scale0,
    new_entry_date = "2020-06"
  )
}

# output_dir_nmr <- file.path(workdir, "figData")
output_dir_nmr <- "figData"

# just run once and save, also if NMR results files have been updated, run again
resl_rate <- get.nmr.resl("NMR")
resl_ratio <- get.nmr.resl("ratio")
saveRDS(resl_rate, file.path(output_dir_nmr, "mcmc.meta.nmr.rda"))
saveRDS(resl_ratio, file.path(output_dir_nmr, "mcmc.meta.nmr.ratio.rda"))

# > After CC ----

p <- savePlotResults(runname = "NMR_Total_AfterCC",
                     After_CC = TRUE,
                     pdf.or.png = png_or_pdf0,
                     NMR_metafile = resl_rate,
                     fig.dir = "fig2020AfterCC/",
                     legend1 = "UN IGME 2020",
                     legend2 = "Country consultation",
                     iso.subset.c = iso.subset.c0,
                     save_cqt_copy = TRUE
)

# for one country
p <- savePlotResults(runname = "TKM",
                     After_CC = TRUE,
                     pdf.or.png = "png",
                     NMR_metafile = resl_rate,
                     fig.dir = "fig/",
                     legend1 = "UN IGME 2020",
                     legend2 = "Country consultation",
                     iso.subset.c = "TKM"
)

p <- savePlotResults(runname = "TKM",
                     pdf.or.png = "png",
                     NMR_metafile = resl_rate,
                     fig.dir = "fig/",
                     legend1 = NULL,
                     # legend2 = "Country consultation",
                     iso.subset.c = "TKM"
)


# 4. Sex-specific ---------------------------------------------------------
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
resultsfile1 <- file.path(workdir, "data/output/M49_one/cis_M49_one_full.rda")
resultsfile_CC <- file.path(workdir, "data/output/M49_one_CC/cis_M49_one_full.rda")
resultsfile_2019 <- file.path(workdir, "data/output/2019/M49_one/cis_M49_one_full.rda")
if(!file.exists(crisisadjfile)|!file.exists(mlinfo)) message("check if filerate and fileratio exists")
if(!file.exists(resultsfile1)|!file.exists(resultsfile_2019)) message("check if filerate and fileratio exists")

load(resultsfile1) # loads the list `res.full` to get `isos_results`
isos_results <- res.full$iso.c
# saveRDS(isos_results, "figData/sexspecific.iso.c.rds")
# sexspecific.iso.c <- readRDS(file.path(workdir, "figData/sexspecific.iso.c.rds"))

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
# CME.assistant::get.match("f", new_list = title_name_list)
output_dir_gender <- "figData"

# produce a grid of ind x gender
cqt_files <- get.gender.cqt.grid()

get.resl <- function(ind0, gender0){
  resl1 <- transformdataSexSpecific(
    dt_gender = dt_gender,
    new_entry_date = "2020-06",
    indicator = CME.assistant::get.match(ind0, new_list = ind0_name_list),
    indicator_label = CME.assistant::get.match(ind0, new_list = ylab_name_list),
    sex = CME.assistant::get.match(gender0, new_list = title_name_list),
    iso = isos_results,
    # Include cqt series:
    resultsfile_cqt = get.gender.cqt(ind0, gender0, resultsfile = resultsfile1),
    expectedresultsfile_cqt =  get.gender.cqt(ind0, gender0, get_expected = TRUE, resultsfile = resultsfile1),
    resultsfile_cqt2 = get.gender.cqt(ind0, gender0, resultsfile = resultsfile_CC),
    resultsfile_cqt3 = get.gender.cqt(ind0, gender0, resultsfile = resultsfile_2019),
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
# # to preview a mcmc.meta created:
# resl0 <- readRDS(file.path(output_dir_gender,
#                            paste0("mcmc.meta.gender.","Q5",".", "f",".rda")))
# resl0$res.cqt[78,,]
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

# test
p = savePlotResults(
  filename = "test",
  output.dir = output_dir_gender,
  pdf.or.png = "pdf",
  fig.dir = "fig",
  # new_entry_date = last.October(),
  gender_ind = "Q5", # takes either U5MR or Q5, case-insensitive
  gender = 'f'
  # iso.subset.c = "MCO"
)
# also be careful with # MCO # MRT

# We have saved in `output_dir_gender` 9 mcmc.meta files corresponding to `resultsfile1`:
list.files(output_dir_gender)

# > After CC   --------------------------------------
save.all <- function(ind0, gender0){
  p <- savePlotResults(
    After_CC = TRUE,
    filename = "AfterCC_Results",
    legend1 = "UN IGME 2020",
    legend2 = "Country consultation",
    pdf.or.png = png_or_pdf0,
    output.dir = output_dir_gender,
    fig.dir = "fig2020AfterCC/",
    # new_entry_date = "2020-06",
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    gender = gender0,
    iso.subset.c = iso.subset.c0,
    save_cqt_copy = TRUE
  )
  return(p)
}
# p <- save.all("Q5", "f")
# # e.g.:
p_list <- invisible(Map(save.all,
                        ind0 = c("Q5", "Q5", "Q1", "Q1"), gender0 = c("f", "m","f", "m")))

# to make all
# p_list <- invisible(Map(save.all, ind0 = cqt_files$ind, gender0 = cqt_files$gender))

# make only one country
save.all <- function(ind0, gender0){
  p <- savePlotResults(
    After_CC = TRUE,
    filename = "AfterCC_Results",
    legend1 = "UN IGME 2020",
    legend2 = "Country consultation",
    pdf.or.png = png_or_pdf0,
    output.dir = output_dir_gender,
    fig.dir = "fig/",
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    gender = gender0,
    iso.subset.c = iso.subset.c0,
    save_cqt_copy = FALSE
  )
  return(p)
}
# p <- save.all("Q5", "f")
# # e.g.:
p_list <- invisible(Map(save.all,
                        ind0 = c("Q5", "Q5", "Q1", "Q1"), gender0 = c("f", "m","f", "m")))



# 5. 5-14 ----
# for 10q5 Pre-CC
output.dir.20CC <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2020 Round Estimation/Country consultation/")
# for 10q5 after CC
output.dir.20 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2020 Round Estimation/output/10q5-IGME2020GLOBALRUN-2_all")
load(file.path(output.dir.20, "mcmc.meta.rda"))
special_isos_5_14 <- get.special.isos(mcmc.meta)$special_isos
dtmethodu5mr[ISO.Code%in%special_isos_5_14, Method.5_14:= "Derived from U5MR"]

# 1.1.-Results
p <- savePlotResults(runname = "MR5-14_AfterCC",
                     After_CC = TRUE,
                     output.dir = output.dir.20,
                     output.dir2 = output.dir.20CC,
                     year.start = 1990,
                     legend1 = "UN IGME 2020",
                     legend2 = "Country consultation",
                     ylab = "10q5",
                     pdf.or.png = png_or_pdf0,
                     fig.dir = "fig2020AfterCC",
                     iso.subset.c = iso.subset.c0,
                     save_cqt_copy = TRUE
)

# # 5-9 ----
output.dir.20 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2020 Round Estimation/output/5q5-IGME2020GLOBALRUN-2_all")
load(file.path(output.dir.20, "mcmc.meta.rda"))
special_isos_5_9 <- get.special.isos(mcmc.meta)$special_isos
dtmethodu5mr[ISO.Code%in%special_isos_5_9, Method.5_9:= "Derived from U5MR"]

p <- savePlotResults(runname = "MR5-9_IGME2020",
                     output.dir = output.dir.20,
                     year.start = 1990,
                     legend1 = "UN IGME 2020",
                     ylab = "5q5",
                     pdf.or.png = "pdf",
                     fig.dir = "fig2020AfterCC",
                     n.countries = NULL,
                     save_cqt_copy = TRUE
)


# 6. 15-24 ----
# for 10q15 Pre-CC
output.dir.20CC <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/IGME 5-14/Estimates 10q15/Country consultation")
# for 10q15 after CC
output.dir.20 <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/IGME 5-14/Estimates 10q15/output/10q15-IGME2020GLOBALRUN-11_all")
load(file.path(output.dir.20, "mcmc.meta.rda"))
special_isos_15_24 <- get.special.isos(mcmc.meta)$special_isos
dtmethodu5mr[ISO.Code%in%special_isos_15_24, Method.15_24:= "Derived from U5MR"]

p <- savePlotResults(runname = "MR15-24_AfterCC",
                     After_CC = TRUE,
                     output.dir = output.dir.20,
                     output.dir2 = output.dir.20CC,
                     year.start = 1990,
                     legend1 = "UN IGME 2020",
                     legend2 = "Country consultation",
                     ylab = "10q15",
                     pooling_weight = "0.8", ## !!
                     pdf.or.png = png_or_pdf0,
                     fig.dir = "fig2020AfterCC",
                     iso.subset.c = iso.subset.c0,
                     save_cqt_copy = TRUE
)

# 15-19 ----
output.dir.20 <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/IGME 5-14/Estimates 10q15/output/5q15-IGME2020GLOBALRUN-11_all")
load(file.path(output.dir.20, "mcmc.meta.rda"))
special_isos_15_19 <- get.special.isos(mcmc.meta)$special_isos
dtmethodu5mr[ISO.Code%in%special_isos_15_19, Method.15_19:= "Derived from U5MR"]

p <- savePlotResults(runname = "MR15-19_IGME2020",
                     output.dir = output.dir.20,
                     year.start = 1990,
                     legend1 = "UN IGME 2020",
                     ylab = "5q15",
                     pooling_weight = "0.8", ## !!
                     pdf.or.png = "pdf",
                     fig.dir = "fig2020AfterCC",
                     n.countries = NULL,
                     save_cqt_copy = TRUE
)


# Save model methods ------------------------------------------------------
dt_new_cnames <- setDT(new_cnames)[,.(OfficialName, ISO3Code)]
setnames(dt_new_cnames, c("Country.Name",  "ISO.Code"))
setkey(dt_new_cnames, ISO.Code)
setkey(dtmethodu5mr, ISO.Code)
dtmethodu5mr2 <- dt_new_cnames[dtmethodu5mr]
setorder(dtmethodu5mr2, Country.Name)
writexl::write_xlsx(dtmethodu5mr2, "ModelMethod/ModelMethod.xlsx")

