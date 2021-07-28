# "Make CME 2020 Plots"
# incl plots for pre CC
# date: "2/3/2020 - 2021"

run.on.server <- FALSE # Indicate if run is on the server
username <- Sys.getenv("USERPROFILE")
workdir <- file.path(username, "/Dropbox/UN IGME Data/2020 Round Estimation/Code") # Give work directory file path if not running things on server

# Define working directory
if (run.on.server) {
  package.dir <- workdir <- getwd()
} else {
  package.dir <- workdir
}

source(file.path(package.dir, "R/loadlibrariesandcodes.R"))
LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir)


# U5MR -----------------------------------------------------------------------
# where is the "output" folder
dir_IGME_out_folder <- get.IGMEoutput.dir(2020)
# if run on server:
# dir_IGME_out_folder <- file.path(getwd(), "output")

runname_CC <- "GR20200214_all_CC"
runname <- "GR20200214_all"
runname_19 <- "IGME2019"
output.dir.CC <- file.path(dir_IGME_out_folder, runname_CC) # 2020 CC on Dropbox
output.dir <- file.path(dir_IGME_out_folder, runname) # 2020 on Dropbox
output.dir.19 <- file.path(dir_IGME_out_folder, runname_19) # 2019 on Dropbox

# 1.0 U5MR Before Country Consultation (CC) -----------------------------------
savePlotResults(runname = "U5MR_Total_CC",
                     output.dir = output.dir,
                     pdf.or.png = "pdf",
                     legend1 = "B3", # Before CC uses a different legend
                     fig.dir = "fig/PreCC",
                     save_cqt_copy = TRUE
)



# 1.1.U5MR-Results ------------------------------------------------------------
savePlotResults(runname = runname,
                     new_entry_date = "2020-10",
                     output.dir = output.dir,
                     pdf.or.png = "pdf",
                     fig.dir = "fig"
)


# 1.1.HIV-removed results for HIV countries -----------------------------------
# I did not find `res.hivremoved.cqt.Lw.rda` in "GR20190311_all" so I just used this for now.
savePlotResults(runname = runname,
                     new_entry_date = "2020-10",
                     output.dir = output.dir,
                     fig.dir = "fig",
                     HIV_removed = TRUE,
                     legend1 = "UN IGME 2020 (HIV-removed)" # so legend is different from default "UN IGME 2020"
)

# compared to 2019 (in this case, it is easier to supply res.cqt2 directly)
load(file.path(dir_IGME_out_folder, "IGME2019/res.hivremoved.cqt.Lw.rda"))
hivremoved.cqt.2019 <- res.hivremoved.cqt.Lw$`0.5`
savePlotResults(runname = runname,
                     new_entry_date = "2020-10",
                     output.dir = output.dir,
                     fig.dir = "fig",
                     HIV_removed = TRUE,
                     legend1 = "UN IGME 2020 (HIV-removed)",
                     res.cqt2 = hivremoved.cqt.2019,
                     legend2 = "UN IGME 2019 (HIV-removed)"
)


# 1.2.U5MR-Data Plot ------------------------------------------------------------
savePlotResults(runname = runname,
                     legend1 = NULL,
                     new_entry_date = "2020-10",
                     output.dir = output.dir,
                     fig.dir = "fig")


# 1.3.Compared to last year ----------------------------------------------
savePlotResults(runname = runname,
                     new_entry_date = "2020-10",
                     output.dir = output.dir,
                     output.dir2 = output.dir.19,
                     legend2 = "UN IGME 2019",
                     fig.dir = "fig")

# 1.4. WPP IHME ---------------------------------------------------------
savePlotResults(runname = runname,
                     new_entry_date = "2020-10",
                     output.dir = output.dir,
                     pdf.or.png = "pdf",
                     # wpp only has estimates
                     wpp.cqt = u5mr.wpp.cqt.2019,
                     ihme.cqt = u5mr.ihme.cqt.2019,
                     # col.ihme = "white" # if want to hide CI for IHME
                     fig.dir = "fig"
)

# 1.5 Add all together (4 series)  ----------------------------------------
savePlotResults(runname = runname,
                     new_entry_date = "2020-10",
                     output.dir2 = output.dir.19,
                     legend2 = "UN IGME 2019",
                     output.dir = output.dir,
                     pdf.or.png = "pdf",
                     wpp.cqt = u5mr.wpp.cqt.2019,
                     ihme.cqt = u5mr.ihme.cqt.2019,
                     # col.ihme = "white" # if want to hide CI for IHME
                     fig.dir = "fig"
)



# IMR ---------------------------------------------------------------------
# Notice: update both runname and output.dir!! (since they are different from U5MR)
runname_IMR <- "IMR20200219_all"
runname_IMR_19 <- "IMR20190314_all"
output.dir.IMR <- file.path(dir_IGME_out_folder, runname_IMR) # 2020 on dropbox
output.dir.IMR.19 <- file.path(dir_IGME_out_folder, runname_IMR_19) # 2019 on dropbox

# load mcmc.meta only for adding IMR model into the legend: `mcmc.meta$data.all$imrmethod.c`
load(file.path(output.dir.IMR, "mcmc.meta.rda"))

# 2.0. IMR Pre Country Consultation ----
savePlotResults(runname = "IMR_Total_CC",
                     output.dir = output.dir.IMR,
                     pdf.or.png = "pdf",
                     legend1 = mcmc.meta$data.all$imrmethod.c,
                     fig.dir = "fig/PreCC",
                     save_cqt_copy = TRUE
)

# 1. Results
savePlotResults(runname = runname_IMR,
                     new_entry_date = "2020-10",
                     output.dir = output.dir.IMR,
                     pdf.or.png = "pdf",
                     fig.dir = "fig"
)


# 2.1 HIV-removed -------------------------------------------------------------
# need the file `res.hivremoved.cqt.Lw.rda` in respective folders
# savePlotResults(runname = runname_IMR,
#                      new_entry_date = "2020-10",
#                      output.dir = output.dir.IMR,
#                      fig.dir = "fig",
#                      HIV_removed = TRUE,
#                      legend1 = "UN IGME 2020 (HIV-removed)"
# )
#
# # HIV-removed compared to 2019
# load(file.path(output.dir.IMR.19, "res.hivremoved.cqt.Lw.rda"))
# hivremoved.cqt.imr2019 <- res.hivremoved.cqt.Lw$`0.5`
# savePlotResults(runname = runname_IMR,
#                      new_entry_date = "2020-10",
#                      output.dir = output.dir.IMR,
#                      fig.dir = "fig",
#                      HIV_removed = TRUE,
#                      legend1 = "UN IGME 2020 (HIV-removed)",
#                      legend2 = "UN IGME 2019 (HIV-removed)",
#                      res.cqt2 = hivremoved.cqt.imr2019
# )


# 2. Data Plot
savePlotResults(runname = runname_IMR,
                     legend1 = NULL,
                     new_entry_date = "2020-10",
                     output.dir = output.dir.IMR,
                     fig.dir = "fig")


# 3. Compared to last year
savePlotResults(runname = runname_IMR,
                     new_entry_date = "2020-10",
                     output.dir = output.dir.IMR,
                     output.dir2 = output.dir.IMR.19,
                     legend2 = "UN IGME 2019",
                     fig.dir = "fig"
)

# 4. WPP IHME
savePlotResults(runname = runname_IMR,
                     new_entry_date = "2020-10",
                     output.dir = output.dir.IMR,
                     wpp.cqt = imr.wpp.cqt.2019,
                     ihme.cqt = imr.ihme.cqt.2019,
                     fig.dir = "fig"
)

# 5 Add all together (4 series)
savePlotResults(runname = runname_IMR,
                     new_entry_date = "2020-10",
                     output.dir = output.dir.IMR,
                     output.dir2 = output.dir.IMR.19,
                     legend2 = "UN IGME 2019",
                     wpp.cqt = imr.wpp.cqt.2019,
                     ihme.cqt = imr.ihme.cqt.2019,
                     # col.ihme = "white", # if want to hide CI for IHME
                     fig.dir = "fig"
)



# NMR ---------------------------------------------------------------------
workdir <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/NMR/")
# directories to the data_NMR_.....csv:
filerate <- get.dir_NMR(y5 = TRUE)
fileratio <- get.dir_NMR(y5 = FALSE)

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
    scale = scale0,
    new_entry_date = "2020-10"
  )
}
resl_rate <- get.nmr.resl("NMR")
resl_ratio <- get.nmr.resl("ratio")

# 3.0. NMR Pre CC ----
savePlotResults(runname = "NMR_Total_CC",
                     NMR_metafile = resl_rate,
                     pdf.or.png = "pdf", # save as pdf
                     legend1 = "BHSR",
                     fig.dir = "fig/PreCC",
                     save_cqt_copy = TRUE
)


# 3.1 Rate and Ratio Results ----------------------------------------------
runname.NMR <- "NMR_20210408" #

savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_rate,
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)

savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_ratio,
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)


# 3.2 Rate and Ratio Compared to expected  -------------------------------------------
savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_rate,
                     legend_ex = "Expected", # turn on the expected series
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)
savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_ratio,
                     legend_ex = "Expected",
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)

# 3.2 compared to last year --------------------------------------------------
savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_rate,
                     legend2 = "UN IGME 2019",
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)


savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_rate,
                     legend2 = "UN IGME 2019",
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)


# 3.3 Compared to IHME --------------------------------------------------------
savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_rate,
                     ihme.cqt = nmr.ihme.cqt.2019,
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)

# all three series together
savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_rate,
                     legend2 = "UN IGME 2019",
                     ihme.cqt = nmr.ihme.cqt.2019,
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)

# all four series together (incl. expected)
savePlotResults(runname = runname.NMR,
                     NMR_metafile = resl_rate,
                     legend2 = "UN IGME 2019",
                     legend_ex = "Expected",
                     ihme.cqt = nmr.ihme.cqt.2019,
                     fig.dir = "fig",
                     new_entry_date = "2020-10"
)


# Sex-specific ---------------------------------------------------------
workdir <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/CMEgender2015")

# load dataset for plotting
filerate <- get.dir_gender(plotting = TRUE)
dt_gender <- fread(filerate)
dt_gender <- fix.entries.dt_gender(dt_gender) # fix some issues in the forplotting dataset

# countries with new series added:
dt_gender[, Date.Of.Data.Added2:= as.Date(paste0(sub("-", "", Date.Of.Data.Added), "01"), format = "%Y%m%d")]
isos_new_series <- dt_gender[Date.Of.Data.Added2>="2020-10-01" , unique(Country.Code)]

# review entries date
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

load(resultsfile1) # loads the list `res.full` to get the desired `isos_results`
isos_results <- res.full$iso.c # could also just use sexspecific.iso.c

message("These expected files exist in resultsfile1: ",
        paste0(paste0("exp", get.gender.cqt.grid()$name)[paste0("exp", get.gender.cqt.grid()$name)%in%grep("exp", names(res.full), value = TRUE)], collapse = ", ")
)
# expQ5f.cqt, expQ1f.cqt, expQ4f.cqt

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
output_dir_gender <- "output/figDataTemp"

# produce a grid of ind x gender
cqt_files <- get.gender.cqt.grid()
get.resl <- function(ind0, sex0){
  resl1 <- transformdataSexSpecific(
    dt_gender = dt_gender,
    new_entry_date = "2020-10",
    indicator = get.match(ind0, new_list = ind0_name_list),
    indicator_label = get.match(ind0, new_list = ylab_name_list),
    sex = get.match(sex0, new_list = title_name_list),
    iso = isos_results,
    # to supply the following series:
    # the cqts from resultsfile1
    resultsfile_cqt = get.gender.cqt(ind0, sex0, resultsfile = resultsfile1),
    # the expected cqts from resultsfile1
    expectedresultsfile_cqt =  get.gender.cqt(ind0, sex0, get_expected = TRUE, resultsfile = resultsfile1),
    # the cqts from resultsfile_2019
    resultsfile_cqt2 = get.gender.cqt(ind0, sex0, resultsfile = resultsfile_2019),
    new.cname.df = new_cnames,
    resultsiso = isos_results,
    mlinfo = mlinfo,
    crisisadjfile = crisisadjfile)
  # return(resl1)
  if(!dir.exists(output_dir_gender)) dir.create(output_dir_gender, recursive = TRUE)
  saveRDS(resl1, file.path(output_dir_gender,
                           paste0("mcmc.meta.gender.",ind0,".", sex0,".rda")))
}
get.resl("Q5", "f")
# To create all output resls:
invisible(Map(get.resl, ind0 = cqt_files$ind, sex0 = cqt_files$gender))
# to preview a mcmc.meta created:
# resl0 <- readRDS(file.path(output_dir_gender,
#                            paste0("mcmc.meta.gender.","Q5",".", "f",".rda")))

# We have saved in `output_dir_gender` 9 mcmc.meta files corresponding to `resultsfile1`:
list.files(output_dir_gender)


# 4.0 Sex-specific pre Country Consultation ----------------------------------
#  double check MCO/Monaco, MRT/Mauritania and LUX/Luxembourg
#` save.all helps to save all the combinations of indicator (ind0) and sex0 (sex) provided
save.all <- function(ind0, sex0, pdf_or_png){
  savePlotResults(
    filename = "CC_Results",
    output.dir = output_dir_gender,
    pdf.or.png = pdf_or_png,
    legend1 = "B3SR",
    fig.dir = "fig/PreCC",
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    gender = sex0, # takes f or m
    # n.countries = NULL,
    # iso.subset.c = "LUX",
    save_cqt_copy = TRUE
  )
}
#
save.all("Q5", "f", "pdf")
save.all("Q5", "m", "pdf")
save.all("Q1", "f", "pdf")
save.all("Q1", "m", "pdf")
# to save png:
# save.all("Q5", "f", "png")
# save.all("Q5", "m", "png")
# save.all("Q1", "f", "png")
# save.all("Q1", "m", "png")


# 4.1 Sex-specific-Results --------------------------------------------------
# data plot
save.all <- function(ind0, sex0){
  #` save.all helps to save all the combinations provided
  savePlotResults(
    output.dir = output_dir_gender,
    fig.dir = "fig/sex-specific",
    new_entry_date = "2020-10",
    legend1 = NULL,
    gender_ind = ind0, # takes indicators in the forms of either "U5MR" or "Q5"
    gender = sex0, # takes f or m
    iso.subset.c = isos_new_series, # plot only a subset of the countries
  )
}
# save.all("Q5", "f")
invisible(Map(save.all, ind0 = cqt_files$ind, sex0 = cqt_files$gender))

# results
save.all <- function(ind0, sex0){
  savePlotResults(
    # filename = "test",
    output.dir = output_dir_gender,
    fig.dir = "fig/sex-specific",
    new_entry_date = "2020-10",
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    gender = sex0, # takes f or m
    n.countries = NULL
  )
}
#
save.all("Q5", "f")
invisible(Map(save.all, ind0 = cqt_files$ind, sex0 = cqt_files$gender))


# 4.2 compared to expected (only exist for gender = `f`) ----------------------
save.all <- function(ind0){
  savePlotResults(
    # runname = "vs_Expected",
    legend_ex = "Expected",
    pdf.or.png = "pdf",
    output.dir = output_dir_gender,
    fig.dir = "fig/sex-specific",
    new_entry_date = "2020-10",
    gender = "f", #
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    n.countries = NULL
  )
}
# # e.g.:
save.all("Q5")
invisible(lapply(c("Q5", "Q4", "Q1"), save.all))



# 4.3 Compared to previous year   --------------------------------------
save.all <- function(ind0, sex0){
  savePlotResults(
    # runname = "vs_UN IGME 2019",
    legend2 = "UN IGME 2019",
    pdf.or.png = "pdf",
    output.dir = output_dir_gender,
    fig.dir = file.path("fig/sex-specific", "Compared to UN IGME 2019"),
    new_entry_date = "2020-10",
    gender_ind = ind0, # takes either U5MR or Q5, case-insensitive
    gender = sex0, #
    n.countries = n_c
  )
}
# # e.g.:
invisible(Map(save.all, ind0 = cqt_files$ind, sex0 = cqt_files$gender))


# 4.4 Compared to IHME and WPP   ---------------------------------------
save.all <- function(ind0, sex0){
  savePlotResults(
    output.dir = output_dir_gender,
    pdf.or.png = "pdf",
    fig.dir = file.path("fig/sex-specific", "Compared to IHME and WPP and 2019"),
    wpp.cqt = get.sex.wpp.cqt(ind0 = ind0, sex0 = sex0),
    ihme.cqt = get.ihme.cqt.2019(ind0 = ind0, sex0 = sex0),
    new_entry_date = "2020-10",
    gender = sex0, #
    gender_ind = ind0 # takes either U5MR or Q5, case-insensitive
    )
}
save.all("Q5", "f")
invisible(Map(save.all, ind0 = c("Q5", "Q5", "Q1", "Q1"), sex0 = c("f", "m","f", "m")))

# + vs. 2019
save.all <- function(ind0, sex0){
  savePlotResults(
    legend2 = "UN IGME 2019",
    output.dir = output_dir_gender,
    pdf.or.png = "pdf",
    fig.dir = file.path("fig/sex-specific", "Compared to IHME and WPP and 2019"),
    wpp.cqt = get.sex.wpp.cqt(ind0 = ind0, sex0 = sex0),
    ihme.cqt = get.ihme.cqt.2019(ind0 = ind0, sex0 = sex0),
    new_entry_date = "2020-10",
    gender = sex0, #
    gender_ind = ind0)
}
invisible(Map(save.all, ind0 = c("Q5", "Q5", "Q1", "Q1"), sex0 = c("f", "m","f", "m")))


# + vs. Expected
save.all <- function(ind0, sex0){
  savePlotResults(
    legend_ex = "Expected",
    legend2 = "UN IGME 2019",
    output.dir = output_dir_gender,
    pdf.or.png = "pdf",
    fig.dir = file.path("fig/sex-specific", "Compared to Expected and 2019 and IHME and WPP"),
    wpp.cqt = get.sex.wpp.cqt(ind0 = ind0, sex0 = sex0),
    ihme.cqt = get.ihme.cqt.2019(ind0 = ind0, sex0 = sex0),
    new_entry_date = "2020-10",
    gender = sex0, #
    gender_ind = ind0)
}
invisible(Map(save.all, ind0 = c("Q5", "Q1"), sex0 = c("f", "f")))



# 5. 5-14 -------------------------------------------------
output.dir.19 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2019 Round Estimation/output/10q5-IGME20190114_all")
output.dir.20 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2020 Round Estimation/Country consultation/")
# for 10q5 country consultation
output.dir.20.CC <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/IGME 5-14/2020 Round Estimation 10q5/Country consultation/")

runname5_14 <- "MR5-14_Total_CC"

# to get special legend: B3, Derived from U5MR
load(file.path(output.dir.20.CC, "mcmc.meta.rda"))
load(file.path(output.dir.20.CC, "year.t.rda"))
special_legend <- get.special.isos(mcmc.meta)$special_legend_vector

# 5.0 Pre Country Consultation ----
savePlotResults(runname = runname5_14,
                     output.dir = output.dir.20.CC,
                     year.start = 1990, # start at 1990 for both plots
                     legend1 = special_legend,
                     ylab = "10q5",
                     fig.dir = "fig/PreCC",
                     save_cqt_copy = TRUE
)

# 5.1 Results ----
savePlotResults(runname = runname5_14,
                     output.dir = output.dir.20,
                     year.start = 1990,
                     ylab = "10q5",
                     fig.dir = "fig"
)

# 5.2 Compared to last year ----
savePlotResults(runname = runname5_14,
                output.dir = output.dir.20,
                output.dir2 = output.dir.19,
                legend2 = "UN IGME 2019",
                year.start = 1990,
                ylab = "10q5",
                fig.dir = "fig"
)

# 6. 15-24 -------------------------------------------------
runname15_24 <- "MR15-24_Total_CC"
output.dir15_24.20.CC <- file.path(Sys.getenv("USERPROFILE"),
                                   "Dropbox/IGME 15-24/2020 Round Estimation 10q15/Country consultation/")
output.dir15_24.20 <- file.path(Sys.getenv("USERPROFILE"),
                                   "Dropbox/IGME 15-24/2020 Round Estimation 10q15/Country consultation/")

load(file.path(output.dir15_24.20.CC, "mcmc.meta.rda"))
load(file.path(output.dir15_24.20.CC, "res.cqt.Lw.rda"))
special_legend <- get.special.isos(mcmc.meta)$special_legend_vector

# 6.0 Pre CC ----
savePlotResults(runname = runname15_24,
                     output.dir = output.dir15_24.20.CC,
                     year.start = 1990,
                     legend1 = special_legend,
                     ylab = "10q15",
                     fig.dir = "fig/PreCC",
                     save_cqt_copy = TRUE
)

# 6.1 Results ----
savePlotResults(runname = runname15_24,
                     output.dir = output.dir15_24.20,
                     year.start = 1990,
                     ylab = "10q15",
                     pooling_weight = "0.8", ## !!
                     fig.dir = "fig"
)
