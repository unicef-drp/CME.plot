# data plot for all

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



# U5MR and IMR --------------------------------------------------------------------
dir_IGME_out_folder <- get.IGMEoutput.dir(2021)

runname.U5MR <- "U5MR_20210409"
runname.IMR <- "IMR_20210409"

output.dir.U5MR <- file.path(dir_IGME_out_folder, runname.U5MR) #  where the mcmc.meta.rda is
output.dir.IMR <- file.path(dir_IGME_out_folder, runname.IMR) #  where the mcmc.meta.rda is
# to make data plot, set `legend1 = NULL` if there is estimates file (res.cqt.Lw.rda)
# plot all countries
savePlotResults(runname = runname.U5MR,  # which will be used in the pdf file name
                output.dir = output.dir.U5MR, # where the mcmc.meta.rda (required) sits
                pdf.or.png = 'pdf',      # pdf or png
                new_entry_date = "2020-10",
                legend1 = NULL           # to make data plot, set `legend1 = NULL` if needed
)
savePlotResults(runname = runname.IMR,  # which will be used in the pdf file name
                output.dir = output.dir.IMR, # where the mcmc.meta.rda (required) sits
                pdf.or.png = 'pdf',      # pdf or png
                new_entry_date = "2020-10",
                legend1 = NULL           # to make data plot, set `legend1 = NULL` if needed
)

# plot countries with new series alone
dt_CME <- fread(file.path(output.dir.U5MR, "data_CMEInfo.csv"))
dt_CME[, Date.Of.Data.Added2:= as.Date(paste0(sub("-", "", Date.Of.Data.Added), "01"), format = "%Y%m%d")]
review.date.of.dataentry(output.dir.U5MR)
isos_new_series <- dt_CME[Date.Of.Data.Added2>="2020-10-01" , unique(Country.Code)]

pdf.or.png <- "png"
savePlotResults(runname = runname.U5MR,  # which will be used in the pdf file name
                # filename = "New_Census",
                iso.subset.c = isos_new_series,
                output.dir = output.dir.U5MR, # where the mcmc.meta.rda (required) sits
                pdf.or.png = pdf.or.png,      # pdf or png
                new_entry_date = "2020-10",
                legend1 = NULL           # to make data plot, set `legend1 = NULL`
)

# plot countries with new series alone
dt_CME_IMR <- fread(file.path(output.dir.IMR, "data_CMEInfo.csv"))
dt_CME_IMR[, Date.Of.Data.Added2:= as.Date(paste0(sub("-", "", Date.Of.Data.Added), "01"), format = "%Y%m%d")]
isos_new_series_IMR <- dt_CME_IMR[Date.Of.Data.Added2>="2020-10-01" , unique(Country.Code)]
savePlotResults(runname = runname.IMR,  # which will be used in the pdf file name
                # filename = "New_Census",
                iso.subset.c = isos_new_series_IMR,
                output.dir = output.dir.IMR, # where the mcmc.meta.rda (required) sits
                pdf.or.png = pdf.or.png,      # pdf or png
                new_entry_date = "2020-10",
                legend1 = NULL           # to make data plot, set `legend1 = NULL`
)


# NMR ---------------------------------------------------------------------
# files required
filerate = get.dir_NMR(y5 = TRUE)
fileratio = get.dir_NMR()
if(!file.exists(filerate)|!file.exists(fileratio)) message("check if filerate and fileratio exists")

check_ratio_and_logit <- function(file_dir){
  nmr <- fread(file_dir)
  nmr$Estimates.check.rates<-with(nmr,log(Neonatal/(U5MR-Neonatal)))
  nmr$Estimates.check.ratio<-with(nmr,log(Ratio/(1-Ratio)))
  nmr$Estimates<-with(nmr,ifelse(is.na(Estimates) & !is.na(Estimates.check.rates),Estimates.check.rates,Estimates))
  nmr$Estimates<-with(nmr,ifelse(is.na(Estimates) & !is.na(Estimates.check.ratio),Estimates.check.ratio,Estimates))

  nmr$ratio.c<-with(nmr,ifelse(is.na(Ratio) & !is.na(U5MR) & is.na(Neonatal),Neontal/U5MR,Ratio))
  nmr$Ratio<-with(nmr,ifelse(is.na(Ratio) & !is.na(ratio.c), ratio.c,Ratio))

  nmr$diffest1<-nmr$Estimates.check.rates-nmr$Estimates
  nmr$diffest2<-nmr$Estimates.check.ratio-nmr$Estimates
  t<-nmr$diffest1[!is.na(nmr$diffest1)]
  hist(t)
  nmr$Estimates.check.rates<-NULL
  nmr$Estimates.check.ratio<-NULL
  nmr$diffest1<-NULL
  nmr$diffest2<-NULL
  nmr$ratio.c<-NULL
  nmr$Estimates<-with(nmr,ifelse(Estimates==-Inf,NA,Estimates))
  #order the database
  nmr <- nmr[with(nmr, order(Country.Code, -as.numeric(Average.date.of.Survey), Series.Name, Series.Type, -Reference.Date)),]
  fwrite(nmr, file_dir)
}
check_ratio_and_logit(file_dir = filerate)
check_ratio_and_logit(file_dir = fileratio)

# u5median.crisisandhivfree.rda required by `GetDataU5MRNMR`
u5median.crisisandhivfree_file <- file.path(Sys.getenv("USERPROFILE"),'/Dropbox/NMR/data/u5median.crisisandhivfree.rda')
file.exists(u5median.crisisandhivfree_file)
# crisis adjustment
crisisadjfile <- file.path(Sys.getenv("USERPROFILE"),'Dropbox/NMR/data/reference/dataPostAdj_U5MR.csv')
file.exists(crisisadjfile)

# runname is used as label in the filename, first NMR rate then ratio
runname <- "NMR_20210409"

# NMR
meta_file <- transformdataforNMR(
  file = filerate,
  new.cname.df = new_cnames,
  crisisadjfile = crisisadjfile,
  scale = "NMR",
  new_entry_date = "2020-10"
)

# to plot isos with new series
dt_nmr <- fread(get.dir_NMR(y5 = TRUE)) # use either dataset
dt_nmr[, Date.Of.Data.Added2:= as.Date(paste0(sub("-", "", Date.Of.Data.Added), "01"), format = "%Y%m%d")]
isos_new_series_nmr <- dt_nmr[Date.Of.Data.Added2>="2020-10-01" , unique(Country.Code)]
# save data plot, add `new_entry_date` will highlight new entries
savePlotResults(runname = runname,
                iso.subset.c = isos_new_series_nmr,
                legend1 = NULL,
                NMR_metafile = meta_file,
                new_entry_date = "2020-10",
                pdf.or.png = pdf.or.png
)

# Ratio
meta_file_ratio <- transformdataforNMR(
  file = filerate,
  new.cname.df = new_cnames,
  crisisadjfile = crisisadjfile,
  scale = "ratio",
  new_entry_date = "2020-10"
)
# save data plot, add `new_entry_date` will highlight new entries
savePlotResults(runname = runname,
                iso.subset.c = isos_new_series_nmr,
                legend1 = NULL,
                NMR_metafile = meta_file_ratio,
                new_entry_date = "2020-10" ,
                pdf.or.png = pdf.or.png
)


# Sex-specific ------------------------------------------------------------
workdir <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/CMEgender2015")

# load dataset for plotting
filerate <- get.dir_gender(plotting = TRUE)
dt_gender <- fread(filerate)
dt_gender <- fix.entries.dt_gender(dt_gender) # fix some issues in the forplotting dataset

# countries with new series added:
dt_gender[, Date.Of.Data.Added2:= as.Date(paste0(sub("-", "", Date.Of.Data.Added), "01"), format = "%Y%m%d")]
isos_new_series_sex <- dt_gender[Date.Of.Data.Added2>="2020-10-01" , unique(Country.Code)]

# review entries date
review.date.of.dataentry(dir_file = filerate)
# add crisisadjfile to exclude some data points like Japan 2011 and 2015
crisisadjfile <- file.path(workdir, "data/crisis mortality/dataPostAdj_U5MR.csv")
# model life table information to be used in IMR VR inclusion criteria
mlinfo <- file.path(workdir, "data/input/MLinfo.csv")

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
    iso = sexspecific.iso.c,
    new.cname.df = new_cnames,
    resultsiso = sexspecific.iso.c,
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


# 4.1 Sex-specific-Data --------------------------------------------------
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
    iso.subset.c = isos_new_series_sex, # plot only a subset of the countries
  )
}
# save.all("Q5", "f")
invisible(Map(save.all, ind0 = cqt_files$ind, sex0 = cqt_files$gender))


