# functions to make IHME and WPP cqt files

devtools::load_all()

library("data.table")
# "2020 Round Estimation/Code/output/"
dir_IGME_out_folder <- get.IGMEoutput.dir(2020)

# load file paths
load.more.file.paths <- function(){
  #
  # year range: 1950 ~ 2017, collected in year 2019
  dir_IHME_2017  <<- file.path(dir_IGME_out_folder, "IHME/GBD 2017", "IHME_5q0_Mortality_estimates_2018.xlsx")
  # dir_IHME_sex contains U5MR, IMR, NMR for both sex too. But there is no UI
  dir_IHME_sex   <<- file.path(dir_IGME_out_folder, "IHME/GBD 2017", "IHME_ProbabilityOfDeath_estimates_200130.csv")
  # year range: 1950 ~ 2019, collected in Oct. 2020
  dir_IHME_2019  <<- file.path(dir_IGME_out_folder, "IHME", "GBD2019_Under5_estimates.xlsx")

  dir_wpp_Q5     <<- file.path(dir_IGME_out_folder, "WPP2019", "WPP2019_MORT_F01_2_Q5_BOTH_SEXES.xlsx")
  dir_wpp_IMR    <<- file.path(dir_IGME_out_folder, "WPP2019", "WPP2019_MORT_F01_1_IMR_BOTH_SEXES.xlsx")
  dir_wpp_female <<- file.path(dir_IGME_out_folder, "WPP2019", "WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx")
  dir_wpp_male   <<- file.path(dir_IGME_out_folder, "WPP2019", "WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx")

  # obtained Aug 2021 from Patrick: The extract covers 1950-2020 and includes all locations, including smaller unpublished ones.
  dir_wpp_5_24   <<- file.path(dir_IGME_out_folder, "WPP2019", "igme_2021_WPP2019-LT_extract.csv")

  check.file.exist <- function(file0){
    if(!file.exists(file0)) warning("Check if these required files exist: ", file0)
  }
  sapply(c(dir_IHME_2017, dir_IHME_sex,
           dir_wpp_Q5, dir_wpp_IMR, dir_wpp_female, dir_wpp_male), check.file.exist)
  return(invisible())
}
load.more.file.paths()


# WPP 2019 ----------------------------------------------------------------
# WPP has no CI, only estimates
# U5MR
u5mr.wpp.cqt.2019 <- get.wpp.cqt(dir_wpp_Q5 = dir_wpp_Q5,
                                 dir_wpp_IMR = dir_wpp_IMR,
                                 ind = "U5MR",
                                 iso_order = u5mr.iso.c)
# IMR
imr.wpp.cqt.2019 <-  get.wpp.cqt(dir_wpp_Q5 = dir_wpp_Q5,
                                 dir_wpp_IMR = dir_wpp_IMR,
                                 ind = "IMR")
# usethis::use_data(u5mr.wpp.cqt.2019)
# usethis::use_data(imr.wpp.cqt.2019)
# 5-24
dt_wpp <- fread(dir_wpp_5_24)
dt_wpp[, value:= nqx * 1000]
dt_wpp <- dt_wpp[!(AgeStart==0 & AgeSpan==5)]
dt_wppw <- dcast.data.table(dt_wpp, LocID + Year + Sex ~ AgeStart)
setnames(dt_wppw, c("0", "1"), c("IMR", "CMR"))
get.5q0 <- function(q1, q4) (1 - (1 - q1 / 1E3) * (1 - q4 / 1E3)) * 1E3
dt_wppw[, `:=`(
  U5MR = get.5q0(q1 = IMR, q4 = CMR),
  `10q5` = get.5q0(q1 = `5`, q4 = `10`),
  `10q15` = get.5q0(q1 = `15`, q4 = `20`))]
setnames(dt_wppw, c("LocID", "Year"), c("UNCode", "year"))
# dt_wppw <- dt_wppw[year %in% as.numeric(dimnames(u5mr.wpp.cqt.2019)[[3]])]

u5mr.wpp.cqt.2019 <- get.wpp.cqt(wpp_dt = dt_wppw, ind = "U5MR")
# 10q5
m10q5.wpp.cqt.2019 <- get.wpp.cqt(wpp_dt = dt_wppw[Sex=="Both sexes"], ind = "10q5")
m10q5.wpp.cqt.2019.f <- get.wpp.cqt(wpp_dt = dt_wppw[Sex=="Female"], ind = "10q5")
m10q5.wpp.cqt.2019.m <- get.wpp.cqt(wpp_dt = dt_wppw[Sex=="Male"], ind = "10q5")
# 10q15
m10q15.wpp.cqt.2019 <-  get.wpp.cqt(wpp_dt = dt_wppw[Sex=="Both sexes"], ind = "10q15")
m10q15.wpp.cqt.2019.f <-  get.wpp.cqt(wpp_dt = dt_wppw[Sex=="Female"], ind = "10q15")
m10q15.wpp.cqt.2019.m <-  get.wpp.cqt(wpp_dt = dt_wppw[Sex=="Male"], ind = "10q15")
# usethis::use_data(m10q5.wpp.cqt.2019, overwrite = TRUE)
# usethis::use_data(m10q15.wpp.cqt.2019, overwrite = TRUE)


# sex-specific
dir_wpp_female <- file.path("data-raw/WPP2019", "WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx")
dir_wpp_male <- file.path("data-raw/WPP2019", "WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx")
dir_IHME_sex <- file.path("data-raw/IHME2018", "IHME_ProbabilityOfDeath_estimates_200130.csv")
wpp_f_2019 <- read.wpp.v3(dir_wpp_female) # this is cleaned dataset with 4 columns: UNCode, year, IMR, U5MR
wpp_m_2019 <- read.wpp.v3(dir_wpp_male)
wpp_f_2019[, sex:= "f"]
wpp_m_2019[, sex:= "m"]
wpp_2019_cqt_by_sex <- rbind(wpp_f_2019, wpp_m_2019)
# usethis::use_data(wpp_2019_cqt_by_sex) # only save one data file for all sex-specific

# for example
# get sex-specific WPP 2019
Q5_f_wpp.cqt <- get.sex.wpp.cqt(ind0 = "Q5", sex0 = "f")

# IHME 2017 ---------------------------------------------------------------
ihme_2017_sex_Q5Q1_noCI <- get_dt_IHME_2017_by_sex(dir_IHME0 = dir_IHME_sex)
# usethis::use_data(ihme_2017_sex_Q5Q1_noCI)
# this dt contains:
# sex      IMR  U5MR
# both    12716 12716
# f       12716 12716
# m       12716 12716

u5mr.ihme.cqt.2017 <- get.ihme.cqt.2017(dir_IHME0 = dir_IHME_2017,
                                        ind = "U5MR")
imr.ihme.cqt.2017 <- get.sex.ihme.cqt.2017(ind = "IMR", sex = "both")
# NMR (IHME only, mean only)
nmr.ihme.cqt.2017 <- get.ihme.cqt.2017(dir_IHME0 = dir_IHME_2017,
                                       ind = "NMR")
# usethis::use_data(u5mr.ihme.cqt.2017)
# usethis::use_data(imr.ihme.cqt.2017)
# usethis::use_data(nmr.ihme.cqt.2017)

# Sex-specific ---- save only one data for the sex-specific WPP or IHMR, a
# function is called to read a specific sex and indicator combination

# get sex-specific IHME 2017
Q5_f_ihme.cqt <- get.sex.ihme.cqt.2017(ind0 = "U5MR", sex0 = "f")


# IHME 2019 ---------------------------------------------------------------
ihme_2019 <- rbindlist(lapply(c("U5MR", "IMR", "NMR", "CMR"), get_dt_IHME_2019,
                              dir_IHME0 = dir_IHME_2019))
# usethis::use_data(ihme_2019)
# no lower and upper for NMR
# columns:
# location_id year    sex      lower       mean     upper     ind
# 1:        6 1950    m      164.317411 192.210113 224.53277  U5MR
# ...
# sex      CMR   IMR   NMR  U5MR
# both    13650 13650 13650 13650
# f       13650 13650     0 13650
# m       13650 13650     0 13650

u5mr.ihme.cqt.2019 <- get.ihme.cqt.2019(ihme = ihme_2019, ind0 = "U5MR", sex0 = "both")
imr.ihme.cqt.2019  <- get.ihme.cqt.2019(ihme = ihme_2019, ind0 = "IMR",  sex0 = "both")
nmr.ihme.cqt.2019  <- get.ihme.cqt.2019(ihme = ihme_2019, ind0 = "NMR",  sex0 = "both")
# usethis::use_data(u5mr.ihme.cqt.2019)
# usethis::use_data(imr.ihme.cqt.2019)
# usethis::use_data(nmr.ihme.cqt.2019)

# produced in script 1.Process_raw_IHME_to_get_GBD2019_Under5_estimates.R:
ihme_2019_5_24 <- fread(file.path(Sys.getenv("USERPROFILE"),
                                  "Dropbox/UN IGME data/2020 Round Estimation/Code/output/IHME/GBD2019_5-24_estimates.csv"))
ihme_2019_5_24[, table(sex, ind)]
m10q5.ihme.cqt.2019 <- get.ihme.cqt.2019(ihme = ihme_2019_5_24, ind0 = "10q5", sex0 = "both")
m10q5.ihme.cqt.2019.f <- get.ihme.cqt.2019(ihme = ihme_2019_5_24, ind0 = "10q5", sex0 = "f")
m10q5.ihme.cqt.2019.m <- get.ihme.cqt.2019(ihme = ihme_2019_5_24, ind0 = "10q5", sex0 = "m")
m10q15.ihme.cqt.2019  <- get.ihme.cqt.2019(ihme = ihme_2019_5_24, ind0 = "10q15",  sex0 = "both")
m10q15.ihme.cqt.2019.f  <- get.ihme.cqt.2019(ihme = ihme_2019_5_24, ind0 = "10q15",  sex0 = "f")
m10q15.ihme.cqt.2019.m  <- get.ihme.cqt.2019(ihme = ihme_2019_5_24, ind0 = "10q15",  sex0 = "m")
# usethis::use_data(m10q5.ihme.cqt.2019)
# usethis::use_data(m10q15.ihme.cqt.2019)

# Sex-specific: call by using this function
# for example
Q5_f_ihme.cqt <- get.ihme.cqt.2019(ind0 = "U5MR", sex0 = "f")
