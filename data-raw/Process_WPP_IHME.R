# Get WPP and IHME cqt
# 2020
library("CME.assistant")
devtools::load_all()

# load file paths
load.more.file.paths <- function(){
  #
  dir_IHME       <<- file.path("data-raw/IHME2018", "IHME_5q0_Mortality_estimates_2018.xlsx")
  #dir_IHME_2019 <<- file.path("data-raw/IHME2019", "IHME_GBD_2019_MORTALITY_1950_2019_5Q0_WSHOCK_Y2020M07D31.CSV")

  dir_IHME_sex   <<- file.path("data-raw/IHME2018","IHME_ProbabilityOfDeath_estimates_200130.csv")
  dir_wpp_Q5     <<- file.path("data-raw/WPP2019", "WPP2019_MORT_F01_2_Q5_BOTH_SEXES.xlsx")
  dir_wpp_IMR    <<- file.path("data-raw/WPP2019", "WPP2019_MORT_F01_1_IMR_BOTH_SEXES.xlsx")
  dir_wpp_female <<- file.path("data-raw/WPP2019",  "WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx")
  dir_wpp_male   <<- file.path("data-raw/WPP2019",  "WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx")

  check.file.exist <- function(file0){
    if(!file.exists(file0)) warning("YL: Check if these required files are in the right place: ", file0)
  }
  sapply(c(dir_IHME, dir_IHME_sex,
           dir_wpp_Q5, dir_wpp_IMR, dir_wpp_female, dir_wpp_male), check.file.exist)
  return(NULL)
}
load.more.file.paths()

# Under-5
# U5MR
u5mr.wpp.cqt.2019 <- get.wpp.cqt(dir_wpp_Q5 = dir_wpp_Q5, dir_wpp_IMR = dir_wpp_IMR)
u5mr.ihme.cqt.2017 <- get.ihme.cqt(dir_IHME0 = dir_IHME)

usethis::use_data(u5mr.wpp.cqt.2019)
usethis::use_data(u5mr.ihme.cqt.2017)

# IMR
imr.wpp.cqt.2019 <-  get.wpp.cqt(dir_wpp_Q5 = dir_wpp_Q5,
                                 dir_wpp_IMR = dir_wpp_IMR,
                                 ind_name = "IMR")
# dir_IHME_sex contains IMR for both sex too. But there is no UI
imr.ihme.cqt.2017 <- get.sex.ihme.cqt(ind_name = "IMR",
                                      gender0 = "both",
                                      iso_order = u5mr.iso.c)
usethis::use_data(imr.wpp.cqt.2019)
usethis::use_data(imr.ihme.cqt.2017)

# NMR (IHME alone)
nmr.ihme.cqt.2017 <- get.ihme.cqt(ind_name = "NMR")
usethis::use_data(nmr.ihme.cqt.2017)

# Sex-specific ----
# Run directly in the script. The following are examples:
dir_wpp_female <- file.path("data-raw/WPP2019", "WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx")
dir_wpp_male <- file.path("data-raw/WPP2019", "WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx")
dir_IHME_sex <- file.path("data-raw/IHME2018", "IHME_ProbabilityOfDeath_estimates_200130.csv")
wpp_f <- read.wpp.v3(dir_wpp_female)
wpp_m <- read.wpp.v3(dir_wpp_male)
# for example
Q5_f_wpp.cqt <- get.sex.wpp.cqt(ind0 = "Q5", gender = "f")
Q5_f_ihme.cqt <- get.sex.ihme.cqt(ind_name = "Q5", gender0 = "f")
