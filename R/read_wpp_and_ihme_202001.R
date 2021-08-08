# Functions to create/read wpp and ihme cqt
# Yang Liu
# 1/3/2020


# Read WPP -------------------------------------------------------------

#' Read WPP xlsx files
#'
#' read from xlsx files: WPP2019_MORT_F01_2_Q5_BOTH_SEXES.xlsx, WPP2019_MORT_F01_1_IMR_BOTH_SEXES.xlsx
#' This is the wide format dataset, columns like "1950-1955", ..., "2015-2020"
#'
#' @param dir_wpp dir to WPP files
#' @param ind  Q5, IMR
read.wpp.file <- function(dir_wpp, ind){
  wpp <- setDT(readxl::read_xlsx(dir_wpp, skip = 16))
  col_years <- grep("-", colnames(wpp), value = TRUE)
  col_wanted <- c("Country code", col_years)
  wpp_dt <- setDT(wpp)[,..col_wanted]
  setnames(wpp_dt, c("UNCode", substr(col_years, 1, 4)))
  wpp_long <- data.table::melt.data.table(wpp_dt, id.vars = "UNCode",
                              variable.name = "year", variable.factor = FALSE)
  wpp_long <- wpp_long[, lapply(.SD, as.numeric), by = .SD]
  # adjust year to match 1953 1958 1963 1968 1973 1978 1983 1988 1993 1998 2003 2008 2013 2018
  wpp_long[, year:=year + 3]
  wpp_long[, ind:=ind]
  return(wpp_long)
}

#' Read the two WPP xlsx files for U5MR and IMR into one dataset
#'
#' @param dir_wpp_Q5 dir to "WPP2019_MORT_F01_2_Q5_BOTH_SEXES.xlsx"
#' @param dir_wpp_IMR dir to "WPP2019_MORT_F01_1_IMR_BOTH_SEXES.xlsx"
read.wpp.v2 <- function(dir_wpp_Q5, dir_wpp_IMR){
  suppressWarnings(
  wpp_list <- mapply(read.wpp.file, dir_wpp = c(dir_wpp_Q5, dir_wpp_IMR), ind = c("Q5", "IMR"), SIMPLIFY = FALSE)
  )
  wpp_long <- data.table::rbindlist(wpp_list)
  wpp_dt <- data.table::dcast.data.table(wpp_long, UNCode + year ~ ind, value.var = "value")
  setnames(wpp_dt, "Q5", "U5MR")
  return(wpp_dt)
}
# wpp_dt1 <- read.wpp.v1(dir_wpp= dir_wpp_new)
# UNCode year     IMR      U5MR
# 1:    108 1950 169.825 286.357
# 2:    108 1951 168.932 284.860
# wpp_dt2 <- read.wpp.v2(dir_wpp_Q5, dir_wpp_IMR)
# UNCode year       IMR        U5MR
# 1:      4 1953 275.86600 405.09000
# 2:      4 1958 253.64700 374.13800


#' Read from WPP life table: seperate files for male / female
#'
#'  The XLSX files are:
#' WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx
#' WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_MALE.xlsx
#'
#' @param dir_wpp_LT dir to WPP life table
read.wpp.v3 <- function(dir_wpp_LT){
  wpp <- setDT(readxl::read_xlsx(dir_wpp_LT, skip = 16))
  wpp[, x_n := paste(`Age (x)`, `Age interval (n)`, sep = "_")]
  wpp <- wpp[x_n %in% c("0_1", "1_4"),]
  setnames(wpp, c("Country code", "Probability of dying q(x,n)"), c("UNCode", "qxn"))
  # P(Dying)
  wpp[, qxn := as.numeric(qxn)]
  wpp <- wpp[,.(UNCode, Period, x_n, qxn)]
  wpp_wide <- data.table::dcast.data.table(wpp, UNCode + Period ~ x_n, value.var = "qxn")

  # Calculate IMR and Q5 from q(0,1) and q(1,4)
  get.U5MR <- function(q01, q14) (1-(1-q01)*(1-q14)) * 1000
  wpp_wide[, IMR:=`0_1`*1000]
  wpp_wide[, U5MR:= get.U5MR(q01 = `0_1`, q14 = `1_4`)]

  # adjust year to match 1953 1958 1963 1968 1973 1978 1983 1988 1993 1998 2003 2008 2013 2018
  wpp_wide[, year:=as.numeric(substr(Period, 1,4)) + 3]
  return(wpp_wide[,.(UNCode, year, IMR, U5MR)])
}


#' Function to get `wpp.cqt` for all indicators for total sex
#'
#' @param ind name of the indicator: Q5, Q1, or U5MR, IMR
#' @param wpp_dt WPP dt, if supply, ignore dir_wpp. Used for obtaining
#'   sex-specific WPP
#' @param new_cnames0 `new_cnames`
#' @param dir_wpp_Q5 dir to WPP U5MR
#' @param dir_wpp_IMR dir to WPP IMR
#' @param iso_order the iso order from mcmc.meta that we want to match
#'
#' @return wpp.cqt
get.wpp.cqt <- function(
  wpp_dt = NULL,
  dir_wpp_Q5 = dir_wpp_Q5,
  dir_wpp_IMR = dir_wpp_IMR,
  ind = "U5MR", # default to read U5MR, could be Q5 / U5mR or Q1 / IMR
  iso_order = u5mr.iso.c,
  new_cnames0 = new_cnames
){
  #
  ind_vector <- c("Q5", "Q1")
  ind_vector2 <- c("U5MR", "IMR", "10q5", "10q15")
  new_list <- list("Q5" = "U5MR", "Q1" = "IMR")
  if(ind%in%ind_vector) ind <- get.match(ind, new_list = new_list)
  if (!ind %in% ind_vector2) stop("`ind` should be among ",
                                                 paste(c(ind_vector, ind_vector2), collapse = ", "))
  if(is.null(wpp_dt)) wpp_dt <- read.wpp.v2(dir_wpp_Q5, dir_wpp_IMR)
  setkey(wpp_dt, UNCode)
  setDT(new_cnames0)
  setkey(new_cnames0, UNCode)
  # not all the wpp are in our 195 list, so fill NA to the rest
  # 184 isos co-exist
  wpp_dt_iso <- new_cnames0[,.(ISO3Code, UNCode)][wpp_dt, nomatch = 0][, c("ISO3Code", "year", ind), with = FALSE]
  wpp_dt_iso <- wpp_dt_iso[ISO3Code%in%iso_order]
  message("wpp_dt_iso contains ", wpp_dt_iso[,uniqueN(ISO3Code)], " countries.")
  years <- unique(wpp_dt_iso[ ,year]) # 14 years interval
  ISO_missing <-  iso_order[!iso_order%in%wpp_dt_iso$ISO3Code] # 11 isos, in total 195
  wpp_dt_NA <- expand.grid(ISO3Code = ISO_missing, year = years)
  wpp_dt_iso <- rbind(wpp_dt_iso, wpp_dt_NA, fill = T)
  # maybe not the best way, not easy , manually match the iso order, as reordering array with one dimension = 1 removes that dimension
  wpp_dt_iso <- wpp_dt_iso[order(match(ISO3Code, rep(iso_order, each = length(years))))]
  setorder(wpp_dt_iso, year) # set the right order is the key to produce right array
  # melt into array
  wpp.cqt <- array(data = wpp_dt_iso[[ind]],
                      dim = c(length(iso_order),
                              1,
                              length(years)),
                      dimnames = list(iso_order,
                                      "0.5",
                                      years))
  return(wpp.cqt)
}


#' Get sex-specific wpp cqt using prepared data `wpp_2019_cqt_by_sex`
#'
#' @param ind0 indicator
#' @param sex0 "f" or "m"
#' @param year default to 2019
#' @return a wpp_cqt file
#' @export get.sex.wpp.cqt
get.sex.wpp.cqt <- function(ind0,
                            sex0,
                            year = 2019
                            ){
  if (!sex0%in%c("f", "m")) stop("sex is either f or m")

  if(year == 2019){
    dt_wpp <- wpp_2019_cqt_by_sex # created in "data-raw/2.Get_WPP_IHME_cqt.R"
  } else {
    stop("only year 2019 WPP is available for now")
  }
  dt_wpp <- if(sex0 == "f") dt_wpp[sex=="f"] else dt_wpp[sex=="m"]
  wpp_cqt <- get.wpp.cqt(wpp_dt = dt_wpp, ind = ind0, iso_order = sexspecific.iso.c)
  return(wpp_cqt)
}





# Read IHME ---------------------------------------------------------------

#' Get IHME cqt 2017 for both sex, not sex-specific, for under-5 indicators
#'
#' Using the "IHME 5q0 Mortality_estimates 2018 latest version.xlsx": this
#' workbook has multiple sheets for both sex only: U5MR, IMR, NMR, Ratio.
#' U5MR and IMR have lower, mean and upper, NMR only has mean
#'
#'
#' @param dir_IHME0 IHME file directory to "IHME 5q0 Mortality_estimates 2018
#'   latest version.xlsx"
#' @param iso_order the iso order we want
#' @param ind indicator name: "U5MR", "IMR", "NMR"
#'
#' @return ihme.cqt
get.ihme.cqt.2017 <- function(
  dir_IHME0 = dir_IHME_2017,
  iso_order = u5mr.iso.c, # same order for them
  ind = "U5MR" # sheetname, accepts "U5MR", "IMR", "NMR"
  ){

  ind_vector <- c("Q5", "Q1") # also accepted
  ind_vector2 <- c("U5MR", "IMR", "NMR")
  new_list <- list("Q5" = "U5MR", "Q1" = "IMR")
  ind <- toupper(ind)
  if(ind%in%ind_vector) ind <- get.match(ind, new_list = new_list)
  if (!ind %in% ind_vector2) stop("`ind` should be among ",
                                       paste(c(ind_vector, ind_vector2), collapse = ", "))

  ihme <- setDT(readxl::read_xlsx(dir_IHME0, sheet = ind, na = "NA"))
  ihme$mean=ihme$mean*1000
  ihme$lower=ihme$lower*1000
  ihme$upper=ihme$upper*1000
  setkey(IHME_codebook, Location_ID)
  setkey(ihme, location_id)
  ihme.cqt <- get.ihme.cqt(ihme, iso_order)
  return(ihme.cqt)
}


#' Input data for sex-specific IHME 2017, used by `get.sex.ihme.cqt.2017`
#'
#' For IHME 2017: use the download data (2020/01/30) from
#' "https://vizhub.healthdata.org/mortality/results" to compile
#' "IHME_ProbabilityOfDeath_estimates_200130.csv", which is a long-formatted
#' dataset with sex: Both, Male, Female for "< 5 years" & "< 1 year"
#'
#' @param dir_IHME0 IHME file directory to
#'   "IHME_ProbabilityOfDeath_estimates_200130.csv"
#' @return a dt of age_group {U5MR, IMR} and sex {both, f, m}
get_dt_IHME_2017_by_sex <- function(
  dir_IHME0 = dir_IHME_sex
){
  ihme <- data.table::fread(dir_IHME0)
  ihme$sex <- as.factor(ihme$sex)
  ihme$sex <- factor(ihme$sex, levels = c("Both", "Female", "Male"))
  levels(ihme$sex) <- c("both", "f", "m")
  ihme$age_group <- as.factor(ihme$age_group)
  levels(ihme$age_group) <- c("IMR", "U5MR")
  return(ihme)
}


#' Get IHME cqt file for plotting based on the sex-specific version on download
#' IHME data for IHME 2017(2018)
#'
#' @param ihme ihme data
#' @param ind0 choose from U5MR, IMR (no NMR in this dataset)
#' @param sex0 choose from "f", "m", "both"
#'
#' @export get.sex.ihme.cqt.2017
get.sex.ihme.cqt.2017 <- function(
  ihme = ihme_2017_sex_Q5Q1_noCI,
  ind0 = "U5MR",
  sex0 = "f"
){
  ind_vector <- c("Q5", "Q1") # also accepted
  ind_vector2 <- c("U5MR", "IMR")
  new_list <- list("Q5" = "U5MR", "Q1" = "IMR")
  ind0 <- toupper(ind0)
  if(ind0%in%ind_vector) ind0 <- get.match(ind0, new_list = new_list)
  if (!ind0 %in% ind_vector2) stop("`ind0` should be among ",
                                       paste(c(ind_vector, ind_vector2), collapse = ", "))
  if (!sex0%in%c("f", "m", "both")) stop("sex is among f, m, and both")
  iso_order <- if(sex0=="both") u5mr.iso.c else sexspecific.iso.c
  # subsetting
  ihme2 <- data.table::copy(ihme)[age_group == ind0 & sex == sex0]
  #
  ihme2$mean=ihme2$mean*1000
  ihme2$upper <- NA_real_
  ihme2$lower <- NA_real_ # fill in since there is no CI in data
  ihme.cqt <- get.ihme.cqt(ihme2, iso_order)
  return(ihme.cqt)
}

#' Reorganize IHME 2019 dt
#'
#' @param ihme can supply ihme dataset following the same structure
#' @param dir_IHME0 IHME file directory to "GBD2019_Under5_estimates.xlsx"
#' @param ind sheet name in "GBD2019_Under5_estimates.xlsx"
#'
get_dt_IHME_2019 <- function(ihme = NULL,
                             dir_IHME0 = dir_IHME_2019,
                             ind = NULL){

  # read in data
  if(is.null(ihme)) ihme <- setDT(readxl::read_xlsx(dir_IHME0, sheet = ind))
  if("sex_name" %in% colnames(ihme)) setnames(ihme, "sex_name", "sex")
  setnames(ihme, c("year_id", "val"), c("year", "mean"), skip_absent = TRUE)
  ihme$sex <- as.factor(ihme$sex)
  if(identical(levels(ihme$sex), c("both", "female", "male"))){
    ihme$sex <- factor(ihme$sex, levels = c("both", "female", "male"))
    levels(ihme$sex) <- c("both", "f", "m")
  }
  # subsetting
  ihme[, sex:=tolower(sex)] # "Both" -> "both"
  ihme$mean <- ihme$mean*1000
  #
  if("upper"%in%colnames(ihme)){
    ihme$upper <- ihme$upper*1000
  } else {
    ihme$upper <- NA_real_
  }
  if("lower"%in%colnames(ihme)){
    ihme$lower <- ihme$lower*1000
  } else {
    ihme$lower <- NA_real_
  }
  if("age_group_name" %in% colnames(ihme)){
    ihme[, ind:=age_group_name]
  }
  ihme <- ihme[,.(location_id, year, sex, lower, mean, upper, ind)]

  if(!is.null(ind)){
    ihme[, ind := ind]
    if(ind == "ratio") ihme$mean <- ihme$mean/1000
  }
  return(ihme)
}

#' Get IHME cqt file 2019
#'
#' Using the processed data/get_dt_IHME_2019. There is no sex-specific NMR so
#' choosing `ind = NMR` and `sex = "both"` returns NULL
#'
#' @param ihme ihme_2019 data made using `get_dt_IHME_2019`
#' @param iso_order the iso order we want in the cqt output file
#' @param ind0 choose from U5MR, IMR, NMR, CMR, Ratio
#' @param sex0 choose from "f", "m", "both"
#'
#' @export get.ihme.cqt.2019
get.ihme.cqt.2019 <- function(
  ihme = ihme_2019,
  iso_order = NULL, # follows a different iso order
  ind0 = "U5MR",
  sex0 = "f"
){
  ind_vector1 <- c("U5MR", "IMR", "NMR", "CMR",
                   "10q15", "10q5",  "5q10",  "5q15",  "5q20",  "5q5"
                   ) # available sheets
  ind_vector2 <- c("Q5", "Q1", "Q4") # also accepted
  new_list <- list("Q5" = "U5MR", "Q1" = "IMR", "Q4" = "CMR")
  if(ind0%in%ind_vector2) ind0 <- get.match(ind0, new_list = new_list)
  if (!ind0 %in% ind_vector1) stop("`ind0` should be among ",
                                       paste(c(ind_vector1), collapse = ", "))
  if (!sex0%in%c("f", "m", "both")) stop("sex0 is among f, m, and both")

  # assign iso order by sex if not specified
  if(is.null(iso_order)) iso_order <- if(sex0=="both") u5mr.iso.c else sexspecific.iso.c

  ihme2 <- data.table::copy(ihme)[sex == sex0 & ind == ind0]
  if(nrow(ihme2)==0)return(NULL)
  ihme.cqt <- get.ihme.cqt(ihme2, iso_order)
  return(ihme.cqt)
}


#' get cqt from organized subsetted ihme data
#'
#' @param ihme the ihme dataset, with 5 columns: location_id, year, lower, mean,
#'   upper
#' @param iso_order the desired iso order
get.ihme.cqt <- function(
  ihme,
  iso_order
  ){
  if(!all(c("location_id", "year", "lower", "mean", "upper")%in%colnames(ihme))){
    stop("In get.ihme.cqt: request columns to be: location_id, year, lower, mean, upper")
  }
  setkey(IHME_codebook, Location_ID)
  setkey(ihme, location_id)
  # join ISO3Code
  ihme_dt_iso <- ihme[,.(location_id, year, lower, mean, upper)][IHME_codebook[,.(Location_ID, ISO3Code)], nomatch = 0]
  # adjust year
  ihme_dt_iso[, year:=year + 0.5]


  years <- unique(ihme_dt_iso[ ,year]) # 68 years interval
  ISO_missing <-  iso_order[!iso_order%in%ihme_dt_iso$ISO3Code] # 11 isos, in total 195
  ihme_dt_NA <- expand.grid(ISO3Code = ISO_missing, year = years)
  ihme_dt_iso <- rbind(ihme_dt_iso, ihme_dt_NA, fill = T)
  suppressWarnings(ihme_dt_long <- data.table::melt.data.table(ihme_dt_iso, measure.vars = c("lower", "mean", "upper")))  # match the ISO order, order by ISO
  ihme_dt_long <- ihme_dt_long[order(match(ISO3Code, rep(iso_order, each = length(years))))]
  setorder(ihme_dt_long, year, variable) # set the right order is the key to produce right array
  # Now the order is by t (year), q, and c (iso)
  # melt into array
  ihme.cqt <- array(data = ihme_dt_long[, value],
                    dim = c(length(iso_order),
                            3,
                            length(years)),
                    dimnames = list(c = iso_order,
                                    q = c(0.05, 0.5, 0.95),
                                    t = years))
  return(ihme.cqt)
}


#' Not needed anymore:
#' Revise IHME country names, match to `OfficialName` (not `CountryName`)
#' @param ihme_country ihme$country
get.match.IHME <- function(ihme_country){

  ihme_revised_country_names <- list(
    "The Bahamas" = "Bahamas",
    "Russia" = "Russian Federation",
    "Saint Vincent & the Grenadines" = "St Vincent & the Grenadines",
    "Palestine" = "State of Palestine",
    "Bolivia" = "Bolivia (Plurinational State of)",
    "Brunei" = "Brunei Darussalam",
    "Cape Verde" = "Cabo Verde",
    "Czech Republic" = "Czechia",
    "Federated States of Micronesia" = "Micronesia (Federated States of)",
    "The Gambia" = "Gambia",
    "Iran" = "Iran (Islamic Republic of)",
    "Laos" = "Lao People's Democratic Republic",
    "Macedonia" = "Republic of North Macedonia",
    "North Korea" = "Democratic People's Republic of Korea",
    "South Korea" = "Republic of Korea",
    "Swaziland" = "Eswatini",
    "Syria" = "Syrian Arab Republic",
    "Tanzania" = "United Republic of Tanzania",
    "Moldova" = "Republic of Moldova",
    "United States" = "United States of America",
    "Venezuela" = "Venezuela (Bolivarian Republic of)",
    "Vietnam" = "Viet Nam"
  )

  get.match(ihme_country, new_list = ihme_revised_country_names)
}



# Old version by Kai
# original function by Kai to read in WPP and IHME data
# revised, also OK to use.
#
# read_wpp_and_ihme <- function(
#   iso_order = u5mr.iso.c,
#   new_cnames,
#   ind = "Q5", # U5MR
#   wpp = NULL,
#   completeihme = NULL,
#   year.end =  last.year()
#   ){
#
#   #WPP part
#   uncode_iso = new_cnames[,c("UNCode", "ISO3Code")]
#
#   if(!is.null(wpp)){
#     wppmerged=merge(uncode_iso, wpp, by.x="UNCode", by.y="LocID", all.x=TRUE)
#     wppmerged=subset(wppmerged, MidPeriod <= year.end + 0.5)
#     # fill wpp.cqt
#     wpp.cqt=array(dim=c(length(iso_order), 1, length(seq(min(wppmerged$MidPeriod),year.end+0.5,5))),
#                   dimnames = list(iso_order, c(0.5) ,seq(min(wppmerged$MidPeriod),year.end+0.5,5)))
#
#     for(c in 1:length(iso_order)){
#       wppselected=subset(wppmerged[order(wppmerged$MidPeriod),], ISO3Code.x==iso_order[c])
#       yearlocation=which(wppselected$MidPeriod %in% seq(min(wppmerged$MidPeriod),year.end+0.5,5))
#       for(i in 1:length(yearlocation)){
#         wpp.cqt[c,1,yearlocation[i]]=wppselected[[ind]][i]
#       }
#     }
#   } else {
#     wpp.cqt=NULL
#   }
#   ####ihme estimates part
#   if(!is.null(completeihme)){
#     ihme=as.data.frame(completeihme)
#     ihme$country=as.character(ihme$location)
#     ihme$mean=ihme$mean*1000
#     ihme$lower=ihme$lower*1000
#     ihme$upper=ihme$upper*1000
#     minyear=min(as.numeric(ihme$year))+0.5
#     maxyear=max(as.numeric(ihme$year))+0.5
#     #ihme[,2:ncol(ihme)]=as.data.frame(apply(ihme[,2:ncol(ihme)],2,function(x) as.numeric(x)))
#     ihme$country=gsub(" and ", " & ", ihme$country)
#     ###change some countries name
#     ihme$country[which(ihme$country==c("The Bahamas"))]=c("Bahamas")
#     ihme$country[which(ihme$country==c("Democratic Republic of the Congo"))]=c("Congo DR")
#     ihme$country[which(ihme$country==c("Cote d'Ivoire"))]=c("Cote d Ivoire")
#     ihme$country[which(ihme$country==c("The Gambia"))]=c("Gambia The")
#     ihme$country[which(ihme$country==c("North Korea"))]=c("Korea DPR")
#     ihme$country[which(ihme$country==c("South Korea"))]=c("Korea Rep")
#     ihme$country[which(ihme$country==c("Laos"))]=c("Lao PDR")
#     ihme$country[which(ihme$country==c("Russia"))]=c("Russian Federation")
#     ihme$country[which(ihme$country==c("Saint Vincent & the Grenadines"))]=c("St Vincent & the Grenadines")
#     ihme$country[which(ihme$country==c("Palestine"))]=c("State of Palestine")
#     ihme$country[which(ihme$country==c("Timor-Leste"))]=c("Timor Leste")
#     ihme$country[which(ihme$country==c("United States"))]=c("United States of America")
#     countrylist = new_cnames[, c("ISO3Code", "OfficialName")]
#     colnames(countrylist) <- c("iso", "country")
#     ihmefinal=merge(ihme,countrylist,by="country",all.y=TRUE)
#     ihmefinal=subset(ihmefinal, iso!="LIE")
#     yearspan=maxyear-minyear+1
#     ihme.cqt=array(dim=c(195,3,yearspan),
#                    dimnames = list(iso_order, c(0.05,0.5,0.95), c(seq(minyear,maxyear,1))))    ####create array of data to save estimates
#     for(c in 1:195){
#       iso.selected=as.character(iso_order[c])
#       ihme.selected=ihmefinal[ihmefinal$iso==iso.selected,]
#       ihme.selected=ihme.selected[order(ihme.selected$year),]     #####order data by year
#       ihme.cqt[c,1,]=ihme.selected$lower
#       ihme.cqt[c,2,]=ihme.selected$mean
#       ihme.cqt[c,3,]=ihme.selected$upper
#     }
#   } else {
#     ihme.cqt=NULL
#   }
#   return(list(wpp.cqt=wpp.cqt, ihme.cqt=ihme.cqt))
# }
