# Functions to create/read wpp and ihme cqt
# Yang Liu
# 2022/07


# Read WPP -------------------------------------------------------------


#' Get WPP cqt by indicator and sex (2022 version)
#'
#' @param ind0 indicator, choose from "U5MR", "IMR", "CMR", "5q5", "5q10",
#'   "5q15", "5q20", "10q5", "10q15"
#' @param sex0 "f" or "m" or "both"
#' @param iso_order isos to extract in order
#' @param WPP_round default to 2022
#'
#' @return a wpp_cqt file
#' @export get.wpp.cqt
get.wpp.cqt <- function(ind0,
                        sex0 = "both",
                        iso_order = u5mr.iso.c,
                        WPP_round = 2022
  ){
  stopifnot(WPP_round%in%c(2019, 2022))
  stopifnot(sex0%in%c("f", "m", "both"))
  stopifnot(ind0%in%c("U5MR", "IMR", "CMR", "5q5", "5q10", "5q15", "5q20", "10q5", "10q15"))
  dt_wpp <- switch(as.character(WPP_round),
    "2022" = dt_wpp_2022,
    "2019" = dt_wpp_2019
  )

  dt_wpp_sub <- dt_wpp[ISO3Code %in% iso_order & Sex==sex0, ]
  years <- sort(unique(dt_wpp_sub[ ,Year]))

  # just in case iso_order has ISOS outside dt_wpp, which should be none in 2022
  ISO_missing <-  iso_order[!iso_order%in%dt_wpp_sub$ISO3Code]
  if(length(ISO_missing)!=0) message("Notice isos in iso_order not in WPP:", paste(ISO_missing, collapse = ","))
  wpp_dt_NA <- expand.grid(ISO3Code = ISO_missing, Year = years)
  dt_wpp_sub <- rbind(dt_wpp_sub, wpp_dt_NA, fill = TRUE)

  dt_wpp_sub <- dt_wpp_sub[order(match(ISO3Code, rep(iso_order, each = length(years))))]
  setorder(dt_wpp_sub, Year) # set the right order is the key to produce right array
  # melt into array
  wpp.cqt <- array(data = dt_wpp_sub[[ind0]],
                   dim = c(length(iso_order),
                           1,
                           length(years)),
                   dimnames = list(iso_order,
                                   "0.5",
                                   years))
  return(wpp.cqt)
}
# e.g.
# get.wpp.cqt("U5MR", "f", u5mr.iso.c, 2022)[1,,]



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
