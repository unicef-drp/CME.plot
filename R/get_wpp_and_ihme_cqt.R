# Functions to format WPP and IHME GBD cqt from processed data
# one function for WPP and one for GBD
# script to process raw WPP and GBD data are saved in "R/CME.plot/data-raw"

# Yang Liu
# 2025/06


# Read WPP -------------------------------------------------------------


#'Get WPP cqt by indicator and sex (2025 version)
#'
#'Note that the iso order of the WPP cqt doesn't matter, (it only needs to have
#'named dimensions). In `savePlotResults` we will rematch iso order, by default
#'we use `u5mr.iso.c`, which should include all the countries that we wish to
#'see
#'
#'@param ind0 indicator, choose from "U5MR", "IMR", "CMR", "5q5", "5q10",
#'  "5q15", "5q20", "10q5", "10q15"
#'@param sex0 "f" or "m" or "both"
#'@param iso_order the order of isos in the cqt file
#'@param wpp_round default to 2025
#'
#'@return a wpp_cqt file
#'@export get.wpp.cqt
get.wpp.cqt <- function(ind0,
                        sex0 = "both",
                        iso_order = u5mr.iso.c,
                        wpp_round = 2025
  ){
  stopifnot(wpp_round%in%c(2019, 2022, 2025))
  stopifnot(sex0%in%c("f", "m", "both"))
  stopifnot(ind0%in%c("U5MR", "Q5", "IMR", "Q1", "CMR", "Q4", "5q5", "5q10", "5q15", "5q20", "10q5", "10q15"))
  dt_wpp <- switch(as.character(wpp_round),
    "2025" = dt_wpp_2024,
    "2022" = dt_wpp_2022,
    "2019" = dt_wpp_2019
  )
  new_list <- list("Q5" = "U5MR", "Q1" = "IMR", "Q4" = "CMR")
  ind0 <- get.match(ind0, new_list = new_list)
  dt_wpp_sub <- dt_wpp[ISO3Code %in% iso_order & Sex==sex0, ]
  dt_wpp_sub[, Year:= floor(Year) + 0.5]
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
                   dimnames = list(c = iso_order,
                                   q = "0.5",
                                   t = years))
  return(wpp.cqt)
}
# e.g.
# get.wpp.cqt("U5MR", "f", u5mr.iso.c, wpp_round = 2025)[1,,]



# Read IHME GBD---------------------------------------------------------------


#'Get GBD cqt by indicator and sex (2025 version)
#'
#'Note that the iso order of the gbd cqt doesn't matter, (it only needs to have
#'named dimensions). In `savePlotResults` we will rematch iso order, by default
#'we use `u5mr.iso.c`, which should include all the countries that we wish to
#'see
#'
#'@param ind0 indicator, choose from "U5MR", "IMR", "CMR", "5q5", "5q10",
#'  "5q15", "5q20", "10q5", "10q15"
#'@param sex0 "f" or "m" or "both"
#'@param iso_order the order of isos in the cqt file
#'
#'@return a gbd.cqt file
#'@export get.gbd.cqt
#'
get.gbd.cqt <- function(ind0,
                        sex0 = "both",
                        iso_order = u5mr.iso.c
){
  stopifnot(sex0%in%c("f", "m", "both"))
  stopifnot(ind0%in%c("NMR", "U5MR", "Q5", "IMR", "Q1", "CMR", "Q4", "5q5", "5q10", "5q15", "5q20", "10q5", "10q15", "Ratio"))
  dt_gbd <- dt_gbd_output
  new_list <- list("Q5" = "U5MR", "Q1" = "IMR", "Q4" = "CMR")
  ind0 <- get.match(ind0, new_list = new_list)
  dt_gbd_sub <- dt_gbd[ISO3Code %in% iso_order & Sex==sex0 & Shortind == ind0, .(ISO3Code, Year, Median, Lower, Upper)]
  dt_gbd_sub[, Year:= floor(Year) + 0.5]
  years <- sort(unique(dt_gbd_sub[ ,Year]))
  # just in case iso_order has ISOS outside dt_gbd, need to inject NA estimates
  ISO_missing <-  iso_order[!iso_order%in%dt_gbd_sub$ISO3Code]
  if(length(ISO_missing)!=0) message("Notice isos in iso_order not in GBD estimates:", paste(ISO_missing, collapse = ","))
  gbd_dt_NA <- expand.grid(ISO3Code = ISO_missing, Year = years)
  dt_gbd_sub_new <- rbind(dt_gbd_sub, gbd_dt_NA, fill = TRUE)
  dt_gbd_long <- data.table::melt.data.table(dt_gbd_sub_new,
                                             measure.vars = c("Lower", "Median", "Upper"),
                                             variable.name = "quintile")  # match the ISO order, order by ISO

  dt_gbd_long <- dt_gbd_long[order(match(ISO3Code, rep(iso_order, each = length(years)*3)))]
  setorder(dt_gbd_long, Year, quintile) # set the right order is the key to produce right array
  # melt into array
  gbd.cqt <- array(data = dt_gbd_long[, value],
                   dim = c(length(iso_order),
                           3,
                           length(years)),
                   dimnames = list(c = iso_order,
                                   q = c(0.05, 0.5, 0.95),
                                   t = years))
  return(gbd.cqt)
}
# e.g.
# get.gbd.cqt("U5MR", "f", u5mr.iso.c)[1,,]

