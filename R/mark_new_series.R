# functions used to mark new entered series for U5MR, IMR, 5-14 datasets
# 2020.02
# UNICEF (YL)


#' Get last October e.g. "2019-10-01" in 2020
#'
#' For marking new entered series, based on system date
#'
#' @export last.October
last.October <- function() {
  as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-1), "10", "01", sep = "-"))
}

#' Get the cmeinfo file directory from mcmc.meta
#'
#' Search all the subfolders in `dir_IGME/input` since the file might be in
#' "old/"
#' @param output.dir used to load mcmc.meta, and obtain
#'   `mcmc.meta$files$data.cmeinfo.file`
#' @param dir_IGME the folder to search
get.cmeinfo.dir <- function(output.dir, dir_IGME){
  check.rda.exist(output.dir, mcmc.only = TRUE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  cmeinfo.file <- sub(".*/", "", mcmc.meta$files$data.cmeinfo.file)
  message("The info.file found is, ", cmeinfo.file)
  n <- which(grepl(cmeinfo.file, list.files(file.path(dir_IGME, "input"), recursive = TRUE)))
  dir_cme_info <- list.files(file.path(dir_IGME, "input"), recursive = TRUE, full.names = TRUE)[n]
  message("The cmeinfo file points to: ", dir_cme_info)
  # returns the directory to
  return(dir_cme_info)
}

#' Review the entry date, output a table by year
#'
#' @param output.dir if provided, will by default use "data_CMEInfo.csv" in the output.dir
#' @param dir_IGME if provided, will search dir_IGME for the specific CMEinfo file recorded in mcmc.meta
#' @param dir_file if the file directory is provided, use it directly, this is the most prioritizing
#' @export review.date.of.dataentry
review.date.of.dataentry <- function(output.dir, dir_IGME = NULL, dir_file = NULL){
  if(!is.null(dir_file)) {
    dir_cmeinfo <- dir_file
  } else {

    # read in (search for) the "data_CMEInfo.csv
    if(is.null(dir_IGME) & file.exists(file.path(output.dir, "data_CMEInfo.csv"))){
      # by default look in the output.dir for "data_CMEInfo.csv"
      dir_cmeinfo <- file.path(output.dir, "data_CMEInfo.csv")
    } else if (!is.null(dir_IGME)){
      # otherwise search all subfolders
      dir_cmeinfo <- get.cmeinfo.dir(output.dir, dir_IGME)
    } else {
      stop("Please provide dir_IGME as there is no data_CMEInfo.csv in output.dir. ")
    }

  }
  if(!file.exists(dir_cmeinfo)) stop("Check if cmeinfo file exists: ", dir_cmeinfo)
  message("The cmeinfo file points to: ", dir_cmeinfo)
  dt_cme <- fread(dir_cmeinfo)
  dt_cme[, Date.Of.Data.Added2:= as.Date(paste0(sub("-", "", Date.Of.Data.Added), "01"), format = "%Y%m%d")]
  message("A table showing the number of rows by `Date.Of.Data.Added`: ")
  table(dt_cme$Date.Of.Data.Added2, useNA = "ifany")
}

#' Create sourceID to match U5MR and IMR `mcmc.meta` array
#'
#' Only needed for U5MR and IMR
#'
#' @param data dt_cme
get.new.sourceID.i <- function(data = dt_cme){
  # based on cleandata.R
  country.i <- StandardiseCountryNames(data$Country.Name)
  seriesyear.i <- data$Series.Year
  source.i <- data$Series.Name
  method.i <- ifelse(data$Series.Type == "Life Table", "Life Table",
                     ifelse(data$Data.Collection.Method == "Household Deaths", "Household Deaths",
                            ifelse(data$Series.Type == "Indirect", "Indirect",
                                   "Direct"))) # Direct: Direct+VR
  sourcename.i <- paste(country.i, source.i, seriesyear.i, sep = "_")
  sourceID.i <- paste(sourcename.i, method.i)
  data$sourceID.i <- sourceID.i
  data[new_entry==1, unique(sourceID.i)]
}

#' Check if the input is date
#' @param mydate date to check
#' @param date.format  default to YYYYMMDD
IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}

#' Create `new_entry` column in the dataset `dt_cme`
#' only mark by date in this step
#'
#' @param dt_cme cme dt object
#' @param new_entry_date must supply, cannot be NULL, format is checked, accept yyyy-mm-dd and yyyy-mm
#' @param show_WHO_VR_by_value default to TRUE: highlight WHO VR difference too
get.new.series.mark.entry <- function(dt_cme,
                                      new_entry_date,
                                      show_WHO_VR_by_value = TRUE
                                      ){
  # Check if the input date is valid: "YYYY-MM" is also allowed
  if(nchar(new_entry_date) == 7) new_entry_date <- as.Date(paste0(sub("-", "", new_entry_date), "01"), format = "%Y%m%d")
  if(!IsDate(new_entry_date)) stop("new_entry_date is: ", new_entry_date, " Allowed format: yyyy-mm-dd")

  setDT(dt_cme)
  dt_cme[, Date.Of.Data.Added2:= as.Date(paste0(sub("-", "", Date.Of.Data.Added), "01"), format = "%Y%m%d")]
  # hist(dt_cme$Date.Of.Data.Added2, breaks = uniqueN(dt_cme$Date.Of.Data.Added2))
  n_new <- dt_cme[Date.Of.Data.Added2>=new_entry_date ,.N]
  dt_cme[, new_entry := 0]
  dt_cme[Date.Of.Data.Added2 >= new_entry_date , new_entry := 1]
  # 2020/2/24:
  # turn off showing new WHO VR (as determined by date), so only surveys and nonWHO VR

  # 2/28: If to highlight part of the WHO Series by comparing values
  if(show_WHO_VR_by_value){
    dt_cme[grepl("WHO", Series.Name), new_entry := 0]
    iso_newVR <- get.diff.dt.WHOVR()$iso_newVR
    dt_cme[grepl("WHO", Series.Name) & Country.Code %in% iso_newVR, new_entry := 1]
  }
  dt_cme[, country_year:= paste0(IGME_Key, "_", Reference.Date)]
  return(dt_cme)
}

#' Read cmeinfo file from `output.dir` and return the vector of new entries:
#' `new.sourceID.i`
#'
#' Used for U5MR and IMR only, called in `savePlotResults`
#'
#' @param output.dir output.dir
#' @param new_entry_date must supply, cannot be NULL, format is checked, accept yyyy-mm-dd and yyyy-mm
#' @param dir_IGME default to NULL, where to read data_CMEInfo.csv
get.new.series <- function(output.dir, new_entry_date = NULL, dir_IGME = NULL){
  # by default, cut new entries as input after Oct.1st last year
  if(is.null(new_entry_date)) stop("Please supply new_entry_date, now it is NULL")
  # Check if the input date is valid: "YYYY-MM" is also allowed
  if(nchar(new_entry_date) == 7) new_entry_date <- as.Date(paste0(sub("-", "", new_entry_date), "01"), format = "%Y%m%d")
  if(!IsDate(new_entry_date)) stop("new_entry_date is: ", new_entry_date, " Allowed format: yyyy-mm-dd")

  # read in the cmeinfo file
  if(is.null(dir_IGME)&file.exists(file.path(output.dir, "data_CMEInfo.csv"))){
    # by default look in the output.dir for "data_CMEInfo.csv"
    dir_cmeinfo <- file.path(output.dir, "data_CMEInfo.csv")
  } else {
    dir_cmeinfo <- get.cmeinfo.dir(output.dir, dir_IGME)
  }
  if(!file.exists(dir_cmeinfo)) stop("Check if cmeinfo file exists: ", dir_cmeinfo)
  dt_cme <- fread(dir_cmeinfo)
  dt_cme <- get.new.series.mark.entry(dt_cme, new_entry_date) # this step adds the `new_entry` column
  #
  if (!"IGME_Key" %in% colnames(dt_cme)) dt_cme <- create.IGME.key(dt_cme)
  #
  message("Marked as newly added after date ", new_entry_date, ": ",
          dt_cme[new_entry == 1,.N], " out of ", dt_cme[,.N], " entries; which is ",
          dt_cme[new_entry == 1, uniqueN(IGME_Key)], " out of ", dt_cme[,uniqueN(IGME_Key)], " unique series.")

  # For the HIV countries, remove those just MM adjusted (since they have a new date)
  iso_hiv <- dt_cme[To.be.adjusted==TRUE, unique(Country.Code)]
  new_hiv_key <- dt_cme[new_entry==1 & To.be.adjusted==1, unique(IGME_Key)] # the TRUE new
  dt_cme[Country.Code%in%iso_hiv & new_entry==1 & !IGME_Key%in% new_hiv_key, new_entry := 0]
  #
  new.sourceID.i <- get.new.sourceID.i(dt_cme)
  return(new.sourceID.i)
}

#' Revise mcmc.meta and return a new mcmc.meta
#'
#' Used in `savePlotResults` to revise mcmc.meta for U5MR and IMR
#'
#' @param mcmc.meta mcmc.meta
#' @param HIV_removed if TRUE uses `mcmc.meta$data.hivremoved.all`
#' @param new.sourceID.i output from `get.new.series`
mark.new.series <- function(mcmc.meta, new.sourceID.i, HIV_removed = FALSE){
  data.all <- mcmc.meta$data.all
  if(HIV_removed) data.all <- mcmc.meta$data.hivremoved.all
  newentry.Lc.s <- newentryvr.Lc.s <- list()
  for(i in 1:length(data.all$iso.c)){
    # extract sourceid from data.all
    id <- data.all$sourceid.Lc.s[[i]]
    if(is.null(id)){
      newentry.Lc.s.i <- NA
    } else {
      newentry.Lc.s.i <- rep(0, length(id))
      newentry.Lc.s.i[id%in%new.sourceID.i] <- 1
    }
    newentry.Lc.s[[i]] <- newentry.Lc.s.i
    idvr <- data.all$sourceidvr.Lc.s[[i]]
    if(is.null(idvr)) {
      newentryvr.Lc.s.i <- NA
    } else {
      newentryvr.Lc.s.i <- rep(0, length(idvr))
      newentryvr.Lc.s.i[idvr%in%new.sourceID.i] <- 1
    }
    newentryvr.Lc.s[[i]] <- newentryvr.Lc.s.i
  }

  data.all$newentry.Lc.s <- newentry.Lc.s
  data.all$newentryvr.Lc.s <- newentryvr.Lc.s
  message("Preview for ZMB: ", paste(newentry.Lc.s[[190]], sep = ", "))
  message("Preview for ZMB: ", paste(newentryvr.Lc.s[[190]], sep = ", "))
  if(HIV_removed){
    mcmc.meta$data.hivremoved.all <- data.all
  } else {
    mcmc.meta$data.all <- data.all
  }

  return(mcmc.meta)
}


# For WHO VR --------------------------------------------------------------
#' Compare the WHO VR
#' Compare dt_new vs. dt_old
#'
#' @param dt_new The new dataset, dt object
#' @param dt_old The old dataset, dt object
get.diff.dt.WHOVR <- function(
  dt_new = NULL,
  dt_old = NULL
){
  if(is.null(dt_new)) dt_new <- fread(CME.assistant::get.dir_U5MR(dir_IGME = get.IGMEinput.dir(2020)))
  if(is.null(dt_old)) dt_old <- fread(file.path(CME.assistant::get.IGMEinput.dir(2019), "data_U5MR_20191018.csv"))

  subset.dt <- function(dt_new){
    dt_new_2 <- dt_new[grepl("WHO", Series.Name) & Visible == 1]
    # Series.Name2 is the new one
    dt_new_2[, Series.Name:=gsub(" version", "", Series.Name)]
    dt_new_2[, Series.Name:=gsub(" 2018| 2019| 2020", "", Series.Name)]
    dt_new_2[, key:=paste(Country.Code, Series.Name, Reference.Date, sep = "_")]

    setkey(dt_new_2, key)
    return(dt_new_2)
  }
  dt_new_2 <- subset.dt(dt_new)[,.(Country.Code, Country.Name, Series.Name, Reference.Date, key, Estimates)]
  dt_old_2 <- subset.dt(dt_old)[,.(key, Estimates)]
  # joined
  setnames(dt_old_2, "Estimates", "Estimates_19")
  setnames(dt_new_2, "Estimates", "Estimates_20")
  dt1 <- dt_old_2[dt_new_2]
  dt1[, diff:= Estimates_20 - Estimates_19]
  dt1[,pcnt:= (Estimates_20 - Estimates_19)/Estimates_19]
  dt_new <- dt1[is.na(Estimates_19),]
  iso_newVR <- dt_new[, unique(Country.Code)]
  # message(dt_diff[, uniqueN(Country.Code)], " out of ", dt_new_2[,uniqueN(Country.Code)],
  #         " have WHO VR U5MR estimate differences > ", cutoff, " comparing 2020 to 2019")
  return(list(dt1 = dt1, iso_newVR = iso_newVR))
}

