# functions used to mark new entered series for U5MR, IMR, 5-14 datasets
# 2020.02
# UNICEF


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
  if(length(dir_cme_info)==0) stop("There is no corresponding data_CMEInfo.csv. Cannot mark new series for this indicator.")
  message("The cmeinfo file points to: ", dir_cme_info)
  # returns the directory to
  return(dir_cme_info)
}

#' Review the entry date, output a table by year
#'
#' @param output.dir if provided, will by default use "data_CMEInfo.csv" in the output.dir
#' @param dir_IGME if provided, will search dir_IGME for the specific CMEinfo file recorded in mcmc.meta
#' @param dir_file if the file directory is provided, use it directly, this is the most prioritizing
#'
#' @return a list of two elements: a table and a vector of new series
#' @export review.date.of.dataentry
#'
review.date.of.dataentry <- function(output.dir, dir_IGME = NULL, dir_file = NULL
                                     ){
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
  return(table(dt_cme$Date.Of.Data.Added2, useNA = "ifany"))
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


#' Create `new_entry` column in the dataset `dt_cme` only mark by date in this
#' step
#'
#' @param dt_cme cme dt object
#' @param new_entry_date must supply, cannot be NULL, format is checked, accept
#'   yyyy-mm-dd and yyyy-mm
#' @param show_new_WHO_VR default to TRUE: mark `new_entry = 1` for WHO VR
#'   series that have new country-year data, obtained by
#'   \code{\link{get.diff.dt.WHOVR}}
#'
get.new.series.mark.entry <- function(dt_cme,
                                      new_entry_date,
                                      show_new_WHO_VR = TRUE
                                      ){
  # Check if the input date is valid: "YYYY-MM" is also allowed
  if(nchar(new_entry_date) == 7) new_entry_date <- as.Date(paste0(sub("-", "", new_entry_date), "01"), format = "%Y%m%d")
  if(!IsDate(new_entry_date)) stop("new_entry_date is: ", new_entry_date, " Allowed format: yyyy-mm-dd")

  setDT(dt_cme)
  dt_cme[, Date.Of.Data.Added2:= as.Date(paste0(substr(sub("-", "", Date.Of.Data.Added), 1, 6), "01"), format = "%Y%m%d")]
  dt_cme[is.na(Date.Of.Data.Added2), Date.Of.Data.Added2:= as.Date(Date.Of.Data.Added, format = "%m/%d/%Y")]
  # hist(dt_cme$Date.Of.Data.Added2, breaks = uniqueN(dt_cme$Date.Of.Data.Added2))
  n_new <- dt_cme[Date.Of.Data.Added2>=new_entry_date ,.N]
  dt_cme[, new_entry := 0]
  dt_cme[Date.Of.Data.Added2 >= new_entry_date , new_entry := 1]
  # 2020/2/24:
  # turn off showing new WHO VR (as determined by date), so only surveys and nonWHO VR

  # 2/28: If to highlight part of the WHO Series by comparing values
  if(show_new_WHO_VR){
    dt_cme[grepl("WHO", Series.Name), new_entry := 0]
    # iso_newVR <- get.diff.dt.WHOVR(count_rounding = NULL)$iso_newVR
    iso_newVR <- get.diff.dt.WHOVR(count_rounding = 1)$iso_newVR
    if(is.null(iso_newVR)) {
      message("New WHO VR series won't be highlighted, run `find.dir.for.VR.comparison()` to check the chosen databases.")
    } else {
      dt_cme[grepl("WHO", Series.Name) & Country.Code %in% iso_newVR, new_entry := 1]
    }
  }
  if(!"IGME_Key" %in% colnames(dt_cme)) dt_cme <- create.IGME.key(dt_cme)
  dt_cme[, country_year:= paste0(IGME_Key, "_", Reference.Date)]
  return(dt_cme)
}

#' Read cmeinfo file from `output.dir` and return the vector of new entries:
#' `new.sourceID.i`
#'
#' Used for U5MR and IMR only, called in `savePlotResults`
#'
#' @param output.dir output.dir
#' @param new_entry_date must supply, cannot be NULL, format is checked, accept
#'   yyyy-mm-dd and yyyy-mm
#' @param dir_database the file directory to database, default to NULL, if
#'   provided, it will be used instead of looking into `output.dir`
#' @param return_dt_cme default to FALSE, if TRUE, can return the dataset with
#'   new entries marked as 1
#' @param suppress_MM_adjustment default to TRUE: MM adjusted series not
#'   considered new
#'
#' @export get.new.series
get.new.series <- function(output.dir = NULL,
                           new_entry_date = NULL,
                           dir_database = NULL,
                           return_dt_cme = FALSE,
                           suppress_MM_adjustment = TRUE
                           ){
  if(is.null(new_entry_date)) stop("Please supply new_entry_date, now it is NULL")
  # Check if the input date is valid: "YYYY-MM" is also allowed
  if(nchar(new_entry_date) == 7) new_entry_date <- as.Date(paste0(sub("-", "", new_entry_date), "01"), format = "%Y%m%d")
  if(!IsDate(new_entry_date)) stop("new_entry_date is: ", new_entry_date, " Allowed format: yyyy-mm-dd")

  # read in the cmeinfo file
  if(is.null(dir_database)){
    # by default look in the output.dir for "data_CMEInfo.csv"
    dir_cmeinfo <- file.path(output.dir, "data_CMEInfo.csv")
  } else {
    dir_cmeinfo <- dir_database
  }
  if(!file.exists(dir_cmeinfo)) stop("Check if cmeinfo file exists: ", dir_cmeinfo)
  dt_cme <- fread(dir_cmeinfo)
  dt_cme <- get.new.series.mark.entry(dt_cme, new_entry_date) # this step adds the `new_entry` column
  #
  if (!"IGME_Key" %in% colnames(dt_cme)) dt_cme <- create.IGME.key(dt_cme)
  #
  # **** Suppressing MM-adjusted series (not shown as new) ****
  if(suppress_MM_adjustment){
    # For the HIV countries, remove those just MM adjusted (since they have a new date)
    # Limited this suppressing to `Series.Type` contains Direct, since only Direct series got MM-adjusted
    iso_hiv <- dt_cme[To.be.adjusted==TRUE, unique(Country.Code)]
    # the TRUE new MM-adjusted series:
    new_hiv_key <- dt_cme[new_entry==1 & To.be.adjusted==1, unique(IGME_Key)]
    # suppress these IGME_Key series:
    dt_cme[Country.Code%in%iso_hiv & new_entry==1 & !IGME_Key %in% new_hiv_key &
            grepl("Direct", Series.Type) & Data.Collection.Method != "Household Deaths",
            unique(IGME_Key)]
    dt_cme[Country.Code%in%iso_hiv & new_entry==1 & !IGME_Key %in% new_hiv_key &
            grepl("Direct", Series.Type) & Data.Collection.Method != "Household Deaths",
            new_entry := 0]
  }

  message("Marked as newly added after date ", new_entry_date, ": ",
          dt_cme[new_entry == 1,.N], " out of ", dt_cme[,.N], " country-years; which are ",
          dt_cme[new_entry == 1, uniqueN(IGME_Key)], " out of ", dt_cme[,uniqueN(IGME_Key)], " unique series.")

  # create source id
  new.sourceID.i <- get.new.sourceID.i(dt_cme)
  return(if(return_dt_cme) dt_cme else new.sourceID.i)
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
  # message("Preview for ZMB: ", paste(newentry.Lc.s[[190]], sep = ", "))
  # message("Preview for ZMB: ", paste(newentryvr.Lc.s[[190]], sep = ", "))
  if(HIV_removed){
    mcmc.meta$data.hivremoved.all <- data.all
  } else {
    mcmc.meta$data.all <- data.all
  }

  return(mcmc.meta)
}


# For WHO VR --------------------------------------------------------------

#' Only works on Dropbox: locate the directories to the dt_new and dt_old to
#' compare WHO VR to find out the countries (ISO3Code) new series for
#' highlighting new series, by default will pick the latest file from this and
#' last year's IGME Input folders
#'
#' @param IGME_year_new Year to look for the IGME `Input` dir
#' @param IGME_year_old Year to look for the IGME `Input` dir
#' @param filename_new if NULL will use the latest file judging by
#'   \code{get.dir_U5MR}
#' @param filename_old if NULL will use the latest file judging by
#'   \code{get.dir_U5MR}
#'
#' @return NULL
#' @export find.dir.for.VR.comparison
#' @examples
#' \dontrun{
#' find.dir.for.VR.comparison(IGME_year_new = 2020, IGME_year_old = 2019,
#' filename_old = "data_U5MR_20191018.csv")
#' }
find.dir.for.VR.comparison <- function(
  IGME_year_new = 2024, # Year to look for the IGME `Input` dir
  IGME_year_old = 2023,
  filename_new = NULL,
  filename_old = NULL # e.g. "data_U5MR_20191018.csv"
){
  if(floor(as.numeric(IGME_year_new)) >= 2024){
    workdir_new <- get.workdir.sharepoint(IGME_year_new)
  } else {
    workdir_new <- get.workdir(IGME_year_new)
  }

  if(floor(as.numeric(IGME_year_old)) >= 2024){
    workdir_old <- get.workdir.sharepoint(IGME_year_old)
  } else {
    workdir_old <- get.workdir(IGME_year_old)
  }

  # allow global overwriting if these object exist: dir_new_data_U5MR, dir_old_data_U5MR
  if(!exists("dir_new_data_U5MR")){
   if(is.null(filename_new)){
     dir_new_data_U5MR  <- get.dir_U5MR(workdir = workdir_new)
   } else {
     dir_new_data_U5MR <- file.path(workdir_new, "input", filename_new)
   }
   if(is.null(dir_new_data_U5MR))
     message("If want to highlight new VR series, please add in the global environment
      valid directories to the new U5MR datasets for value comparison:
      `dir_new_data_U5MR = ...` ")
  }

  if(!exists("dir_old_data_U5MR")){
    if(is.null(filename_old)){
      dir_old_data_U5MR <- get.dir_U5MR(workdir = workdir_old)
    } else {
      dir_old_data_U5MR <- file.path(workdir_old, "input", filename_old)
    }
    if(is.null(dir_old_data_U5MR))
      message("If want to highlight new VR series, please add in the global environment
      valid directories to the old U5MR datasets for value comparison:
      `dir_old_data_U5MR = ...`")
  }
  return(list(dir_new_data_U5MR = dir_new_data_U5MR, dir_old_data_U5MR = dir_old_data_U5MR))
}

#' Compare the WHO VR dt_new vs. dt_old, count only the countries with new years
#' of data because if we calculate the value differences, the differences could
#' be very small and it will depend on the rounding of the difference
#'
#' dir_new_data_U5MR and dir_old_data_U5MR can be loaded by
#' \code{\link{find.dir.for.VR.comparison}}
#'
#' Supply `dir_new_data_U5MR` or/and `dir_old_data_U5MR` in the global
#' environment to overwrite the default selection
#'
#' @param count_rounding default to NULL, if supply a value, e.g. 6, it will
#'   count the difference also using the diff round to 1E-6, if NULL only
#'   count new country-year
#'
#' @return list of dt1 (the comparison dataset for debugging) and iso_newVR (the
#'   vector of country isos with different WHO VR)
get.diff.dt.WHOVR <- function(
  count_rounding = NULL
){
  default_dir <- find.dir.for.VR.comparison()
  dir_new_data_U5MR <- default_dir$dir_new_data_U5MR
  dir_old_data_U5MR <- default_dir$dir_old_data_U5MR
  if(is.null(dir_new_data_U5MR)|is.null(dir_old_data_U5MR)){
    return(NULL)
  }
  if(!file.exists(dir_new_data_U5MR)){
    message("Check if dir_new_data_U5MR exists: ", dir_new_data_U5MR)
    return(NULL)
  }
  if(!file.exists(dir_old_data_U5MR)){
    message("Check if dir_old_data_U5MR exists: ", dir_old_data_U5MR)
    return(NULL)
  }

  dt_new <- fread(dir_new_data_U5MR)
  dt_old <- fread(dir_old_data_U5MR)
  subset.dt <- function(dt_new){
    dt_new_2 <- dt_new[grepl("WHO", Series.Name) & Visible == 1]
    # Series.Name2 is the new one
    dt_new_2[, Series.Name:=gsub(" version", "", Series.Name)]
    dt_new_2[, Series.Name:=gsub(paste(2018:2030, collapse  = "|"), "", Series.Name)]
    dt_new_2[, Series.Name:=trimws(Series.Name)]
    dt_new_2[, key:=paste(Country.Code, Series.Name, Reference.Date, sep = "_")]

    setkey(dt_new_2, key)
    return(dt_new_2)
  }
  dt_new_2 <- subset.dt(dt_new)[,.(Country.Code, Country.Name, Series.Name, Reference.Date, key, Estimates)]
  dt_old_2 <- subset.dt(dt_old)[,.(key, Estimates)]
  # joined
  setnames(dt_old_2, "Estimates", "Estimates_WHO_old")
  setnames(dt_new_2, "Estimates", "Estimates_WHO_new")
  dt1 <- dt_old_2[dt_new_2]

  if(!is.null(count_rounding)){
    if(count_rounding<1) message("count_rounding is the rounding digits")
    count_rounding <- as.integer(count_rounding)
    dt1[, diff:= abs(round(Estimates_WHO_new - Estimates_WHO_old, count_rounding))]
    dt_new <- dt1[ diff >0 | is.na(Estimates_WHO_old),]
  } else {
    dt_new <- dt1[is.na(Estimates_WHO_old),] # only count new year
  }

  iso_newVR <- dt_new[, unique(Country.Code)]
  # message(dt_diff[, uniqueN(Country.Code)], " out of ", dt_new_2[,uniqueN(Country.Code)],
  #         " have WHO VR U5MR estimate differences > ", cutoff, " comparing 2020 to 2019")
  return(list(dt1 = dt1, iso_newVR = iso_newVR))
}



# copy code ---------------------------------------------------------------

#' Copy R folder to Dropbox allowing direct loading the code without loading the
#' library. All paths are absolute paths.
#'
#' @export copy_code_to_dropbox
#' @return NULL
#'
copy_code_to_dropbox <- function(){

  username <- Sys.getenv("USERNAME")
  # revise the destination folders accordingly
  dir_of_destinations <- list(
    "Localcopy" = file.path("C:/Users/", username, "/Dropbox/UNICEF_Work_Project/2021_Code/R"),
    "IGME2020" = file.path("C:/Users/", username, "/Dropbox/UN IGME Data/2020 Round Estimation/Code/R/"),
    "IGME2021" = file.path("C:/Users/", username, "/Dropbox/UN IGME Data/2021 Round Estimation/Code/R/"),
    "NMR" = file.path("C:/Users/", username, "/Dropbox/NMR/code/functions/")
  )
  if(!any(sapply(dir_of_destinations, dir.exists)))stop("Check if all file path exists")
  # only existing paths will be written
  # dir_of_destinations <- dir_of_destinations[sapply(dir_of_destinations, dir.exists)]
  # The rest won't change

  # Code
  # CME_code_source <- file.path("C:/Users/", username, "/Dropbox/UNICEF Work/CME Plots/R")
  CME_code_source <- file.path("C:/Users/", username, "/Dropbox/UNICEF_Work_Project/CME.plot/R")
  files_in_folder <- list.files(CME_code_source, full.names = TRUE)

  # limit to files end with ".R" and the .txt file
  R_sources <- c(grep(".R", files_in_folder, value = TRUE, fixed = TRUE),
                 grep(".txt", files_in_folder, value = TRUE, fixed = TRUE))

  # copy source files to designated folders

  # could copy to multiple destinations as well
  copy.paste.script <- function(dir0){
    dir0 <- file.path(dir0, "CME.plot") # /"CME Plots Code"/
    unlink(dir0, recursive = TRUE)
    dir.create(dir0, recursive = TRUE)
    invisible(lapply(R_sources, file.copy, to = dir0, overwrite = TRUE))
  }
  suppressWarnings({
    invisible(lapply(dir_of_destinations, copy.paste.script))
  })
  message("Data copied to: \n", paste(dir_of_destinations, collapse = "\n"))
  return(invisible())
}


#' Copy data to Dropbox allowing direct loading the code without loading the
#' library. All paths are absolute paths.
#'
#' @export copy_data_to_dropbox
#' @return NULL
#'
copy_data_to_dropbox <- function(){

  username <- Sys.getenv("USERNAME")
  # revise the destination folders accordingly
  dir_of_destinations <- list(
    "Localcopy" = file.path("C:/Users/", username, "/Dropbox/UNICEF_Work_Project/2021_Code/output/"),
    "IGME2020" = file.path("C:/Users/", username, "/Dropbox/UN IGME Data/2020 Round Estimation/Code/output/"),
    "IGME2021" = file.path("C:/Users/", username, "/Dropbox/UN IGME Data/2021 Round Estimation/Code/output/"),
    "NMR" = file.path("C:/Users/", username, "/Dropbox/NMR/output/")
  )
  if(!any(sapply(dir_of_destinations, dir.exists)))stop("Check if file path exists")
  # The rest won't change

  # Code
  CME_code_source <- file.path("C:/Users/", username, "/Dropbox/UNICEF_Work_Project/CME.plot/data")
  files_in_folder <- list.files(CME_code_source, full.names = TRUE)

  # limit to files end with ".R" and the .txt file
  rda_data_files <- c(grep(".rda", files_in_folder, value = TRUE, fixed = TRUE))

  # copy source files to designated folders

  # could copy to multiple destinations as well
  copy.paste.script <- function(dir0){
    dir0 <- file.path(dir0, "figData")
    unlink(dir0, recursive = TRUE)
    dir.create(dir0, recursive = TRUE)
    invisible(lapply(rda_data_files, file.copy, to = dir0, overwrite = TRUE))
  }
  suppressWarnings({
    invisible(lapply(dir_of_destinations, copy.paste.script))
  })
  message("Data copied to: \n", paste(dir_of_destinations, collapse = "\n"))

  return(invisible())
}
