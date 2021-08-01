# Yang adds universal helper functions for CME plotting
# 2019/12
# helper functions



#' Create IGME_Key column
#'
#' Extra strings like "Preliminary" or "MM/NN adjusted" are removed in the
#' created `IGME_Key` column
#'
#' @param dt0 dataset
#'
#' @return dt0 dataset with added column `IGME_Key`
#' @export create.IGME.key
create.IGME.key <- function(dt0){
  strings_to_remove <- " \\(Adjusted\\)| \\(MM adjusted\\)| \\(NN adjusted\\)| \\(Preliminary\\)| \\(preliminary\\)"

  # the process to create IGME_Key
  if ("Country.Code"%in%colnames(dt0)&is.character(dt0$Country.Code)) {
    dt0[, Code:= Country.Code]
  } else if ("Country.ISO"%in%colnames(dt0)&is.character(dt0$Country.ISO)) {
    dt0[, Code:= Country.ISO]
  } else {stop("Check Country.Code and Country.ISO")}
  # Some SVR like South Africa has year associated with it
  dt0[Series.Category %in% c("VR", "SVR"), IGME_Key := paste0(Code, "-", Series.Category)]
  dt0[Series.Type %in% c("Life Table"), IGME_Key := paste0(Code, "-", Series.Type)]
  # dt0[Series.Type %in% c("Life Table"), ]
  dt0[!Series.Category %in% c("VR", "SVR", "Life Table"), IGME_Key := paste0(Code, "-", Series.Year, "-", Series.Name)]
  dt0[Series.Category %in% c("SVR") & Country.Name == "South Africa", IGME_Key := paste0(Code, "-", Series.Year, "-", Series.Category)]
  dt0[, IGME_Key := gsub(strings_to_remove, "", IGME_Key)]
  # remove blank
  dt0[, IGME_Key := trimws(IGME_Key)]

  # add direct/indirect?
  dt0[grepl("Direct", Series.Type), IGME_Key := paste0(IGME_Key, "-Direct")]
  dt0[grepl("Indirect", Series.Type), IGME_Key := paste0(IGME_Key, "-Indirect")]

  dt0[, Code:=NULL]
  return(dt0)
}


#' A label function to replace values by a given list in a variable
#'
#' You can provide a __new_list__ to define the values you wish to change in
#' this variable. Values not revised in the given list will be kept
#'
#' @param x a element or a vector
#' @param new_list if you supply a new list the function will use instead of the
#'   default_labels
#' @param no_line_break to remove linebreak from the string
#' @export get.match
#' @return an updated vector as character
get.match <- function(x,
                      new_list = NULL,
                      no_line_break = FALSE){
  if(is.null(new_list)){
    labs <- default_label
  } else {
    if(is.list(new_list)){
      labs <- new_list
    } else {
      message("new_list must be a list. Still use the default list.")
      labs <- default_label
    }
  }
  if(!is.character(x)){
    message("Coerse input into character.")
    x <- as.character(x)
  }
  out <- rep(NA, length(x))
  for (i in 1:length(x)){
    if (is.null(labs[[ x[i] ]])){
      out[i] <- x[i]
    }else{
      out[i] <- labs[[ x[i] ]]
    }
  }
  return(if(no_line_break)gsub("\n", "", out) else out)
}


#' Get directory to gender mcmc file
#'
#' Internal function used by \code{\link{savePlotResults}}
#' @importFrom grDevices adjustcolor dev.off pdf png rainbow
#' @importFrom graphics abline layout legend lines par points polygon segments
#' @importFrom stats median na.omit quantile
#' @importFrom data.table := setDT setorder setnames setkey melt.data.table
#'   dcast.data.table %like% .N .SD fread uniqueN rbindlist
#'
#' @param output.dir output.dir
#' @param gender_ind accepts either "Q5 Q1 Q4" or "q5 q1 q4"
#' @param gender     accepts "m" "f" "ratio"
get.mcmc.meta.dir <- function(output.dir, gender_ind, gender){
  #
  ind_vector <- c("Q5", "Q1", "Q4")
  ind_vector2 <- c("U5MR", "IMR", "CMR")
  new_list <- list(
    "U5MR" = "Q5",
    "IMR" = "Q1",
    "CMR" = "Q4"
  )
  gender_ind <- toupper(gender_ind)
  if(gender_ind%in%ind_vector2) gender_ind <- get.match(gender_ind, new_list = new_list)
  if (!gender_ind %in% ind_vector) stop("`gender_ind` should be among ",
                                                 paste(c(ind_vector, ind_vector2), collapse = ", "))
  gender_vector <- c("m", "f", "ratio")
  if (!gender %in% gender_vector) stop("`gender` should be among ", paste(gender_vector, collapse = ", "))
  #
  file_dir <- file.path(output.dir, paste0("mcmc.meta.gender.",
                                           gender_ind,".", gender,".rda"))
  if(!file.exists(file_dir)) stop("Check if the file ",file_dir ," exists.")
  message("Gender-specific file used: ", file_dir)
  return(file_dir)
}

#' What year is last year?
#' @return e.g. 2019.5
#' @export last.year
last.year <- function(){
  as.numeric(format(Sys.Date(), "%Y")) - 0.5
}

#' What year is this year?
#' @return e.g. 2020.5
#' @export this.year
this.year <- function(){
  as.numeric(format(Sys.Date(), "%Y")) + 0.5
}

#' Check if some rda files exist
#'
#' Check if mandatary files exist in the output dir supplied
#' @param output.dir output dir
#' @param mcmc.only check mcmc.meta.rda only
check.rda.exist <- function(output.dir, mcmc.only = FALSE){
  if(!dir.exists(output.dir)) message ("- file.dir doesn't exist")
  report <- function(file){
    if (file.exists(file.path(output.dir, file))) message("+ ", file, " exists") else message("- Counldn't find ", file)
    file.exists(file.path(output.dir, file))
  }

  all_files <- c("mcmc.meta.rda",
                 get.res.cqt.rda.diffname(output.dir, name_only = TRUE),
                 "year.t.rda")
  files <- if(mcmc.only) c("mcmc.meta.rda") else all_files
  invisible(sapply(files, report))
}

#' Update cname using a supplied data.frame with new "OfficialName" if the
#' supplied data frame looks right
#' Internal function
#' @param new.cname.df new_cname
#' @param mcmc.meta mcmc.meta object
#'
#' @return a vector country names in the right order following
#'   `mcmc.meta$data.all$iso.c`
#'
update.name.c <- function(new.cname.df, mcmc.meta){
  if(!is.data.frame(new.cname.df)| !all(c('ISO3Code', 'OfficialName')%in%colnames(new.cname.df))){
    message("Please provide new.cname.df as a data.frame containing two columns: 'ISO3Code', 'OfficialName'.")
    return(mcmc.meta$data.all$name.c)
  } else {
    new.cname.df$ISO3Code <- as.character(new.cname.df$ISO3Code)
    new.cname.df$OfficialName <- as.character(new.cname.df$OfficialName)
    new.cname.df <- new.cname.df[,c("ISO3Code", "OfficialName")]
    iso.c  <-  mcmc.meta$data.all$iso.c
    new_name.c <- new.cname.df$OfficialName[match(iso.c, new.cname.df$ISO3Code)]
    return(new_name.c)
  }
}

#' Get the new official country names from country.info.CME.csv
#' @importFrom utils read.csv
#'
#' @param filename default to "country.info.CME.csv"
#' @param dir_IGME_input input dir to look for country.info.CME.csv
#'
#' @export get.new_cnames
get.new_cnames <- function(dir_IGME_input, filename = "country.info.CME.csv"){
  if(file.exists(file.path(dir_IGME_input, filename))){
    new_cnames <- utils::read.csv(file.path(dir_IGME_input, filename) ,
                           stringsAsFactors = FALSE, header=TRUE)[,c("ISO3Code", "UNCode", "OfficialName")]
    return(new_cnames)
  } else {
    stop("Check if file exists: ", file.path(dir_IGME_input, filename))
  }
}


#' Internal function to update new_cnames.rda (official names)
#' @importFrom usethis use_data
#' @param country.info.file.name country.info.CME.csv
#'
update.data.new_cnames <- function(country.info.file.name = "country.info.CME.197isos.csv"){
  new_cnames <- get.new_cnames(get.IGMEinput.dir(2021), country.info.file.name)
  usethis::use_data(new_cnames, overwrite = TRUE)
  return(invisible())
}

# Functions for managing array  ------------------------------------------------

#' If the cqt is named differently, allowing an alternative to select cqt in
#' other names
#'
#' @param output.dir dir to output folder
#' @param name_only return only the file names, otherwise the cqt object
#'
#' @return the wanted res.cqt.Lw object
#'
get.res.cqt.rda.diffname <- function(output.dir, name_only = FALSE){
  # return the first cqt file found among this list:
  cqt.names <- c( "res.cqt.Lw.new.rda", "res.cqt.Lw.rda")
  cqt.exist <- file.exists(file.path(output.dir, cqt.names))

  if(any(cqt.exist)){
    cqt.names <- cqt.names[cqt.exist][1]
    load(file.path(output.dir, cqt.names))
    res.cqt.Lw <- get(sub(".rda", "", cqt.names))
  } else {
    cqt.names <- NULL
    res.cqt.Lw <- NULL
  }
  return(if(name_only) cqt.names else res.cqt.Lw)
}


#' Obtain cqt from runname2, match to runname1 by iso order and number and year
#' range
#'
#' Match output.dir2 to output.dir1, prefer to supply the complete `output.dir`
#' than using `runname`
#'
#' @param output.dir1 the output directory including runname folder, where to
#'   find res.cqt, iso.c, and year.t
#' @param output.dir2 the output directory including runname folder, where to
#'   find res.cqt, iso.c, and year.t
#' @param runname1 if supply, will try to load `file.path(getwd(), "output",
#'   runname1)`
#' @param runname2 if supply, will try to load `file.path(getwd(), "output",
#'   runname1)`
#' @param pooling_weight pooling_weight to draw from res.cqt
#'
#' @return matched cqt object
#' @export obtain.matched.cqt
#'
obtain.matched.cqt <- function(
  output.dir1 = NULL,
  output.dir2 = NULL,
  runname1 = NULL,
  runname2 = NULL,
  pooling_weight = 0.5
){
  if (is.null(output.dir1)) output.dir1 <- file.path(getwd(), "output", runname1)
  if(!dir.exists(output.dir1)) stop("Check if dir exists: ", output.dir1)
  if (is.null(output.dir2)) output.dir2 <- file.path(getwd(), "output", runname2)
  if(!dir.exists(output.dir2)) stop("Check if dir exists: ", output.dir2)

  load(file.path(output.dir1, "iso.c.rda"))
  iso.c1 <- iso.c
  load(file.path(output.dir1, "year.t.rda"))
  year.t1 <- year.t
  res.cqt.Lw1 <- get.res.cqt.rda.diffname(output.dir1)
  res.cqt1 <- res.cqt.Lw1[[pooling_weight]]


  load(file.path(output.dir2, "iso.c.rda"))
  iso.c2 <- iso.c
  load(file.path(output.dir2, "year.t.rda"))
  year.t2 <- year.t
  res.cqt.Lw2 <-  get.res.cqt.rda.diffname(output.dir2)
  res.cqt2 <- res.cqt.Lw2[[pooling_weight]]

  resfinal.cqt2 <- array(NA, c(length(iso.c1), 3, length(year.t1)))
  resfinal.cqt2[, , is.element(year.t1, year.t2)] <-
    res.cqt2[match(iso.c1, iso.c2), , is.element(year.t2, year.t1)]
  dimnames(resfinal.cqt2)[[3]] <- year.t1

  return(resfinal.cqt2)
}



#' Match `res.cqt2` to `iso.c1` and `year.t1`, the core code used in
#' obtain.matched.cqt
#'
#' @param iso.c1 target iso(s)
#' @param year.t1 target years
#' @param res.cqt2 the cqt that needs to be matched to target
#'
#' @return matched cqt object
#'
match.cqt.core <- function(iso.c1, year.t1, res.cqt2){
  if(is.null(res.cqt2)) return(NULL)
  iso.c2 <- dimnames(res.cqt2)[[1]]
  year.t2 <- dimnames(res.cqt2)[[3]]
  year.t2_new <- floor(as.numeric(year.t2))
  year.t1_new <- floor(as.numeric(year.t1))

  resfinal.cqt2 <- array(NA, c(length(iso.c1), 3, length(year.t1)))
  resfinal.cqt2[, , is.element(year.t1_new, year.t2_new)] <-
    res.cqt2[match(iso.c1, iso.c2), , is.element(year.t2_new, year.t1_new)]
  dimnames(resfinal.cqt2)[[3]] <- year.t1
  # dimnames(resfinal.cqt2)[[1]] <- iso.c1
  return(resfinal.cqt2)
}

#' Match iso of `res.cqt2` to iso.c1
#'
#' @inheritParams match.cqt.core
#'
#' @return iso-matched cqt object
#'
match.cqt.iso <- function(iso.c1, res.cqt2){
  if(is.null(res.cqt2)) return(NULL)
  iso.c2 <- dimnames(res.cqt2)[[1]]
  resfinal.cqt2 <- res.cqt2[match(iso.c1, iso.c2), , , drop = FALSE]
  return(resfinal.cqt2)
}

#' Limit the 3rd dimension (year) of NMR cqt
#'
#' Set what's beyond year_end as NA since the NMR cqt goes to 2030.5
#'
#' @param res.cqt a 3-dimentional cqt array
#' @param year_start year_start
#' @param year_end year_end
set.cqt.year.limit <- function(
  res.cqt = NULL,
  year_start = NULL,
  year_end = last.year()
  ){
  if(is.null(res.cqt)) return(NULL)
  year.t <- as.numeric(dimnames(res.cqt)[[3]])
  # adjust res.cqt
  if (!is.null(year_start)) {
    if (year_start > min(year.t)) {
      res.cqt[, , year.t < year_start] <- NA
    }
  }
  if (!is.null(year_end)) {
    if (year_end < max(year.t)) {
      res.cqt[, , year.t > year_end] <- NA
    }
  }
  return(res.cqt)
}


# For sex-specific --------------------------------------------------------

#' Fix some entries in the dataset
#'
#' @param dt_gender the dataset for plotting
#' @return revised dt_gender
#' @export fix.entries.dt_gender
fix.entries.dt_gender <- function(dt_gender){

  # MRT keeps "Revised in January 2019 - coding bug in CMRJack input needed correction" (the closer one)
  dt_gender[Country.Code=="MRT"&Visible==1&Series.Name=="Multiple Indicator Cluster Survey"&Series.Year=="2015"&Date.Of.Data.Added=="2018-06", Visible:=0]
  # Edit MAR Morocco PAPFAM, which is a dup of DHS2003-2003 (same values)
  # Duplication: MAR Demographic and Health Survey 2003-2004
  dt_gender[Country.Code=="MAR"&Visible==1&Series.Name=="PAPFAM Family Health Survey"&Series.Year=="2003-2004", Visible:=0]

  dt_gender[Country.Code=="LUX" & Series.Name=="WHO Good Vital Registration Data 2018 version" & Visible ==1,
            `:=`(Series.Name = "WHO Vital Registration Data 2020 version",
                 Series.Year = 2020,
                 End.date.of.Survey = 2020)]

  dt_gender[Country.Code=="MCO"& Visible ==1,
            `:=`(
              # Series.Year = 2020,
              End.date.of.Survey = 2019)]

  # Fix India issue, shouldn't have NA `End.date.of.Survey`
  dt_gender[Series.Type=="VR" & Visible ==1 & is.na(End.date.of.Survey), End.date.of.Survey:= as.numeric(Series.Year)]

  # the data should be sorted correctly
  setorder(dt_gender, Country.Name, -End.date.of.Survey, Series.Name, Series.Type,-Reference.Date, - Inclusion.Gender)

  if(dt_gender[Country.Code=="LUX" & Indicator%like%"Under-five" & Visible == 1,][Series.Name=="WHO Good Vital Registration Data 2018 version",.N]>0) warning("Check LUX")

  # numeric columns
  col_num <- c(
    "Country.ISO"             ,"Start.date.of.Survey" ,   "End.date.of.Survey"     , "Average.date.of.Survey" ,
    "Series.Quantity"         ,"Interval"             ,   "Reference.Date"         , "Sex.Ratio"              ,
    "Sex.Ratio.SE"            ,"Male"                 ,   "Male.SE"                , "Female"                 ,
     "Female.SE"              , "Both.Sexes"          ,    "Both.Sexes.SE"         ,  "Inclusion.U5MR"         ,
     "Exclusion.External.Info", "Exclusion.Old.Data"  ,    "Exclusion.Total.U5MR"  ,  "Visible"                ,
     "agecat.i"               , "Inclusion.Gender"    ,    "Q1.i"                  ,  "Q4.i"                   ,
     "Q5.i"
  )
  dt_gender <- dt_gender[, (col_num):= lapply(.SD, function(x)as.numeric(as.character(x))), .SDcols = col_num]

  if(!"IGME_Key" %in% colnames(dt_gender))  dt_gender <- create.IGME.key(dt_gender)
  return(dt_gender)
}

# return a table of cqt files:
#    ind gender    name
# 1:  Q5      m Q5m.cqt
# 2:  Q1      m Q1m.cqt
# 3:  Q4      m Q4m.cqt
# 4:  Q5      f Q5f.cqt
# 5:  Q1      f Q1f.cqt
# 6:  Q4      f Q4f.cqt
# 7:  Q5  ratio  S5.cqt
# 8:  Q1  ratio  S1.cqt
# 9:  Q4  ratio  S4.cqt

#' Create a table of cqt files
#'
#' @return a table of ind, gender and corresponding filenames like Q5m.cqt
#' @export get.gender.cqt.grid
get.gender.cqt.grid <- function(){
  # using male data:   1. Q1m for infant ; 2. Q4m for child ; 3. Q5m for under-five
  # using female data: 1. Q1f for infant ; 2. Q4f for child ; 3. Q5f for under-five
  # ratio: S1, S4, S5
  ind_vector <- c("Q5", "Q1", "Q4")
  gender_vector <- c("m", "f", "ratio")
  # cqt_files list all the combinations and cqt file to extract
  cqt_files <- data.table::setDT(expand.grid(ind = ind_vector, gender = gender_vector))
  cqt_files[gender!="ratio", name:=paste0(ind, gender, ".cqt")]
  cqt_files[gender=="ratio", name:=paste0("S", substr(ind, 2,3) ,".cqt")]
  return(cqt_files)
}


#' Read the specific cqt, like Q5m.cqt, from `resultsfile`
#'
#' @param ind0 ind0
#' @param gender0 gender0
#' @param get_expected get expected file or not
#' @param resultsfile results file
#' @export
get.gender.cqt <- function(ind0, gender0, get_expected = FALSE, resultsfile = NULL){
  if(!is.null(resultsfile)) {
    load(resultsfile)
    message("Load new `res.full` from directory: ", resultsfile)
  }
  cqt_files <- get.gender.cqt.grid()
  cqt_file_name <- if (get_expected) paste0("exp", cqt_files[ind==ind0 & gender == gender0, name]) else
  cqt_files[ind==ind0 & gender == gender0, name]
  # message("cqt_file_name is ", cqt_file_name)
  return(res.full[[cqt_file_name]])
}


# For 5-14 ----------------------------------------------------------------

#' Find those special "Derived from 5q0" isos based on mcmc.meta
#'
#' @param mcmc.meta mcmc.meta object
#' @param HIV_removed if TRUE use mcmc.meta$data.hivremoved.all instead of
#'   mcmc.meta$data.all
#' @param pattern0 pattern to grep, default to "Derived from 5q0"
#' @param legend_normal how to name the normal legend, default to "B3"
#' @param legend_special how to name the special legend, default to "Derived
#'   from U5MR"
#'
#' @return both the special isos and the customized legend vector
#' @export get.special.isos
get.special.isos <- function(
  mcmc.meta,
  HIV_removed = FALSE,
  pattern0 = "Derived from 5q0",
  legend_normal = "B3", # how to name the normal legend
  legend_special = "Derived from U5MR" # how to name the special legend
){
  data.all <- mcmc.meta$data.all
  if(HIV_removed) data.all <- mcmc.meta$data.hivremoved.all

  flag.special <- function(i){
    if(!is.null(data.all$source.Lc.s[[i]])){
      any(grepl(pattern0, data.all$source.Lc.s[[i]]))
    } else {
      FALSE
    }
  }
  flags1 <- sapply(1:data.all$C, flag.special)

  # 15-25 counts Derived from 5q0 as VR
  flag.special.10q15 <- function(i){
    if(!is.null(data.all$sourcevr.Lc.s[[i]])){
      any(grepl(pattern0, data.all$sourcevr.Lc.s[[i]]))
    } else {
      FALSE
    }
  }
  flags2 <- sapply(1:data.all$C, flag.special.10q15)
  flags <- flags1 | flags2
  special_isos <- data.all$iso.c[flags]
  special_legend_vector <- rep(legend_normal, data.all$C)
  special_legend_vector[flags] <- legend_special

  if(HIV_removed){
    mcmc.meta$data.hivremoved.all <- data.all
  } else {
    mcmc.meta$data.all <- data.all
  }

  return(list(special_isos = special_isos,
              special_legend_vector = special_legend_vector))

}

#' Remove specific series e.g. "Derived from 5q0" in 5-14 datasets
#'
#' @param mcmc.meta mcmc.meta object
#' @param HIV_removed is it for `mcmc.meta$data.hivremoved.all`
#' @param remove_date remove series if all the series are before 1990, this is mainly for legend purpose since year.start won't clean up the legend
#' @param remove_pattern e.g. "Derived from 5q0"
#' @return revised mcmc.meta object
#' @export remove.specific.series
#'
remove.specific.series <- function(
  mcmc.meta,
  HIV_removed = FALSE,
  remove_pattern = "Derived from 5q0",
  remove_date = 1990
  ){
  data.all <- mcmc.meta$data.all
  if(HIV_removed) data.all <- mcmc.meta$data.hivremoved.all
  # message("Preview for AGO before: ", paste(data.all$source.Lc.s[[5]], sep = ", "))

  for(i in 1:length(data.all$iso.c)){
    if(!is.null(data.all$source.Lc.s[[i]])){
    # print(i)
    # extract sourceid from data.all
    # keep_series is a white list e.g. T,T,T,F & F,T,T,T (max >= 1990) = F, T, T, F
    keep_series <- !grepl(remove_pattern, data.all$source.Lc.s[[i]]) & (sapply(data.all$year.Lcs.j[[i]], max) >= remove_date)
    data.all$nseriesnonvr.c[[i]] <- sum(keep_series)
    data.all$source.Lc.s[[i]] <- data.all$source.Lc.s[[i]][keep_series]
    data.all$u.Lcs.j[[i]] <- data.all$u.Lcs.j[[i]][keep_series]
    data.all$year.Lcs.j[[i]] <- data.all$year.Lcs.j[[i]][keep_series]
    data.all$seriesyear.Lc.s[[i]] <- data.all$seriesyear.Lc.s[[i]][keep_series]

    data.all$se.Lcs.j[[i]] <- data.all$se.Lcs.j[[i]][keep_series]
    data.all$senonNA.Lcs.j[[i]] <- data.all$senonNA.Lcs.j[[i]][keep_series]
    data.all$included.Lcs.j[[i]] <- data.all$included.Lcs.j[[i]][keep_series]
    data.all$sourcetype.Lc.s[[i]] <- data.all$sourcetype.Lc.s[[i]][keep_series]
    data.all$sourcetype.Lcs.j[[i]] <- data.all$sourcetype.Lcs.j[[i]][keep_series]
    data.all$method.Lc.s[[i]] <- data.all$method.Lc.s[[i]][keep_series]
    data.all$method.Lcs.j[[i]] <- data.all$method.Lcs.j[[i]][keep_series]
    data.all$sourceid.Lcs.j[[i]] <- data.all$sourceid.Lcs.j[[i]][keep_series]
    data.all$interval.Lcs.j[[i]] <- data.all$interval.Lcs.j[[i]][keep_series]
    data.all$hasbias.Lc.s[[i]] <- data.all$hasbias.Lc.s[[i]][keep_series]
    }
  }

  # message("Preview for AGO after: ", paste(data.all$source.Lc.s[[5]], sep = ", "))
  if(HIV_removed){
    mcmc.meta$data.hivremoved.all <- data.all
  } else {
    mcmc.meta$data.all <- data.all
  }

  return(mcmc.meta)
}

#' Remove specific series e.g. "Derived from 5q0" in 15-24 datasets
#'
#' @inheritParams remove.specific.series
#' @return revised mcmc.meta object
#' @export remove.specific.series.15_24
#'
remove.specific.series.15_24 <- function(
  mcmc.meta,
  HIV_removed = FALSE,
  remove_pattern = "Derived from 5q0",
  remove_date = 1990
){
  data.all <- mcmc.meta$data.all
  if(HIV_removed) data.all <- mcmc.meta$data.hivremoved.all
  # message("Preview for AND before: ", paste(data.all$sourcevr.Lc.s[[4]], sep = ", "))

  for(i in 1:length(data.all$iso.c)){
    if(!is.null(data.all$sourcevr.Lc.s[[i]])){
      # print(i)
      # extract sourceid from data.all
      # keep_series is a white list e.g. T,T,T,F
      keep_series <- !grepl(remove_pattern, data.all$sourcevr.Lc.s[[i]])
      data.all$nseriesvr.c[[i]] <- sum(keep_series)
      data.all$sourcevr.Lc.s[[i]] <- data.all$sourcevr.Lc.s[[i]][keep_series]
      data.all$sourceidvr.Lc.s[[i]] <- data.all$sourceidvr.Lc.s[[i]][keep_series]

      data.all$uvr.Lcs.j[[i]] <- data.all$uvr.Lcs.j[[i]][keep_series]
      data.all$yearvr.Lcs.j[[i]] <- data.all$yearvr.Lcs.j[[i]][keep_series]

      data.all$sevr.Lcs.j[[i]] <- data.all$sevr.Lcs.j[[i]][keep_series]
      data.all$senonNAvr.Lcs.j[[i]] <- data.all$senonNAvr.Lcs.j[[i]][keep_series]

      data.all$includedvr.Lcs.j[[i]] <- data.all$includedvr.Lcs.j[[i]][keep_series]
      data.all$isincompletevr.Lcs.j[[i]] <- data.all$isincompletevr.Lcs.j[[i]][keep_series]

      data.all$intervalvr.Lcs.j[[i]] <- data.all$intervalvr.Lcs.j[[i]][keep_series]
      data.all$hasbiasvr.Lcs.j[[i]] <- data.all$hasbiasvr.Lcs.j[[i]][keep_series]
    }
  }

  # message("Preview for AND after: ", paste(data.all$sourcevr.Lc.s[[4]], sep = ", "))
  if(HIV_removed){
    mcmc.meta$data.hivremoved.all <- data.all
  } else {
    mcmc.meta$data.all <- data.all
  }

  return(mcmc.meta)
}

#' make res.cqt from results.csv
#'
#' read results.csv and mcmc.meta files in the output dir and make
#' res.cqt.Lw.rda
#'
#'
#' @param output_dir output.dir
#' @param mcmc.meta_filename file name to read, default to "mcmc.meta.rda"
#' @param results_filename file name to read, default to "results.csv"
#' @param return_dt_long default to FALSE, if TRUE, returns the long-format
#'   results file
#'
#' @return res.cqt.Lw
#' @export get.cqt.from.results
#'
get.cqt.from.results <- function(
  output_dir,
  mcmc.meta_filename = "mcmc.meta.rda",
  results_filename = "results.csv",
  return_dt_long = FALSE
){
  check.rda.exist(output_dir)

  if(file.exists(file.path(output_dir, mcmc.meta_filename))){
    load(file.path(output_dir, mcmc.meta_filename))
  } else {
    stop("File doesn't exist: ", file.path(output_dir, mcmc.meta_filename))
  }
  iso_order <- mcmc.meta$data.all$iso.c

  if(file.exists(file.path(output_dir, "year.t.rda"))){
    load(file.path(output_dir, "year.t.rda"))
  } else {
    message("Maybe you can get `year.t.rda` from `as.numeric(dimnames(res.cqt.Lw$`0.5`)[[3]])`")
    # but I won't do it automatically here since it's not always right
    stop("File doesn't exist: ", file.path(output_dir, "year.t.rda"))
  }
  years <- year.t

  if(file.exists(file.path(output_dir, results_filename))){
    dt <- fread(file.path(output_dir, results_filename))
  } else {
    stop("File doesn't exist: ", file.path(output_dir, results_filename))
  }
  vars_wanted <- c("ISO.Code", "Quantile", paste0("X", years))
  dt_long <- melt(dt[,..vars_wanted], measure.vars = paste0("X", years), variable.factor = FALSE)
  dt_long[, years:=as.numeric(sub("X", "", variable))]
  dt_long <- dt_long[order(match(ISO.Code, rep(iso_order, each = length(years))))]
  setorder(dt_long, years, Quantile) # set the right order is the key to produce right array
  if(return_dt_long)
    return(dt_long
    )
  # Now the order is by t (year), q, and c (iso)
  cqt <- array(data = dt_long[, value],
               dim = c(length(iso_order),
                       3,
                       length(years)),
               dimnames = list(c = iso_order,
                               q = c(0.05, 0.5, 0.95),
                               t = years))
  res.cqt.Lw <- list()
  res.cqt.Lw[["0.5"]] <- cqt
  return(res.cqt.Lw)
}


#' Check cqt file vs. results.csv
#'
#' Return the difference if there is any
#' @importFrom reshape2 melt
#' @param dir_res.cqt_file directory where to find the "res.cqt.Lw.rda"
#' @param dir_results.csv_file directory where to find the "results.csv"
#' @param alpha_weight default to "0.5"
#'
#' @return the different part as dt
#' @export check.cqt.vs.results.csv
#'
check.cqt.vs.results.csv <- function(
  dir_res.cqt_file,
  dir_results.csv_file,
  alpha_weight = "0.5" # 0.8 for 15-24
){
  # load the cqt file
  if(!file.exists(file.path(dir_res.cqt_file, "res.cqt.Lw.rda"))) stop("res.cqt.Lw.rda doesn't exist")
  load(file.path(dir_res.cqt_file, "res.cqt.Lw.rda"))
  res.cqt <- res.cqt.Lw[[as.character(alpha_weight)]]
  dt_cqt <- reshape2::melt(res.cqt) ## distinguish from melt.data.table
  data.table::setnames(data.table::setDT(dt_cqt), c("ISO.Code", "Quantile", "Year", "Value"))
  dt_cqt <- na.omit(dt_cqt)
  setkey(dt_cqt, ISO.Code, Quantile, Year)
  dt_cqt[, Quantile:=as.factor(Quantile)]
  levels(dt_cqt$Quantile) <- c("Lower", "Median", "Upper")
  setkey(dt_cqt, ISO.Code, Quantile, Year)

  # results.csv
  years <- sort(unique(dt_cqt$Year))
  if(!file.exists(file.path(dir_results.csv_file, "Results.csv"))) stop("Results.csv doesn't exist")
  dt1 <- fread(file.path(dir_results.csv_file, "Results.csv"))
  vars_wanted <- c("ISO.Code", "Quantile", paste0("X", years))
  dt_long <- melt(dt1[,..vars_wanted], measure.vars = paste0("X", years),
                  variable.factor = FALSE, value.name = "csv_value")
  dt_long[, Year:=as.numeric(sub("X", "", variable))]
  dt_long <- na.omit(dt_long)
  dt_long[, variable := NULL]
  dt_long[, Quantile:=as.factor(Quantile)]
  setkey(dt_long, ISO.Code, Quantile, Year)

  dt2 <- dt_long[dt_cqt]

  roundoff <- function(#
    x, digits = 2
  ) {
    if(!is.numeric(x)) message("x coerse to numeric. ")
    x <- as.numeric(x)
    z <- trunc(abs(x)*10^digits + 0.5)
    z <- sign(x)*z/10^digits
    return(z)
  }

  dt2[, diff := roundoff(Value, 1) - roundoff(csv_value,1)]
  if(mean(dt2$diff)==0) {
    message("All the data passed check")
  } else {
    message("Check following ", dt2[diff>0, uniqueN(ISO.Code)]," isos: ", paste(dt2[diff>0, unique(ISO.Code)], collapse = ", "))
    return(dt_diff = dt2[diff>0, ])
  }
}

# Extra ------------------------------------------------------------------


#' source all .R files in the folder
#'
#' @param print_file_names logic, print/return all the files sourced
#' @param dir0 all the required scripts are in the `/R` folder
source.CME.plot <- function(print_file_names = FALSE, dir0 = NULL){
  if(is.null(dir0)) {
    dir0 <- file.path(getwd(), "R")
    cat("Source from local R folder\n")
  }
  # try absolute path if this is not working
  if(length(list.files(dir0))==0) dir0 <- file.path(Sys.getenv("USERPROFILE"), "/Dropbox/UNICEF Work/CME Plots/R")
  if(length(list.files(dir0))==0) stop("Check if file directory is set correctly for ", dir0)
  files_in_folder <- list.files(dir0, full.names = TRUE)
  # limit to files end with ".R"
  R_sources <- grep(".R", files_in_folder, value = TRUE, fixed = TRUE)
  R_sources_print <- sort(grep(".R", list.files(dir0), value = TRUE, fixed = TRUE))
  invisible(sapply(R_sources, source))
  if(print_file_names)return(R_sources_print)
}
