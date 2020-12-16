# PlotResults -> savePlotResults
# based on the original "PlotResults" function by Jinrou
# Revised 202001 - UNICEF

#' Major function to save CME plots
#'
#' `savePlotResults` saves plots as png or pdf files by setting `pdf.or.png`. It
#' saves results as one file or separately by setting
#' `separate.plots.by.country`. It calls the plotting function
#' \code{PlotDataAndEstimates2020}. Plot a subset of countries using
#' either `n.countries` (a numeric vector) or `iso.subset.c` (a vector of
#' ISO3Code). The name of the output file saved is auto-generated as
#' `filename`+`runname` auto-generated part (Results/Data/Results Comparison +
#' legends of series) depending on how many series will be plot. Set `AfterCC`
#' to `TRUE` will force the output to be named "Results" instead of
#' "Comparison". The function combined the old `PlotResults` and
#' `PlotComparison` function. If error occurs, debugging information is
#' returned.
#'
#' @param runname used to look for `res.cqt`. If `output.dir` is NULL, it will
#'   make the `output.dir` from runname if `getwd()` can point correctly to the
#'   IGME "Code" folder. If `output.dir` is supplied (recommended), `runname` is
#'   used as a label in the filename for Data and Results-only plots
#' @param runname2 used to look for `res.cqt2` if `output.dir2` is NULL. It will
#'   be used as legend if legend is not supplied
#' @param runname3 used to look for `res.cqt3` if `output.dir3` is NULL. It will
#'   be used as legend if legend is not supplied
#' @param runname4 used to look for `res.cqt4` if `output.dir4` is NULL. It will
#'   be used as legend if legend is not supplied
#' @param output.dir the MCMC output folder: where to read `mcmc.meta.rda` and
#'   `res.cqt1`
#' @param output.dir2 if supplied and valid, will use it to locate `res.cqt2`
#' @param output.dir3 if supplied and valid, will use it to locate `res.cqt3`
#' @param output.dir4 if supplied and valid, will use it to locate `res.cqt4`
#'
#' @param filename extra label in the front of the output file if supplied
#' @param After_CC suppress the rules to create filenames automatically, adding
#'   a "Results" in the front for CC profiles
#' @param fig.dir where to save the figures
#' @param year.start default to NULL
#' @param year.end default to last year e.g. 2019.5 if now is 2020
#' @param zoom.year.start default to 1990
#' @param zoom.year.end   default to `last.year()`
#' @param main.plot default to TRUE, used to determine the right size of the
#'   plot
#' @param zoom      default to TRUE, used to determine the right size of the
#'   plot
#' @param add.legend default to TRUE, used to determine the right size of the
#'   plot
#' @param legend1 the legend for the main series (legendfull), has default
#'   value. It can be used as switch to hide the estimate by setting it
#'   as NULL. To plot imr method in legend: input \code{legend1 =
#'   mcmc.meta$data.all$imrmethod.c}, to plot 5-14 legend: use \code{legend1 =
#'   get.special.isos(mcmc.meta)$special_legend_vector}
#' @param legend2 show legend for cqt2, can be used as a switch for showing cqt
#'   for gender-specific plot, if
#' @param legend3 show legend for cqt3
#' @param legend4 show legend for cqt4
#' @param legend_ex show legend for (NMR) expected cqt
#' @param new_entry_date provide a date as "YYYY-MM". The series after this date
#'   will be highlighted as new series in red in plots
#' @param res.cqt1 the cqt file for first series, instead of supplying
#'   output.dir and runname, we can also input the cqt object directly
#' @param res.cqt2 the second series, years adjusted in the function, default to
#'   a green line
#' @param res.cqt3 the third series, years adjusted in the function, default to
#'   a brown (sienna) line
#' @param res.cqt4 the fourth series, years adjusted in the function, default to
#'   a dark-red line
#' @param res_ex.cqt the expected series for NMR and sex-specific plots
#'
#' @param pdf.or.png save results as "pdf" or "png"
#' @param separate.plots.by.country separate "pdf" by country? Ignored for "png"
#' @param new.cname.df the dataset of official country names, contains ISO and
#'   new country names, default to CME.assistant::new_cnames
#' @param n.countries select a subset of countries to plot using an numeric
#'   vector, e.g. 1:10. Ignored if it is HIV-removed plot, please use
#'   `iso.subset.c` to subset countries for HIV-removed plot
#' @param iso.subset.c select a subset of countries to plot using a vector of
#'   ISO3Code, If both `n.countries` and `iso.subset.c` are NULL, will plot all
#'   countries available in the meta data. If both are supplied, will plot
#'   countries commonly selected
#' @param sort_the_isos default to TRUE, if FALSE will just make plot following
#'   the order of `iso.subset.c`. This is useful in case that a specific order
#'   of the output is needed
#' @param NMR_metafile NMR mcmc.meta object produced and saved by
#'   \code{\link{transformdataforNMR}}. For NMR plot, either supply only the
#'   `NMR_metafile` (preferred), or the `output.dir` together with `NMR_scale`
#'   (so we know to read `mcmc.meta.nmr.rda` or `mcmc.meta.nmr.ratio.rda`). In
#'   short, supply either `NMR_metafile` or `output.dir` + `NMR_scale`
#' @param NMR_scale default to NULL. Only needed to supply as "NMR" or "ratio"
#'   for NMR plots if using `output.dir`, which contains `mcmc.meta.nmr.rda` and
#'   `mcmc.meta.nmr.ratio.rda`
#'
#' @param HIV_removed default to FALSE, set as TRUE for HIV-removed plot for
#'   HIV-countries
#' @param output.dir.for.hivremoved.cqt default to NULL. If provided will search
#'   in it for `res.hivremoved.cqt.Lw.rda` instead of searching in output.dir by
#'   default
#'
#' @param gender input to internal function \code{\link{get.mcmc.meta.dir}} for
#'   sex-specific plot, accepts "m", "f", or "ratio"
#' @param gender_ind input to internal function \code{\link{get.mcmc.meta.dir}}
#'   for sex-specific plot, accepts either "Q5", "Q1", "Q4" or "q5", "q1", "q4"
#' @param wpp.cqt a green line for WPP series
#' @param ihme.cqt a blue line for IHME series
#' @param legend_WPP  default WPP legend
#' @param legend_IHME default IHME legend
#'
#' @param ylab default to NULL, value supplied by mcmc.meta
#' @param pooling_weight default to "0.5", sometimes we need to show a different
#'   weight, for example, for 5-24 estimates
#' @param save_cqt_copy logical, if TRUE will save the `res.cqt.rds` plotted
#'   into a folder created as `cqt_backup`
#' @param ...  more arguments not listed here but can still be passed into the
#'   function `PlotDataAndEstimates2020`
#'
#' @return a list of related information: runname, time_spent, debugging
#'   information if there is an error
#'
#' @export savePlotResults
#'
savePlotResults <- function(
  filename = NULL,
  After_CC = FALSE,
  runname = NULL, # `Runname` for labeling and for constructing output.dir
  runname2 = NULL,
  runname3 = NULL,
  runname4 = NULL,
  output.dir = NULL,  # directory where mcmc.meta and raw output are stored
  output.dir2 = NULL, # if supplied and is valid, will use it to locate `res.cqt2`
  output.dir3 = NULL,
  output.dir4 = NULL,
  fig.dir = NULL,    # directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory. Will create directory if doesn't exist
  year.start = NULL, # start year of estimates to plot. If \code{NULL}, earliest year of estimates available is used.
  year.end = last.year(),   # end year of estimates to plot. If \code{NULL}, latest year of estimates available is used.
  zoom.year.start = 1990,
  zoom.year.end = this.year(),
  main.plot = TRUE,  # include main plot?
  zoom = TRUE,       # include zoom plot?
  add.legend = TRUE, # show legend?
  res.cqt1 = NULL,
  res.cqt2 = NULL,       # a green line, years adjusted in function
  res.cqt3 = NULL,       # a brown line, years adjusted in function
  res.cqt4 = NULL,       # a dark-red line, years adjusted in function
  res_ex.cqt = NULL,     # the expected series, years adjusted in function
  save_cqt_copy = FALSE, # if TRUE save a copy of res.cqt1 for checking
  legend1 = "Draft UN IGME 2020", # a switch for showing main estimates (set NULL for data plot): red line
  legend2 = NULL, # used as a switch for showing cqt2: green line
  legend3 = NULL, # used as a switch for showing cqt3: sienna line: `scales::show_col("sienna2")`
  legend4 = NULL, # used as a switch for showing cqt4: dark red line: `scales::show_col("#c7202e")`
  legend_ex = NULL,# used as a switch for showing expected series: blue dotted line
  wpp.cqt = NULL,  # green line
  ihme.cqt = NULL, # blue line
  legend_WPP = "WPP 2019",
  legend_IHME = "IHME GBD 2018",
  ylab = NULL,
  new_entry_date = NULL,
  pdf.or.png = "pdf",  # "pdf" or "png"
  separate.plots.by.country = FALSE,  # separate pdf plots generated by country? Not used if plot "png"
  new.cname.df = CME.assistant::new_cnames,   # use official country name
  n.countries = NULL,    # optional to save several specific countries. accept a numeric vector e.g. 1:5
  iso.subset.c = NULL,   # ISO country codes of countries to plot comparison for
  sort_the_isos = TRUE,  # if FALSE will just follow the order, won't resort
  NMR_metafile = NULL,   # NMR mcmc.meta object produced and saved by `transformdataforNMR`
  NMR_scale = NULL,      # "NMR" or "ratio" for NMR/U5MR, need to provide output.dir if use this option
  # HIV-removed plot for the 17 HIV countries
  HIV_removed = FALSE,
  output.dir.for.hivremoved.cqt = NULL,
  # for gender-specific plot
  gender = NULL,
  gender_ind = NULL,
  pooling_weight = "0.5", # sometimes for 5-24 need a different weight
  ... # allowing more options passed to `PlotDataAndEstimates2020`
) {
  # set directory
  # if runname is NULL, output.dir
  if (is.null(output.dir)) output.dir <- file.path(getwd(), "output", runname)
  indicator.filename <- NULL # for NMR filename

  # Load data ---------------------------------------------------------------
  if(is.null(gender)){
    if(is.null(NMR_scale) & is.null(NMR_metafile)){

      # data: U5MR, IMR, 5-24 ---------------------------------------------------
      # load mcmc.meta for U5, IMR, and 5-24

      if(!file.exists(file.path(output.dir, "mcmc.meta.rda"))){
        # allow output.dir to be the parent folder of runname,
        # It is fine if the output.dir doesn't contain the folder name
        if(!is.null(runname) & file.exists(file.path(output.dir, runname, "mcmc.meta.rda"))){
          output.dir <- file.path(output.dir, runname)
        } else {
          stop("`output.dir` must have the data file `mcmc.meta.rda`.
               Current `output.dir` is: ", output.dir)
        }
      }
      check.rda.exist(output.dir)
      load(file.path(output.dir, "mcmc.meta.rda"))
      # revise mcmc.meta for 5-24 --- 8/2020, combined into the function
      if(!is.null(ylab)){
        if(ylab %in% c("10q5", "5q5")) mcmc.meta <- remove.specific.series(mcmc.meta, remove_pattern = "Derived from 5q0|Subnational")

        if(ylab %in% c("10q15", "5q15")){
          mcmc.meta <- remove.specific.series.15_24(mcmc.meta, remove_pattern = "Derived from 5q0")
          mcmc.meta <- remove.specific.series(mcmc.meta, remove_date = 1990)
        }
      }

      # highlight new series?
      if(!is.null(new_entry_date)){
        new.sourceID.i <- get.new.series(output.dir, new_entry_date)
        mcmc.meta <- mark.new.series(mcmc.meta, new.sourceID.i, HIV_removed = HIV_removed)
      }
      # message("newentry.Lc.s is", paste(mcmc.meta$data.hivremoved.all$newentry.Lc.s[[190]], collapse = ", "))

      if(file.exists(file.path(output.dir, "year.t.rda"))) load(file.path(output.dir, "year.t.rda")) else year.t <- NULL
      if(!is.null(res.cqt1)){
        res.cqt <- res.cqt1
      } else {
        res.cqt.Lw <- get.res.cqt.rda.diffname(output.dir)
        res.cqt <- res.cqt.Lw[[as.character(pooling_weight)]] #
        if(!HIV_removed)message("res.cqt.Lw file used: ", get.res.cqt.rda.diffname(output.dir, name_only = TRUE))
      }
      # YL 1/29 HIV-removed, can load from a specific `output.dir.for.hivremoved.cqt` if supplied
      if(HIV_removed){
        if(!is.null(output.dir.for.hivremoved.cqt)){
            if(file.exists(file.path(output.dir.for.hivremoved.cqt,
                                      "res.hivremoved.cqt.Lw.rda"))){
              load(file.path(output.dir.for.hivremoved.cqt, "res.hivremoved.cqt.Lw.rda"))
            } else {
              warning("Cannot find and load the `res.hivremoved.cqt.Lw.rda`")
            }

          } else if (file.exists(file.path(output.dir, "res.hivremoved.cqt.Lw.rda"))) {
            load(file.path(output.dir, "res.hivremoved.cqt.Lw.rda"))
          } else (
            stop("Please check if `res.hivremoved.cqt.Lw.rda` is in the output directory.")
          )
        res.cqt <- res.hivremoved.cqt.Lw[[as.character(pooling_weight)]]
        message("res.cqt.Lw file used: res.hivremoved.cqt.Lw.rda")

      }

      dimnames(res.cqt)[[3]] <- year.t

      indicator.type <- mcmc.meta$settings$indicator.type
      is.validation <- mcmc.meta$settings$is.validation

      # Allowing using output.dir to search for other res.cqt series to plot 2020.12
      # if only supply `runname`, will try to guess `output.dir`; better supply `output.dir`
      # if `output.dir` is supplied, either `runname` or `legend` is also needed so we can have the legend
      # runname2
      if(!is.null(runname2)){
        if(is.null(legend2)) legend2 <- runname2
        if(is.null(output.dir2)) output.dir2 <- file.path(getwd(), "output", runname2)
      }
      if(!is.null(output.dir2)) res.cqt2 <- obtain.matched.cqt(output.dir1 = output.dir, output.dir2 = output.dir2)

      # runname 3
      if(!is.null(runname3)){
        if(is.null(legend3)) legend3 <- runname3
        if(is.null(output.dir3)) output.dir3 <- file.path(getwd(), "output", runname3)
      }
      if(!is.null(output.dir3)) res.cqt3 <- obtain.matched.cqt(output.dir1 = output.dir, output.dir2 = output.dir3)

      # runname4
      if(!is.null(runname4)){
        if(is.null(legend4)) legend4 <- runname4
        if(is.null(output.dir4)) output.dir4 <- file.path(getwd(), "output", runname4)
      }
      if(!is.null(output.dir4)) res.cqt4 <- obtain.matched.cqt(output.dir1 = output.dir, output.dir2 = output.dir4)


    } else {
      # data: NMR ---------------------------------------------------
      # for NMR plot, either supply the NMR metafile as argument, or the output.dir that stores it (then need to tell me `NMR_scale`)
      # so it is either `NMR_metafile` or `output.dir` + `NMR_scale`
      # `NMR_metafile` contains the info if it is rate or ratio
      if(!is.null(NMR_metafile)) {
        nmr <- NMR_metafile
        NMR_scale <- nmr$NMR_scale
      } else if (NMR_scale %in% c("NMR", "ratio")) {
        file_dir <- file.path(output.dir, if(NMR_scale=="NMR") "mcmc.meta.nmr.rda" else "mcmc.meta.nmr.ratio.rda")
        if(!file.exists(file_dir)) stop("Check if the NMR mcmc.meta.rda file exists.")
        nmr <- readRDS(file_dir)
      } else {
        stop("For NMR plots, either supply `NMR_metafile` or `output.dir`(where the `mcmc.meta.rda` is) + `NMR_scale`")
      }

      # NMR: construct mcmc.meta - - - - -
      # highlight new series
      if(is.null(new_entry_date)){
        # if new_entry_data is NULL, new entries not plotted even in the data
        nmr$data$newentry.Lc.s <- NULL
        nmr$data$newentryvr.Lc.s <- NULL
      } else {
        message("`new_entry_date` was set as ", nmr$new_entry_date, " in the step `transformdataforNMR`")
      }
      mcmc.meta <- list()
      mcmc.meta$data.all <- nmr$data
      mcmc.meta$data.all$C <- nmr$data$c
      mcmc.meta$data.all$iso.c <- nmr$iso
      year.t <- nmr$year.t
      res.cqt <- nmr$res.cqt
      if(!is.null(legend2)) res.cqt2 <- nmr$res2.cqt
      if(!is.null(legend3)) res.cqt3 <- nmr$res3.cqt
      if(!is.null(legend_ex)) res_ex.cqt <- nmr$res_ex.cqt
      # if(!is.null(res.cqt)) dimnames(res.cqt)[[3]] <- year.t
      indicator.type <- if(NMR_scale=="NMR") "NMR" else "NMR/U5MR Ratio"
      indicator.filename <- if(NMR_scale=="NMR") "Rate" else "Ratio"
      is.validation <- FALSE
    }

  } else {
    # data: sex-specific ---------------------------------------------------
    # for sex-specific plot: if !is.null(gender)
    file_dir <- get.mcmc.meta.dir(output.dir, gender_ind, gender)
    gender.rda <- readRDS(file_dir)

    # highlight new series (follows the same logic as NMR)
    if(is.null(new_entry_date)){
      # if new_entry_data is NULL, new entries not plotted even if marked in the data
      gender.rda$data$newentry.Lc.s <- NULL
      gender.rda$data$newentryvr.Lc.s <- NULL
    } else {
      # if you supply new_entry_data, do newentry exist in the data?
      if(is.null(gender.rda$data$newentry.Lc.s)) message("data$newentry.Lc.s is NULL, was it added correctly in `transformdataforNMR`?")
      if(new_entry_date!=gender.rda$new_entry_date) message("Note: new_entry_date used when making the data file is ", gender.rda$new_entry_date)
    }
    # build mcmc.meta for sex-specific
    mcmc.meta <- list()
    mcmc.meta$data.all <- gender.rda$data
    mcmc.meta$data.all$C <- gender.rda$data$c
    mcmc.meta$data.all$iso.c <- gender.rda$iso
    year.t <- gender.rda$year.t
    res.cqt <- gender.rda$res.cqt
    if(!is.null(legend2)) res.cqt2 <- gender.rda$res2.cqt
    if(!is.null(legend3)) res.cqt3 <- gender.rda$res3.cqt
    if(!is.null(legend_ex)) res_ex.cqt <- gender.rda$res_ex.cqt
    # adds to the title1 (gender_title ISO) -> e.g. "Female AFG"
    gender_title <- gender.rda$sex
    # for ylab:
    indicator.type <- gender.rda$indicator_label
    if(is.null(filename)){
      filename <- paste(indicator.type, gender_title, sep = "_")
    } else {
      filename <- paste(indicator.type, gender_title, filename, sep = "_") # append as extra note
    }
    # for ratio
    if(gender == "ratio"){
      gender_title <- NULL
      # e.g. "U5MR Sex Ratio"
      indicator.type <- paste(gender.rda$indicator_label, gender.rda$sex)
    }

    is.validation <- FALSE
  }



  iso.c <- mcmc.meta$data.all$iso.c
  if(HIV_removed){
    # plot all the 17 countries if iso.subset.c not correctly provided
    # n.countries is chosen from 1:17
    iso.c.hiv <- mcmc.meta$data.hivremoved.all$iso.c[mcmc.meta$data.hivremoved.all$hiv.c]
  }

  # YL: Optional to save several specific countries
  subset.iso <- function(){
    total_num_c <- mcmc.meta$data.all$C
    if(HIV_removed) total_num_c <- length(iso.c.hiv)
    if(is.numeric(n.countries)){
      numeric.c <- n.countries[n.countries %in% 1:total_num_c] # e.g. 2,3,4, limit to within the range of 1:total_C
    } else {
        numeric.c <- 1:total_num_c # otherwise, plot all available
    }
    iso.c.1 <- if(HIV_removed) iso.c.hiv[numeric.c] else iso.c[numeric.c] # 1st subset of iso
    # further subsetting by iso:
    if(!is.null(iso.subset.c) & sum(iso.c.1%in%iso.subset.c)>0){
      iso.c.1 <- iso.c.1[iso.c.1%in%iso.subset.c]
    }
    # if sort_the_isos, will follow the order, otherwise use the given order of the isos
    C <- if(sort_the_isos) which(iso.c%in%iso.c.1) else match(new_iso_order, iso.c)
    return(C) # since required by `PlotDataAndEstimates2020`
  }
  C <- subset.iso() # this capital C is a vector of numbers e.g. c(1,10,150)
  message("C is ", paste(C, collapse = ","))

  # YL: update country names
  if(is.data.frame(new.cname.df)) {
    mcmc.meta$data.all$name.c <- update.name.c(new.cname.df, mcmc.meta)
    if (HIV_removed) mcmc.meta$data.hivremoved.all$name.c <- update.name.c(new.cname.df, mcmc.meta)

  }

  # set cqt ------------------------------------------------------------
  # save a copy of the cqt plotted 2020.05
  if(save_cqt_copy){
    if(!dir.exists("figData/cqt_backup")) dir.create("figData/cqt_backup", recursive = TRUE)
    dt_cqt <- reshape2::melt(res.cqt)
    colnames(dt_cqt) <- c("ISO.Code", "Quantile", "Year", "Value")
    ylab0 <- if(is.null(ylab)) indicator.type else ylab
    ylab0 <- gsub("/", "_", ylab0)
    dt_cqt$ind_short <- ylab0
    if(is.null(gender)){
      dt_cqt$Sex <- "Total"
      saveRDS(dt_cqt, file = file.path("figData/cqt_backup", paste0("copy_of_res.cqt_", ylab0, ".rds")))
    } else {
      dt_cqt$Sex <- gender_title
      saveRDS(dt_cqt, file = file.path("figData/cqt_backup", paste0("copy_of_res.cqt_", gender_title, "_", ylab0, ".rds")))
    }
  }

  # limit years of res.cqt series by `year.start` and `year.end`
  res.cqt <- set.cqt.year.limit(res.cqt, year_start = year.start, year_end = year.end)
  res.cqt2 <- set.cqt.year.limit(res.cqt2, year_start = year.start, year_end = year.end)
  res.cqt3 <- set.cqt.year.limit(res.cqt3, year_start = year.start, year_end = year.end)
  res.cqt4 <- set.cqt.year.limit(res.cqt4, year_start = year.start, year_end = year.end)
  res_ex.cqt <- set.cqt.year.limit(res_ex.cqt, year_start = year.start, year_end = year.end)

  # if to hide the estimate line (legend as an extra switch)
  if(is.null(legend1)) res.cqt <- NULL
  if(is.null(legend2)) res.cqt2 <- NULL
  if(is.null(legend3)) res.cqt3 <- NULL
  if(is.null(legend4)) res.cqt4 <- NULL
  if(is.null(legend_ex)) res_ex.cqt <- NULL
  if(is.null(legend_WPP)) wpp.cqt <- NULL
  if(is.null(legend_IHME)) ihme.cqt <- NULL

  # for pdf size:
  plot.height <- 7
  plot.width <- (main.plot + zoom + add.legend) * plot.height

  # set filenames -----------------------------------------------------------
  # create filename from arguments
  n_series <- sum(!is.null(legend1), !is.null(legend2), !is.null(legend3),
                  !is.null(legend4), !is.null(legend_ex),
                  !is.null(wpp.cqt),
                  !is.null(ihme.cqt))
  if(After_CC) n_series <- 1
  # show.legend <- function(x){
  #   if(!is.null(x)) cat ("x is ", eval(x))
  # }
  # invisible(lapply(c(legend1, legend2, legend3, legend4,
  #                    legend_ex, legend_WPP, legend_IHME), show.legend))

  if(is.null(gender)){ # doesn't apply to sex-specific
      # U5, IMR
      if(n_series ==0) name_plot_type <- paste(runname, "Data", sep = "_")
      if(n_series ==1) name_plot_type <- paste(runname, "Results", sep = "_")
      if(n_series >1 ) name_plot_type <- paste("Results Comparison", runname, sep = "_")
      # Rate/Ratio Data / Results
      if(!is.null(indicator.filename)) name_plot_type <- paste(indicator.filename, name_plot_type)
      filename <- if(!is.null(filename)) paste(filename, name_plot_type) else name_plot_type
      if(HIV_removed)filename <- paste(filename, "(HIV-removed)")
  }

  if(!After_CC){
  filename <- paste0(filename,
                     ifelse(!is.null(legend_ex), "_vs_Expected", ""),
                     ifelse(!is.null(legend2), paste0("_vs_", gsub(" ", "", legend2)), ""),
                     ifelse(!is.null(legend3), paste0("_vs_", gsub(" ", "", legend3)), ""),
                     ifelse(!is.null(legend4), paste0("_vs_", gsub(" ", "", legend4)), "")
                     )
  }

  if(!is.null(wpp.cqt)) {
    filename <- paste0(filename, "_vs_", gsub(" ", "", legend_WPP)) # e.g. _vs_WPP2019
  }
  if(!is.null(ihme.cqt)) {
    filename <- paste0(filename, "_vs_", gsub(" ", "", legend_IHME))
  }

  message("filename is ", filename)

  # This is the complete file names for pdf and png output:
  get.file.name <- function(c){
    if (pdf.or.png=="png") file_name <-  paste0(filename, "_", iso.c[c], ".", pdf.or.png)
    if (pdf.or.png=="pdf") file_name <-  if(separate.plots.by.country) paste0(filename, "_", iso.c[c], ".", pdf.or.png) else paste0(filename, ".", pdf.or.png)
    # message("file_name is ", file_name)
    return(file_name)
  }

  # set fig.dir -------------------------------------------------------------
  if (is.null(fig.dir)) fig.dir <- file.path(getwd(), "fig")
  # add a subfolder for pngs using full runname e.g. NMR_20190807 Ratio Results
  if (pdf.or.png == "png") fig.dir <- file.path(fig.dir, if(is.null(filename)) "test" else filename)
  # message("fig.dir is ", fig.dir)
  if(!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE) # create all levels of folders using recursive


  # Call `PlotDataAndEstimates2020`-----------------------------------
  plot.by.c <- function(c){suppressMessages(
    PlotDataAndEstimates2020(data = NULL, # YL: doesn't really need mcmc.meta$data
                         data.all = if(!HIV_removed) mcmc.meta$data.all else mcmc.meta$data.hivremoved.all,
                         ylab = if(is.null(ylab)) indicator.type else ylab,
                         title1 = if(!is.null(gender)) gender_title, # add Male/Female in title
                         est.years = year.t,

                         CIs.cqt = res.cqt,
                         CIs2.cqt = res.cqt2,
                         CIs3.cqt = res.cqt3,
                         CIs4.cqt = res.cqt4,
                         CIs_ex.cqt = res_ex.cqt,
                         wpp_and_completeihme = list(wpp.cqt = wpp.cqt, ihme.cqt = ihme.cqt),
                         legendfull = if(length(legend1)==1) legend1 else legend1[c],

                         legend2 = legend2,
                         legend3 = legend3,
                         legend4 = legend4,
                         legend_ex = legend_ex,
                         legend_WPP = legend_WPP,
                         legend_IHME = legend_IHME,

                         zoom.year.start = zoom.year.start,
                         zoom.year.end = zoom.year.end,
                         year.start = year.start,
                         add.legend = add.legend,
                         c = c,

                         main.plot = main.plot,  # Include main plot?
                         zoom = zoom,       # Include zoom plot?

                         ... # passing extra arguments to `PlotDataAndEstimates2020`

                         )
  )}

  time0 <- Sys.time()
  # separate.plots.by.country only applies to PDF
  if(!pdf.or.png%in%c("pdf", "png")) {
    message("pdf.or.png is either pdf or png, set to pdf.")
    pdf.or.png <- "pdf"
  }
  if(pdf.or.png=="pdf"){
    width0 <- if(all(main.plot, zoom)) plot.width else plot.width + 2.5
    height0 <- if(all(main.plot, zoom)) plot.height else plot.height + 1.5
    # seperate pdf
    if(separate.plots.by.country){
      for (c in C) {
        pdf(file = file.path(fig.dir, get.file.name(c)), width = width0 , height = height0)
        plot.by.c(c)
        dev.off()
      }
    } else {
      # one pdf
      pdf(file = file.path(fig.dir, get.file.name(c)), width = width0 , height = height0)
      tryCatch(
        invisible(lapply(C, plot.by.c))
      )
      # for debugging
      # results <- lapply(C, function(x) try(plot.by.c(x)))
      # is.error <- function(x)inherits(x, "try-error")
      # failed <- sapply(results, is.error)
      # message("Failed: ", paste(iso.c[C[failed]], collapse = ", "))
      dev.off()
    }
    failed <- FALSE
  }
  # save png
  if(pdf.or.png == "png"){
    # seperate png
    width0 <- if(all(main.plot, zoom)) 24 else 8.5 # YL 200430
    height0 <- if(all(main.plot, zoom)) 8 else 8
    if(After_CC){
      width0 <- 22
    }
    save.png <- function(c){
      # message("save plot for iso: ", iso.c[c])
      # add a progress message
      cat('\014')
      cat(c, ":", iso.c[c], ";", paste0(round(which(C==c) / length(C) * 100), "%\n"))
      png(filename = file.path(fig.dir, get.file.name(c)), width = width0, height = height0, units="in", res=250)
      plot.by.c(c)
      dev.off()
    }
    # lapply(C, function(x) save.png(x))

    # debugging tools when run all the countries
    results <- lapply(C, function(x) try(save.png(x)))
    is.error <- function(x)inherits(x, "try-error")
    failed <- sapply(results, is.error)
    failed_isos <- iso.c[failed]
    if(any(failed)){
      message("Failed isos: ", paste(failed_isos, collapse = ", "))
    } else {
      message("All isos passed")
    }
  }
  message("Results saved to ", fig.dir)

  time_spent <- round(Sys.time() - time0, 1)
  if(any(failed)){
    return(list(filename = filename, fig.dir = fig.dir,
                C = C, isoc = iso.c,
                failed_c = C[failed],
                failed_iso = failed_isos))
  } else {
    return(list(filename = filename, fig.dir = fig.dir,
                iso.c = iso.c, C = C, iso.subset.c = iso.subset.c,
                time_spent = time_spent))
  }

  # started at 19/12/23 YL
}

