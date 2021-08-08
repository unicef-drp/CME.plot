#----------------------------------------------------------------------
# plotdataandestimates.R
# Leontine Alkema & Jin Rou New, 2012-2013
# slightly updated YL 01/2020
#----------------------------------------------------------------------

PlotDataAndEstimates2020 <- function(# Plot data, estimated fits and uncertainty intervals.
  ### Plot data, estimated fits and uncertainty intervals.
  data = NULL, ##<< \code{data} list from \code{\link{ReadData}}.
  data.all = NULL, ##<< \code{data.all} list from \code{\link{ReadData}}.
  ## If \code{NULL}, excluded series/observations are not plotted.
  c, ##<< Index of country to plot.
  est.years = NULL,##<< Estimation years for CIs.cqt.
  CIs.cqt = NULL,##<< Optional: Show CIs for country c (percentiles lower-median-upper and estimation year t).
  CIs.tr.cqt = NULL, ##<< Optional: Show training CIs?
  CIs.iid.cqt = NULL, ##<< Optional: Show old CIs?.
  CIs_ex.cqt = NULL, ## the expected series
  CIs2.cqt = NULL, ##<< Optional: Show another set of CIs?
  CIs3.cqt = NULL, ##<< Optional: Show yet another set of CIs?
  CIs4.cqt = NULL, ##<< Optional: Show yet one more set of CIs?
  legendtext = NULL, ###<<Optional: Show the most recent CIs
  legendfull  = NULL, ##<< Optional: Name in legend for CIs.
  legendtr = NULL, ##<< Optional: Name in legend for CIs.tr.cqt.
  legendiid = NULL, ##<< Optional: Name in legend for CIs.iid.cqt.
  legend_ex = NULL,
  legend2 = NULL, ##<< Optional: Name in legend for CIs2.cqt.
  legend3 = NULL, ##<< Optional: Name in legend for CIs3.cqt.
  legend4 = NULL, ##<< Optional: Name in legend for CIs4.cqt.
  legend_WPP = NULL,
  legend_IHME = NULL,
  igme = NULL,##<< Optional: Include IGME estimates? Output from \code{\link{GetIGME}}.
  igme2 = NULL,##<< Optional: Include alternative IGME estimates? Output from \code{\link{GetIGME}}.
  legendigme2 = NULL,##<< Optional: Legend for alternative IGME estimates.
  legendigmemethod = NULL, ##<< Optional: Indicate IGME method used in legend.
  legend.bty = "o",
  plot.se = TRUE, ##<< Show SEs?
  newobsPIs.ciq = NULL, ##<< Optional: Show PIs for new observations? Array of quantiles for exp(ypredicts)
  ## for country c and observation i. Output from \code{\link{ConstructOutput}}.
  PIexcl.iq = NULL, ##<< Optional. Show PIs for excluded observations?
  excluded.surveys.Lc.s = NULL, ##<< Optional: Show excluded surveys in grey.
  excludedobsandyears.Lc.i2 = NULL, ##<< Optional: Show excluded observations in grey.
  Ytr.c = NULL, ##<< Optional: Show last observation year as a vertical line in training set?
  plot.biasadjobs = FALSE, ##<< Show bias-adjusted observations?
  plot.b1adjobs = FALSE, ##<< Show bias-adjusted (for level only) observations?
  data.hivremoved.biasadjusted = NULL, ##<< Optional: Bias-adjusted data, required if \code{plot.biasadjobs}
  ## or \code{plot.b1adjobs} is \code{TRUE}.
  B.tk = NULL, ##<< Optional: Show B-splines (multiplied by constant)? Use resproject.list.c[[c]]$B.tk
  ## from \code{\link{ConstructOutput}}.
  alphayears.k = NULL, ##<< Optional: Years corresponding to B-splines and alphas, needed to show B-splines
  ## and exp(alphas) as points. Use resproject.list.c[[c]]$alphayears.k from \code{\link{ConstructOutput}}. # change JR, 20140423
  alpha.cp = NULL, ##<< Optional: Show exp(alphas) as points? Output from \code{\link{ConstructOutput}}
  knots = NULL, ##<< Optional: Show knots as dotted vertical lines? Use
  ## resproject.list.c[[c]]$uyears.q.plot from \code{\link{ConstructOutput}}.
  u5.tj = NULL, ##<< Optional: Show a sample of trajectories for a particular country for estimation year t
  ## and trajectory j.
  col.CI = "#FF000025", ##<< Optional: Colour for CIs, default is light #c7202e # change JR, 24 Sep 2013: from FF000032
  col.val = "#00FF0032", ##<< Optional: Colour for CIs.tr, default is lighter than lightgreen. # change JR, 24 Sep 2013: from #00FF0050
  col.un = "#0000FF32", ##<< Optional: Colour for CIs.iid.cqt, default is light purple
  col.CI_ex = "#BFEFFF80", ## be sure to use transparent color for lightblue1
  col.CI2 = "#00FF0032", #"#A020F032", ##<< Optional: Colour for yet another set of CIs?
  col.CI3 = "#FFA50010", # "#FFA50032", ##<< Optional: Colour for another set of CIs?
  col.CI4 = "#0000FF32", ##<< Optional: Colour for yet one more set of CIs? light purple
  col.igme = "black", ##<< Optional: Colour for IGME estimates, default black.
  col.igme2 = "darkgreen", ##<< Optional: Colour for alternative IGME estimates, default dark green.
  col.data = NULL, ##<< Optional: Colour (same for all data points) for included data
  col.data.all = NULL, ##<< Optional: Colour (same for all data points) for excluded data
  col.newobsPIs = "#FF000050", ##<< Optional: Colour for PIs for new obs, default red.
  col.biasadjobs = "#0000FF50", ##<< Optional: Colour for bias-adjusted observations, default steelblue.
  col.b1adjobs = "#60331150", ##<< Optional: Colour for bias-adjusted (for level only)
  col.CI_IHME = "#7df5ed30",    #  "lightcyan" for CI
  ## observations, default brown.
  ylab = "U5MR", ##<< y-axis label for both plots.
  ymax = NULL, ##<< Optional: User-defined ymax for first plot.
  title1 = NULL, ##<< Optional: Title for first plot.
  title2 = NULL, ##<< Optional: Title for second plot.
  main.plot = TRUE, ##<< Include main plot? # change JR, 26 Aug 2013
  year.start = NULL,
  year.end = last.year(),
  zoom = TRUE, ##<< Add zoom plot?
  add.legend = TRUE, ##<< Add legend plot?
  zoom.year.start = 1990, ##<< First year of zoom range for zoom plot.
  zoom.year.end = last.year(), ##<< Last year of zoom range for zoom plot.
  seriesnames.in.full = TRUE, ##<< Display series names in full? # change JR, 3 Sep 2013
  mfrow.suppress = FALSE, ##<< Change layout of plots with layout() argument instead of mfrow argument?
  suppress.legend.plot1 = FALSE, ##<< Suppress legend in plot 1?
  suppress.legend.plot2 = FALSE, ##<< Suppress legend in plot 2?
  cex.adj.factor = 1, ##<< Optional: Factor to adjust plot text size by
  cex.legend = 1.39, ##<< cex for the legend
  indirect_series_visibility = TRUE,    ####If false, some indirect series will be invisible due to the existence of direct data series with the same name.   Kai Zhong 05/23/2018
  wpp_and_completeihme = NULL   #####plot wpp and ihme data
  ) {
  if (main.plot + zoom == 2) {
    i.seq <- 1:2
  } else if (main.plot + zoom == 1) {
    if (main.plot)
      i.seq <- 1
    if (zoom)
      i.seq <- 2
  } else {
    cat("Error: Either main.plot or zoom must be TRUE.")
    return()
  }
  # change JR, 3 Sep 2013
  if ((main.plot + zoom + add.legend) == 3 & !seriesnames.in.full) {     #####show all three parts of plot and abbreviated name    change kai 05/07/2018
    plot.widths <- c(1.1, 1.1, 0.8)
  } else if((main.plot + zoom + add.legend) == 3 & seriesnames.in.full){     #####show all three parts of plot and complete name  change kai 05/07/2018
    plot.widths <- c(1, 1, 1)
  } else {
    if(add.legend==T){
      plot.widths <- c(1.5, 1.5)           ####each part of plot has the same width   change kai 05/07/2018
    } else {plot.widths <- rep(1.5, (main.plot + zoom + add.legend))}
  }
  if (!mfrow.suppress){
    layout(matrix(1:(main.plot + zoom + add.legend), 1, main.plot + zoom + add.legend), widths = plot.widths)
    } else {
     par(mfrow = c(1, 3))
    }

  if((main.plot+zoom+add.legend)==3){
    par(mar = c(5.5, 5.5, 5, 1), mgp = c(3.5, 1.5, 0), # change JR, 20140422
        cex.main = 2.3*cex.adj.factor, cex.axis =2*cex.adj.factor, cex.lab = 2*cex.adj.factor)     ####layout of three parts in the plot     Kai changed 05/13/2018
  } else if((main.plot+zoom+add.legend)==2){
    par(mar = c(3.8, 4.3, 3.3, 1), mgp = c(2.3, 1, 0), # change JR, 20140422
        cex.main = 1.45*cex.adj.factor, cex.axis = 1.45*cex.adj.factor, cex.lab = 1.45*cex.adj.factor)     ####layout of two random parts in the plot    Kai changed 05/13/2018
  } else { par(mar = c(3.8, 4.3, 3.3, 1), mgp = c(2.3,1, 0), # change JR, 20140422
               cex.main = 1.45*cex.adj.factor, cex.axis = 1.45*cex.adj.factor, cex.lab = 1.45*cex.adj.factor)     ####layout of one random part in the plot    Kai changed 05/13/2018
  }

  data.all=indirect_data_visibility(data=data.all, indirect_series_visibility=indirect_series_visibility)     #####whether indirect series should be visible kai added 05/22/2018


  message("mar:", paste0(par("mar"), sep= ", "))
  message("mgp:", paste0(par("mgp"), sep= ", "))

  # set xy-axis min and max range ------------------------------------------------
  for (i in i.seq) { # 1st plot is normal, 2nd plot is zoomed
    if (i == 1) {
      # main <- ifelse(!is.null(title1), title1, ifelse(!is.null(data$name.c[c]), data$name.c[c], data.all$name.c[c]))
      main_cname <- ifelse(!is.null(data$name.c[c]), data$name.c[c], data.all$name.c[c])
      main <- if(!is.null(title1)) paste(title1, main_cname) else main_cname # YL:add title1 in front like "Male/Female"

      if (!is.null(data)) {  # use data
        xmin <- ifelse(is.null(year.start), round(-2 + min(1990,
                                                           data$yearvr.Lc.j[[c]],
                                                           unlist(data$year.Lcs.j[[c]]), na.rm = T)), year.start)
        xmax <- ifelse(is.null(year.end),   round( 0 + max(est.years,
                                                           data$yearvr.Lc.j[[c]],
                                                           unlist(data$year.Lcs.j[[c]]), na.rm = T)), year.end)
        ymin <- 0
        ymax <- ifelse(is.null(ymax), min(1000, 1.1*max(CIs.cqt[c,,],
                                                       data$uvr.Lc.j[[c]],
                                                       unlist(data$u.Lcs.j[[c]]), na.rm = T), na.rm = T), ymax)

      } else if (!is.null(data.all)) {                  # use data.all
        xmin <- ifelse(is.null(year.start), round(-2 + min(1990, # just in case of no data
                                                           data.all$yearvr.Lc.j[[c]],
                                                           unlist(data.all$year.Lcs.j[[c]]),
                                                           na.rm = T)), year.start)
        xmax <- ifelse(is.null(year.end),   round( 0 + max(est.years,
                                                           data.all$yearvr.Lc.j[[c]],
                                                           unlist(data.all$year.Lcs.j[[c]]),
                                                           na.rm = T)), year.end)
        ymin <- 0
        ymax <- ifelse(is.null(ymax), min(1000, 1.1*max(CIs.cqt[c,,],
                                                       data.all$uvr.Lc.j[[c]],
                                                       unlist(data.all$u.Lcs.j[[c]]), na.rm = T), na.rm = T), ymax)
      } else {
        stop ("provice at least one of the `data` and `data.all`. ")
      }

      # consider the year of estimates for gender-specific mainly (YL2020)
      # plot from the most left-side available estimates
      if(!is.null(CIs.cqt)){
        xmin <- min(as.numeric(names(na.omit(CIs.cqt[c,1,]))), xmin)
      }

      if (!is.null(wpp_and_completeihme$wpp.cqt)){
        # YL Aug 2021: avoid too much empty space in plot since wpp or ihme can
        # go back too much beyond where we have data
       wpp_years <- as.numeric(dimnames(wpp_and_completeihme$wpp.cqt)[[3]])
       ymin <- min(ymin, wpp_and_completeihme$wpp.cqt[c,1, wpp_years> xmin], na.rm = T)
       ymax <- max(ymax, wpp_and_completeihme$wpp.cqt[c,1, wpp_years> xmin], na.rm = T)
      }
      if (!is.null(wpp_and_completeihme$ihme.cqt)) {
       ihme_years <- as.numeric(dimnames(wpp_and_completeihme$ihme.cqt)[[3]])
       ymin <- min(ymin, wpp_and_completeihme$ihme.cqt[c,1, ihme_years> xmin], na.rm = T)
       # is there CI plotted or not
       if(is.null(col.CI_IHME)){
         ymax <- max(ymax, wpp_and_completeihme$ihme.cqt[c,2, ihme_years> xmin], na.rm = T)
       } else {
         ymax <- max(ymax, wpp_and_completeihme$ihme.cqt[c,3, ihme_years> xmin], na.rm = T)

       }
      }

    } else {
      # for the zoom-in plot
      main <- ifelse(!is.null(title2), title2,
                     ifelse(main.plot, "Zoomed in",
                            ifelse(!is.null(data$name.c[c]), data$name.c[c], data.all$name.c[c])))
      xmin <- zoom.year.start
      xmax <- zoom.year.end
      if (!is.null(CIs.cqt)) {
        # adjust using the interval besides estimates
        ymin <- max(0, 0.9*min(CIs.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)
        ymax <- min(1000, 1.1*max(CIs.cqt[c, , is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)   #c(1,2) revised by YS
      } else {
        ymin <- ymax <- NULL
      }
      if (!is.null(CIs2.cqt)) {
        ymin <- min(ymin, 0.9*min(CIs2.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)
        ymax <- max(ymax, 1.1*max(CIs2.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)
      }
      if (!is.null(CIs_ex.cqt)) {
        ymin <- min(ymin, 0.9*min(CIs_ex.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)
        ymax <- max(ymax, 1.1*max(CIs_ex.cqt[c, c(1,2), is.element(floor(est.years), seq(floor(xmin), floor(xmax)))],
                                  na.rm = T), na.rm = T)
      }
      # adjust by data
      if (!is.null(data) & ylab=="U5MR") {
        ymin <- min(ymin, 0.9*min(data$uvr.Lc.j[[c]][data$yearvr.Lc.j[[c]] <= xmax
                                                     & data$yearvr.Lc.j[[c]] >= xmin],
                                  unlist(data$u.Lcs.j[[c]])[unlist(data$year.Lcs.j[[c]]) <= xmax &
                                                             unlist(data$year.Lcs.j[[c]]) >= xmin],
                                  na.rm = T), na.rm = T)
        ymax <- max(ymax, 1.1*max(data$uvr.Lc.j[[c]][data$yearvr.Lc.j[[c]] <= xmax &
                                                       data$yearvr.Lc.j[[c]] >= xmin],
                                  unlist(data$u.Lcs.j[[c]])[unlist(data$year.Lcs.j[[c]]) <= xmax &
                                                              unlist(data$year.Lcs.j[[c]]) >= xmin],
                                  na.rm = T), na.rm = T)
      }
      if (!is.null(data.all)) {
        ymin <- min(ymin, 0.9*min(data.all$uvr.Lc.j[[c]][data.all$yearvr.Lc.j[[c]] <= xmax
                                                         & data.all$yearvr.Lc.j[[c]] >= xmin],
                                  unlist(data.all$u.Lcs.j[[c]])[unlist(data.all$year.Lcs.j[[c]]) <= xmax &
                                                                  unlist(data.all$year.Lcs.j[[c]]) >= xmin],
                                  na.rm = T), na.rm = T)
        ymax <- max(ymax, 1.1*max(data.all$uvr.Lc.j[[c]][data.all$yearvr.Lc.j[[c]] <= xmax &
                                                           data.all$yearvr.Lc.j[[c]] >= xmin],
                                  unlist(data.all$u.Lcs.j[[c]])[unlist(data.all$year.Lcs.j[[c]]) <= xmax &
                                                                  unlist(data.all$year.Lcs.j[[c]]) >= xmin],
                                  na.rm = T), na.rm = T)
      }


      # compare the max and min value with the value in ihme and wpp data
      # for the zoom-in part
      if (!is.null(wpp_and_completeihme$wpp.cqt)){
        # YL Aug 2021: which is not different from the main plot
        ymin <- min(ymin, wpp_and_completeihme$wpp.cqt[c,1, wpp_years> xmin], na.rm = T)
        ymax <- max(ymax, wpp_and_completeihme$wpp.cqt[c,1, wpp_years> xmin], na.rm = T)
      }
      if (!is.null(wpp_and_completeihme$ihme.cqt)) {
        ymin <- min(ymin, wpp_and_completeihme$ihme.cqt[c,1, ihme_years> xmin], na.rm = T)
        ymax <- max(ymax, wpp_and_completeihme$ihme.cqt[c,3, ihme_years> xmin], na.rm = T)
      }

    } # end i loop

    # YL: print blank for data plot without data. 1/22/2020
    if(ymax == -Inf) {
      plot(1, ylab = ylab, main = main, xlab = "Year", xlim = c(xmin, xmax), ylim = c(0, 10), type = "n")
      # legend("topright", legend = "There is no data to plot ",
      #        cex = ifelse((main.plot+zoom+add.legend)==3, 1.4, 0.9))
      return()
      }

    plot(1, ylab = ylab, main = main, xlab = "Year", xlim = c(xmin, xmax), ylim = c(ymin, ymax), type = "n")

    # plot data ---------------------------------------------------------------
    message("newentry.Lc.s is", paste(data.all$newentry.Lc.s[[c]], collapse = ", "))

    if(any(is.finite(unique(unlist(data.all$u.Lcs.j[[c]])))) | any(is.numeric(unique(unlist(data.all$uvr.Lcs.j[[c]])))))
    # if(is.null(data.all$has.no.data[[c]])) # if has data
      {

    if (plot.se) {
      # add survey and VR SEs # change JR, 1 Jun
      if (!is.null(data.all)) {
        AddSurveyData(u.Ls.i = data.all$u.Lcs.j[[c]],
                      year.Ls.i = data.all$year.Lcs.j[[c]],
                      se.Ls.i = data.all$se.Lcs.j[[c]],
                      included.Ls.i = data.all$included.Lcs.j[[c]],
                      plot.se = TRUE, plot.data.excl = TRUE,
                      seriesnames.in.full = seriesnames.in.full,
                      lwd = 2*cex.adj.factor)
        if(!is.null(unlist(data.all$sevr.Lcs.j[[c]]))){
         AddVRData(u.Ls.i = data.all$uvr.Lcs.j[[c]],
                  year.Ls.i = data.all$yearvr.Lcs.j[[c]],
                  se.Ls.i = data.all$sevr.Lcs.j[[c]],
                  included.Ls.i = data.all$includedvr.Lcs.j[[c]],
                  source.s = data.all$sourcevr.Lc.s[[c]],
                  plot.se = TRUE, plot.data.excl = TRUE,
                  seriesnames.in.full = seriesnames.in.full,
                  lwd = 2*cex.adj.factor)
        }
      } else {
        AddSurveyData(u.Ls.i = data$u.Lcs.j[[c]],
                      year.Ls.i = data$year.Lcs.j[[c]],
                      se.Ls.i = data$se.Lcs.j[[c]],
                      included.Ls.i = data$included.Lcs.j[[c]],
                      plot.se = TRUE, plot.data.excl = TRUE,
                      seriesnames.in.full = seriesnames.in.full,
                      lwd = 2*cex.adj.factor)
        if(!is.null(unlist(data.all$sevr.Lcs.j[[c]]))){
        AddVRData(u.Ls.i = data$uvr.Lcs.j[[c]],
                  year.Ls.i = data$yearvr.Lcs.j[[c]],
                  se.Ls.i = data$sevr.Lcs.j[[c]],
                  included.Ls.i = data$includedvr.Lcs.j[[c]],
                  source.s = data$sourcevr.Lc.s[[c]],
                  plot.se = TRUE, plot.data.excl = TRUE,
                  seriesnames.in.full = seriesnames.in.full,
                  lwd = 2*cex.adj.factor)
        }
      }
    }

    if (!is.null(data.all)) {
      for (plot.data.excl in c(TRUE, FALSE)) {
        res <- AddSurveyData(u.Ls.i = data.all$u.Lcs.j[[c]],
                             year.Ls.i = data.all$year.Lcs.j[[c]],
                             included.Ls.i = data.all$included.Lcs.j[[c]],
                             col.s = rep(col.data.all, data.all$nseriesnonvr.c[c]), # change JR, 20140527
                             sourcetype.s = data.all$sourcetype.Lc.s[[c]],
                             method.s = data.all$method.Lc.s[[c]],
                             surveyyear.s = data.all$seriesyear.Lc.s[[c]],
                             source.s = data.all$source.Lc.s[[c]],
                             hasbias.s = data.all$hasbias.Lc.s[[c]], # change JR, 20140429
                             newentry.s = data.all$newentry.Lc.s[[c]],

                             plot.data.excl = plot.data.excl,
                             seriesnames.in.full = seriesnames.in.full,
                             lwd = 2*cex.adj.factor,cex=ifelse((main.plot+zoom+add.legend)==3,1.8,1.3))    #####add cex here to control the size of point  Kai added 05/21/2018
      }
    } else {
      res <- AddSurveyData(u.Ls.i = data$u.Lcs.j[[c]],
                           year.Ls.i = data$year.Lcs.j[[c]],
                           included.Ls.i = data$included.Lcs.j[[c]],
                           col.s = rep(col.data, data$nseriesnonvr.c[c]), # change JR, 20140527
                           sourcetype.s = data$sourcetype.Lc.s[[c]],
                           method.s = data$method.Lc.s[[c]],
                           surveyyear.s = data$seriesyear.Lc.s[[c]],
                           source.s = data$source.Lc.s[[c]],
                           newentry.s = data$newentry.Lc.s[[c]],

                           hasbias.s = data$hasbias.Lc.s[[c]], # change JR, 20140429
                           seriesnames.in.full = seriesnames.in.full,
                           lwd = 2*cex.adj.factor,cex=ifelse((main.plot+zoom+add.legend)==3,1.8,1.3))    #####add cex here to control the size of point  Kai added 05/21/2018
    }
    # message("res$legendbg.s: ", paste(res$legendbg.s, sep = ", "))
    legendtext.s <- res$legendtext.s
    legendcol.s <- res$legendcol.s
    legendbg.s <- res$legendbg.s
    legendpch.s <- res$legendpch.s

    if (plot.b1adjobs & !is.null(data.hivremoved.biasadjusted)) {
      # add bias-adjusted (for level only) survey data series
      AddSurveyData(u.Ls.i = data.hivremoved.biasadjusted$ub1adj.Lcs.j[[c]],
                    year.Ls.i = data$year.Lcs.j[[c]],
                    sourcetype.s = data$sourcetype.Lc.s[[c]],
                    method.s = data$method.Lc.s[[c]],
                    surveyyear.s = data$seriesyear.Lc.s[[c]],
                    hasbias.s = data$hasbias.Lc.s[[c]], # change JR, 20140429
                    col.s = rep(col.b1adjobs, data$nseriesnonvr.c[c]),
                    plot.se = FALSE,
                    seriesnames.in.full = seriesnames.in.full,
                    lwd = 2*cex.adj.factor,cex=ifelse((main.plot+zoom+add.legend)==3,1.8,1.3))       #####add cex here to control the size of point  Kai added 05/21/2018
    }
    if (plot.biasadjobs & !is.null(data.hivremoved.biasadjusted)) {
      # add bias-adjusted survey data series
      AddSurveyData(u.Ls.i = data.hivremoved.biasadjusted$ubiasadj.Lcs.j[[c]],
                    year.Ls.i = data$year.Lcs.j[[c]],
                    sourcetype.s = data$sourcetype.Lc.s[[c]],
                    method.s = data$method.Lc.s[[c]],
                    surveyyear.s = data$seriesyear.Lc.s[[c]],
                    hasbias.s = data$hasbias.Lc.s[[c]], # change JR, 20140429
                    col.s = rep(col.biasadjobs, data$nseriesnonvr.c[c]),
                    plot.se = FALSE,
                    seriesnames.in.full = seriesnames.in.full,
                    lwd = 2*cex.adj.factor,cex=ifelse((main.plot+zoom+add.legend)==3,1.8,1.3))       #####add cex here to control the size of point  Kai added 05/21/2018
    }
    if (!is.null(newobsPIs.ciq)) { # add PIs for new observations
      year.i <- c(unlist(data$year.Lcs.j[[c]]), unlist(data$yearvr.Lc.j[[c]]))
      for(y in 1:length(year.i)) {
        segments(year.i[y], newobsPIs.ciq[c, y, 1], year.i[y], newobsPIs.ciq[c, y, 3],
                 col = col.newobsPIs)
      }
    }
    if (!is.null(PIexcl.iq)) { # add PIs for excluded observations
      segments(excludedobsandyears.Lc.i2[[c]][1:length(PIexcl.iq[,1]), 2], PIexcl.iq[, 1],
               excludedobsandyears.Lc.i2[[c]][1:length(PIexcl.iq[,1]), 2], PIexcl.iq[, 3],
               col = "darkgrey", lwd = 3*cex.adj.factor)
    }
    # plot a sample of trajectories
    if (!is.null(u5.tj)) {
      traj.colors <- rainbow(20)
      for (traj in 1:20) {
        j <- sample(1:ncol(u5.tj), 20)[traj]
        lines(u5.tj[, j] ~ est.years, col = traj.colors[traj], lwd = 1*cex.adj.factor)
      }
    }
    if (!is.null(B.tk) & !is.null(alphayears.k)) { # add B-splines
      # scaling.bsplines <- min(50, round(min(CIs.cqt[c, 2, ], na.rm = T), digits = -1))
      # for (k in 1:length(alphayears.k)) {
      #   lines(scaling.bsplines*B.tk[,k] ~ est.years, type= "l", col = k, lwd = 2*cex.adj.factor)
      # }
      # change JR, 20140424: splines for which coeffs are pooled are dashed
      maxyear <- unique(c(data$maxyear.c[c], data.all$maxyear.c[c]))
      K <- which(alphayears.k == maxyear + 1.5*unique(diff(alphayears.k)))
      lty.k <- c(rep(1, K-1), rep(2, length(alphayears.k)-K+1))
      scaling.bsplines <- min(50, round(min(CIs.cqt[c, 2, ], na.rm = T), digits = -1))
      for (k in 1:length(alphayears.k)) {
        lines(scaling.bsplines*B.tk[,k] ~ est.years, type= "l", col = k, lwd = 2*cex.adj.factor, lty = lty.k[k])
      }
    }
    if (!is.null(knots)) { # add knots
      for(k in 1:length(knots)) {
        abline(v = knots[k], col = "grey", lty = 2, lwd = 1*cex.adj.factor)
      }
      #text(x = knots, y = 0, labels = 1:length(knots))
    }


    # plot data - VR ----------------------------------------------------------
    # YL 1/8 added checking uvr exists in three places where `AddVRData` is called
    if (!is.null(data.all)  & !is.null(unlist(data.all$uvr.Lcs.j[[c]]))) {
      for (plot.data.excl in c(TRUE, FALSE)) {
        resvr <- AddVRData(u.Ls.i = data.all$uvr.Lcs.j[[c]],
                           year.Ls.i = data.all$yearvr.Lcs.j[[c]],
                           included.Ls.i = data.all$includedvr.Lcs.j[[c]],
                           col.s = rep(col.data.all, data.all$nseriesvr.c[c]), # change JR, 20140527
                           hasbias.Ls.i = data.all$hasbiasvr.Lcs.j[[c]], # change JR, 20140429
                           isincomplete.Ls.i = data.all$isincompletevr.Lcs.j[[c]], # change JR, 20140429
                           source.s = data.all$sourcevr.Lc.s[[c]],
                           newentry.s = data.all$newentryvr.Lc.s[[c]],

                           plot.data.excl = plot.data.excl,
                           seriesnames.in.full = seriesnames.in.full,
                           lwd = 2*cex.adj.factor,
                           cex=ifelse((main.plot+zoom+add.legend)==3,2,1.4))     #####add cex here to control the size of point  Kai added 05/21/2018
      }
    } else {
      resvr <- AddVRData(u.Ls.i = data$uvr.Lcs.j[[c]],
                         year.Ls.i = data$yearvr.Lcs.j[[c]],
                         included.Ls.i = data$includedvr.Lcs.j[[c]],
                         col.s = rep(col.data.all, data$nseriesvr.c[c]), # change JR, 20140527
                         hasbias.Ls.i = data$hasbiasvr.Lcs.j[[c]], # change JR, 20140429
                         isincomplete.Ls.i = data$isincompletevr.Lcs.j[[c]], # change JR, 20140429
                         source.s = data$sourcevr.Lc.s[[c]],
                         newentry.s = data$newentryvr.Lc.s[[c]],

                         seriesnames.in.full = seriesnames.in.full,
                         lwd = 2*cex.adj.factor,
                         cex=ifelse((main.plot+zoom+add.legend)==3,2,1.4))         #####add cex here to control the size of point  Kai added 05/21/2018
    }

    legendtext.s <-  gsub('(.{86})(\\s)', '\\1\n',c(resvr$legendtext.s, legendtext.s))

    # message("resvr$legendbg.s: ", paste(resvr$legendbg.s, sep = ", "))
    legendcol.s <- c(resvr$legendcol.s, legendcol.s)
    legendbg.s <- c(resvr$legendbg.s, legendbg.s)
    legendpch.s <- c(resvr$legendpch.s, legendpch.s)
    if (!is.null(excludedobsandyears.Lc.i2[[c]])) { # add excluded observations and years
      points(excludedobsandyears.Lc.i2[[c]][,1] ~ excludedobsandyears.Lc.i2[[c]][,2],
             col = "white", bg = "black", pch = 21, lwd = 3*cex.adj.factor, cex = 0.8)
    }
    if (!is.null(excluded.surveys.Lc.s)) { # add excluded surveys
      if (!is.null(excluded.surveys.Lc.s[[paste(name.c[c])]])) {
        for (s in excluded.surveys.Lc.s[[paste(name.c[c])]]) {
          u <- data$u.Lcs.j[[c]][[s]]
          year <- data$year.Lcs.j[[c]][[s]]
          nobs <- length(u)
          points(u ~ year, pch = 19, col = "darkgrey", lwd = 5*cex.adj.factor)
          lines(u ~ year, col = darkgrey, lwd = 3*cex.adj.factor)
        }
      }
    }
    } else {
      add.legend = FALSE
    }


    message("row 482 xmin is " , xmin)
    message("row 482 ymin is " , ymin)
    #
    message("year start " , year.start)
    message("year end " , year.end)

    # plotCIs ---------------------------------------------
    # Plot CI for IHME
    if (!is.null(wpp_and_completeihme$ihme.cqt)){
      # don't know what these are for
      # if(xmin!=1990){
      #   completeihmeyear.t = c(as.numeric(dimnames(wpp_and_completeihme$ihme.cqt)[[3]]))
      # } else {
      #   completeihmeyear.t = seq(1989, floor(max(as.numeric(dimnames(wpp_and_completeihme$ihme.cqt)[[3]]))),1)+0.5
      # }
      # message("completeihmeyear.t: ", paste(range(completeihmeyear.t), collapse = "-"))
      # ihmeyearminlocation = which(as.numeric(dimnames(wpp_and_completeihme$ihme.cqt)[[3]]) == min(completeihmeyear.t))  #####get the min year location in IHME
      # ihmeyearmaxlocation = which(as.numeric(dimnames(wpp_and_completeihme$ihme.cqt)[[3]]) == max(completeihmeyear.t))  #####get the max year location in IHME

      # beware that doing things like wpp_and_completeihme$ihme.cqt[,,ihmeyearminlocation:ihmeyearmaxlocation]
      # will lose one dimention of the array
      PlotCIs(c = c,
              # CIs.cqt = wpp_and_completeihme$ihme.cqt[,,ihmeyearminlocation:ihmeyearmaxlocation,
              #                                         drop = FALSE], # YL 2021.07 add `drop = FALSE` to keep array dimension for one-country run, avoid dimension lose!
              CIs.cqt = wpp_and_completeihme$ihme.cqt,
              year.t = ihme_years,
              col.est = "steelblue", col.CI = col.CI_IHME)    ####plot CIs for ihme gbd 2016
    }


    if (!is.null(CIs4.cqt)) # darkred "#c7202e"
      PlotCIs(c = c, CIs.cqt = CIs4.cqt, year.t = est.years, col.est = "#c7202e", col.CI = col.CI4)
    if (!is.null(CIs3.cqt))
      PlotCIs(c = c, CIs.cqt = CIs3.cqt, year.t = est.years, col.est = "sienna2", col.CI = col.CI3)
    if (!is.null(CIs2.cqt)){
      PlotCIs(c = c, CIs.cqt = CIs2.cqt, year.t = est.years, col.est =
                "green", col.CI = col.CI2)
    }
    if (!is.null(CIs_ex.cqt)){
      PlotCIs(c = c, CIs.cqt = CIs_ex.cqt, year.t = est.years, col.est =
                "blue", col.CI = col.CI_ex)
    }
    if (!is.null(CIs.iid.cqt))
      PlotCIs(c = c, CIs.cqt = CIs.iid.cqt, year.t = est.years, col.est = "steelblue", col.CI = col.un)
    if (!is.null(CIs.tr.cqt))
      PlotCIs(c = c, CIs.cqt = CIs.tr.cqt, year.t = est.years, col.est = "green", col.CI = col.val)
    if (!is.null(CIs.cqt))
      PlotCIs(c = c, CIs.cqt = CIs.cqt, year.t = est.years, col.est = "red", col.CI = col.CI)


    # plot lines ---------------------------------------------
    # if (!is.null(wpp_and_completeihme$wpp.cqt)){
    #   # wppyear.t = c(seq((max(ceiling(xmin*2/10)*5-2,
    #   #                        min(as.numeric(dimnames(wpp_and_completeihme$wpp.cqt)[[3]])))),year.end+0.5,5))
    #   wppyear.t <- as.numeric(dimnames(wpp_and_completeihme$wpp.cqt)[[3]])
    #   wppyearlocation = which(as.numeric(dimnames(wpp_and_completeihme$wpp.cqt)[[3]]) == min(wppyear.t))
    #
    #   message("wppyear.t is", paste(wppyear.t, collapse = ", "))
    #   message("wppyearlocation is", paste(wppyearlocation, collapse = ", "))
    #
    # }
    if (!is.null(wpp_and_completeihme$wpp.cqt)){
      # 2nd dimension is `1` since WPP doesn't have CI
      lines(wpp_and_completeihme$wpp.cqt[c,1,] ~ wpp_years, col = "olivedrab", lty = 1, lwd = 5*cex.adj.factor)  ####line plot for wpp
    }
    if (!is.null(wpp_and_completeihme$ihme.cqt)){
      lines(wpp_and_completeihme$ihme.cqt[c,2,] ~ ihme_years, col = "steelblue", lty = 1, lwd = 5*cex.adj.factor)    #####line plot for ihme
    }
    if (!is.null(igme)) # add IGME fit # added before CIs
      lines(igme$u.ct[c,] ~ igme$t, col = col.igme, lty = 1, lwd = 5*cex.adj.factor) # change JR, 20140423
    if (!is.null(igme2)) # add alternative IGME fit # change JR, 20140423: order of igme and igme2 plotting swapped
      lines(igme2$u.ct[c,] ~ igme2$t, col = col.igme2, lty = 2, lwd = 5*cex.adj.factor) # change JR, 20140423
    if (!is.null(CIs4.cqt))
      lines(CIs4.cqt[c, 2, ] ~ est.years, col = "#c7202e", lwd = 5*cex.adj.factor)
    if (!is.null(CIs3.cqt))
      lines(CIs3.cqt[c, 2, ] ~ est.years, col = "sienna2", lwd = 5*cex.adj.factor)
    if (!is.null(CIs2.cqt)){
      # message(paste(range(as.numeric(dimnames(CIs2.cqt)[[3]])), collapse = " - "))
      # message(length(est.years), "est.year range is ", paste(range(est.years), collapse = " - "))
      lines(CIs2.cqt[c, 2, ] ~ est.years, col = "green", lwd = 5*cex.adj.factor)
    }
    if (!is.null(CIs_ex.cqt)) # dotted line
      lines(CIs_ex.cqt[c, 2, ] ~ est.years, col = "blue", lty = 2, lwd = 5*cex.adj.factor)
    if (!is.null(CIs.iid.cqt))
      lines(CIs.iid.cqt[c, 2, ] ~ est.years, col = "steelblue", lwd = 5*cex.adj.factor)
    if (!is.null(CIs.tr.cqt))
      lines(CIs.tr.cqt[c, 2, ] ~ est.years, col = "green", lwd = 5*cex.adj.factor)
    if (!is.null(CIs.cqt))
      lines(CIs.cqt[c, 2, ] ~ est.years, col = "red", lwd = 5*cex.adj.factor)
    if (!is.null(Ytr.c)) # add vertical line for last observation year
      abline(v = Ytr.c[c])
    if (!is.null(alpha.cp) & !is.null(alphayears.k)){ # add exp(alphas) # change JR, 20140423: change alphayears.k.plot to alphayears.k
      if(all(main.plot,zoom)==TRUE){
        points(exp(alpha.cp[c, alpha.cp[c, ] != 0]) ~ alphayears.k, type = "p", pch = 1, cex = 1.5*cex.adj.factor) # change JR, 20140423: change alphayears.k.plot to alphayears.k
      } else {
        points(exp(alpha.cp[c, alpha.cp[c, ] != 0]) ~ alphayears.k, type = "p", pch = 1, cex = 0.9*cex.adj.factor) # change Kai, 20180513
      }
    }


    # add legends -------------------------------------------------------------
    if (!( (i == 1 & suppress.legend.plot1) | (i == 2 & suppress.legend.plot2) )) {
      if (is.null(legendigmemethod))
        legendigmemethod <- "UN IGME 2012"
      if (is.null(legendigme2))
        legendigme2 <- "Default Loess"

      if (!is.null(igme) & !is.null(igme2)) {
        legend("bottomleft", legend = c(legendigmemethod, legendigme2), col = c(col.igme, col.igme2), # change JR, 20140423
               lty = c(1,2), lwd = 3, bty = legend.bty, cex = cex.legend*cex.adj.factor) # change JR, 20140423
      } else if (!is.null(igme)) {
        legend("bottomleft", legend = legendigmemethod, col = col.igme, lty = 1, lwd = 3, bty = legend.bty, # change JR, 20140423
               cex = cex.legend*cex.adj.factor)
      } else if (!is.null(igme2)) {
        legend("bottomleft", legend = legendigme2, col = col.igme2, lty = 2, lwd = 3, bty = legend.bty, # change JR, 20140423
               cex = cex.legend*cex.adj.factor)
      }


      if (!is.null(CIs.cqt) | !is.null(legendfull)) {
        # if these cqts are used (not used for now)
        if ((!is.null(CIs.tr.cqt) | !is.null(legendtr)) + (!is.null(CIs.iid.cqt) | !is.null(legendiid)) == 2) {
          message("legend condition 1")
          legend.text=legendtext
          if(is.null(legend.text))
          legend.text <- legendfull
          if (!is.null(legend_ex))
            legend.text <- c(legend.text, legend_ex)
          if (!is.null(legend2))
            legend.text <- c(legend.text, legend2)
          if (!is.null(legend3))
            legend.text <- c(legend.text, legend3)
          if (!is.null(legend4))
            legend.text <- c(legend.text, legend4)

          legend.col <- c("red")
          if (!is.null(legend_ex))
            legend.col <- c(legend.col, "blue")
          if (!is.null(legend2))
            legend.col <- c(legend.col, "green")
          if (!is.null(legend3))
            legend.col <- c(legend.col, "sienna3")
          if (!is.null(legend4))
            legend.col <- c(legend.col, "#c7202e")

          # put the legend:
          legend("topright", legend = legend.text, col = legend.col, lty = 1, lwd = 3, bty = legend.bty, cex = ifelse((main.plot+zoom+add.legend)==3,1.4,0.9))
        } else if ((!is.null(CIs.tr.cqt) | !is.null(legendtr)) +
                   (!is.null(CIs.iid.cqt) | !is.null(legendiid)) == 1) {
          message("legend condition 2")
          if (!is.null(CIs.tr.cqt) | !is.null(legendtr))
            legend("topright", legend = c(ifelse(is.null(legendfull), "B3", legendfull),
                                            ifelse(is.null(legendtr), "Training", legendtr)),
                   bty = legend.bty,
                   col = c("red", "green"), lty = 1, lwd = 3, cex = ifelse((main.plot+zoom+add.legend)==3,1.4,0.9))   #set the size of legend    change Kai, 20180308
          if (!is.null(CIs.iid.cqt) | !is.null(legendiid))
            legend("topright", legend = c(ifelse(is.null(legendfull), "B3", legendfull),
                                            ifelse(is.null(legendiid), "Old UIs", legendiid)),
                   bty = legend.bty,
                   col = c("red", "steelblue"), lty = 1, lwd = 3, cex = ifelse((main.plot+zoom+add.legend)==3,1.4,0.9))    #set the size of legend    change Kai, 20180308
        } else {
          # where there is nothing special:
          message("legend condition normal, line 673")
          legend.text <- legendtext
          legend.col <- c("red")
          legend.lty <- 1L
          if(is.null(legend.text)){
            legend.text <- c(ifelse(is.null(legendfull), "B3", legendfull))

          }
          if (!is.null(legend_ex)) {
            legend.text <- c(legend.text, legend_ex)
            legend.col <- c(legend.col, "blue")
            legend.lty <- c(legend.lty, 2L) # dotted line

          }

          if (!is.null(legend2)) {
            legend.text <- c(legend.text, legend2)
            legend.col <- c(legend.col, "green")
            legend.lty <- c(legend.lty, 1L)

          }

          if (!is.null(legend3)) {
            legend.text <- c(legend.text, legend3)
            legend.col <- c(legend.col, "sienna2")
            legend.lty <- c(legend.lty, 1L)

            }
          if (!is.null(legend4)) {
            legend.text <- c(legend.text, legend4)
            legend.col <- c(legend.col, "#c7202e")
            legend.lty <- c(legend.lty, 1L)

          }
          if (!is.null(wpp_and_completeihme$wpp.cqt)){
            legend.text <- c(legend.text, legend_WPP)
            legend.col <- c(legend.col, "olivedrab")
            legend.lty <- c(legend.lty, 1L)

          }
          if (!is.null(wpp_and_completeihme$ihme.cqt)){
            legend.text <- c(legend.text, legend_IHME)
            legend.col <- c(legend.col, "steelblue")
            legend.lty <- c(legend.lty, 1L)
          }
          legend("topright", legend = legend.text,
                 bty = legend.bty,
                 col = legend.col, lty = legend.lty, lwd = 2,
                 # YL 2020, assume always wants legend?
                 cex = if(zoom) 1.4 else 1)
        }
      }
    } # end if(!(i = 1 & suppress.legend.plot1)) | !(i = 2 & suppress.legend.plot2)) loop
  } # end i plots loop
  if (add.legend)
    PlotLegend(legendtext.s = legendtext.s, legendcol.s = legendcol.s,
               legendbg.s = legendbg.s, legendpch.s = legendpch.s,
               # cex = ifelse(all(zoom, main.plot)==TRUE,
               #              cex.legend*cex.adj.factor*ifelse(seriesnames.in.full, 1, 1.3),
               #              cex.legend*cex.adj.factor*0.67),    ####Kai changed 05/13/2018
               cex = cex.legend*cex.adj.factor*ifelse(seriesnames.in.full, 1, 1.3),
               seriesnames.in.full = seriesnames.in.full,
               pt.cex=ifelse((main.plot+zoom+add.legend)==3,2,1.1))     #set the size of legend    change Kai, 20180308
  # revised version YL Jan.2020
}
#----------------------------------------------------------------
AddSurveyData <- function(# Add survey data and/or sampling errors to plot
  u.Ls.i,
  year.Ls.i,
  se.Ls.i = NULL,
  included.Ls.i = NULL, # change JR, 3 Jun
  source.s = NULL, # change JR, 3 Jun
  sourcetype.s = NULL,
  method.s = NULL,
  surveyyear.s = NULL,
  hasbias.s = NULL, # change JR, 20140429
  col.s = NULL,
  newentry.s = NULL,
  plot.se = FALSE,
  plot.data.excl = FALSE,
  plot.newobsPIs = FALSE,
  plot.points = TRUE,
  lwd = 2,
  cex = 1.8,     ###add cex option   kai added 05/14/2018
  seriesnames.in.full = TRUE # change JR, 3 Sep 2013
) {
  # u.Ls.i[grepl("Derived from 5q0", source.s)] <- NULL
  # year.Ls.i[grepl("Derived from 5q0", source.s)] <- NULL
  # change JR, 10 May 2013: added select.surveys
  select.surveys <- !sapply(year.Ls.i, is.null)
  nsurveys <- sum(select.surveys)
  if (nsurveys == 0 ) return()
  if (!is.null(surveyyear.s)) {
    # re-order survey order, `surveys` is a ordering vector: e.g. for AFG:
    # 11  1 13 12  2 14 16 15  5  4  3  6  7 18 17 19  8  9 20 10
    surveys <- rev(order(as.numeric(substring(surveyyear.s[select.surveys], first = 1, last = 4))))

  } else {
    surveys <- seq(1, nsurveys)
  }
  if (is.null(col.s)) {
    col.palette <- c(RColorBrewer::brewer.pal(12, "Paired")[c(2, 4, 10, 6, 8)],
                     RColorBrewer::brewer.pal(8, "Dark2")[4],
                     RColorBrewer::brewer.pal(12, "Paired")[c(1, 3, 5, 7, 9)]) # change JR, 24 Sep 2013
    col.s <- rep(col.palette, ceiling(nsurveys/length(col.palette)))[1:nsurveys]
    message("AddSurvey: col.s:", paste(col.s, collapse = ", "))
  }
  legendpch.si <- rep(NA, nsurveys)
  legendtext.si <- rep("", nsurveys)

  # set legend text color in the right order for new entry: YL 2020
  col.newentry.s <- NULL
  if(!is.null(newentry.s)){
    col.newentry.s <- rep("black", nsurveys)
    newentry.s.selected <- newentry.s[select.surveys][surveys] # important to reorder
    col.newentry.s[newentry.s.selected == 1] <- "#c7202e"
  }
 # cat("AddSurveyData: col.newentry.s:", paste(col.newentry.s, collapse = ", "), "\n")
 # trim trailing white space in source.s and add by myself (YL)
 trim.trailing <- function (x) sub("\\s+$", "", x)

 if (is.null(source.s) | !seriesnames.in.full) { # change JR, 4 Jun
    legendtext.si <- paste0(sourcetype.s, " ", method.s, " ",
                            surveyyear.s)[select.surveys][surveys] # change JR, 3 Sep 2013
  } else {
    sep0 <- if(is.null(method.s)) "" else " " # YL: avoid the extra space, e.g. (DHS ) in NMR plot
    legendtext.si <- paste0(trim.trailing(source.s), " ", surveyyear.s, " (",  sourcetype.s,
                            sep0,
                            method.s, ")")[select.surveys][surveys]
  }
  if (!is.null(hasbias.s)) {
    hasbias.si <- hasbias.s[select.surveys][surveys]
  } else {
    hasbias.si <- rep(0, nsurveys)
  }

  # note: s in data and legend/col now refer to different ordering
  si <- 0
  for (s in surveys) {
    si <- si+1
    u <- u.Ls.i[select.surveys][[s]]
    nobs <- length(u)
    year <- year.Ls.i[select.surveys][[s]]
    if (!is.null(se.Ls.i)) {
      se <- se.Ls.i[select.surveys][[s]]
    } else {
      se <- rep(NA, nobs)
    }
    if (!is.null(included.Ls.i)) {
      included <- included.Ls.i[select.surveys][[s]] # change JR, 3 Jun
    } else {
      included <- rep(1, nobs)
    }
    legendpch.si[si] <- ifelse(hasbias.si[si] == 1, # change JR, 20140429
                               ifelse(sum(included == 1) > 0, 18, 5),
                               ifelse(sum(included == 1) > 0, 19, 1))
    if (!plot.data.excl) {
      # u <- u[included == 1]
      # year <- year[included == 1]
      # se <- se[included == 1]
      # included <- included[included == 1]
      ### if there's no excluded in the series
      if(prod(unlist(included))!=0){ # if there's no exclusion in the series
        if (nobs == 1) {
          message("nobs == 1")
          if (plot.se) {
            # segments(year, u-2*se, year, u+2*se, col = col.s[si])
            segments(year, u-2*se, year, u+2*se, col = "grey")
          } else {
            points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                   bg = ifelse(included == 1, col.s[si], "white"),
                   col = col.s[si], lwd = lwd, cex = cex)
          }
        } else {
          if (plot.se){
            for (obs in 2:nobs){
              polygon(c(year[obs-1],year[obs-1],year[obs],year[obs],year[obs-1]),
                      c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                        u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]),
                      col = adjustcolor("grey", alpha.f = 0.2), border = NA)
            }
          } else {
            lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))
            if (plot.points)
              points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                     bg = ifelse(included == 1, col.s[si], "white"),
                     col = col.s[si], lwd = lwd, cex = cex)
          }
        }} else{ #if there's exclusion in the series
          sub=splitseries(u=u,inclusion = included,year=year)
          sub_u=sub$sub_u
          sub_year=sub$sub_year

          for (i in 1:length(sub_u)){
            year=sub_year[[i]]
            u=sub_u[[i]]


            if (nobs == 1) {
              if (plot.se) {
                segments(year, u-2*se, year, u+2*se, col = "grey")
              } else {
                points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                       #bg = ifelse(included == 1, col.s[si], "white"),
                       bg = col.s[si],
                       col = col.s[si], lwd = lwd, cex = cex)
              }
            } else {
              if (plot.se){
                for (obs in 2:nobs){
                  polygon(c(year[obs-1],year[obs-1],year[obs],year[obs],year[obs-1]),
                          c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                            u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]),
                          col = adjustcolor("grey", alpha.f = 0.2), border = NA)
                }
              } else {
                lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))
                if (plot.points)
                  points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                         #bg = ifelse(included == 1, col.s[si], "white"),
                         bg = col.s[si],
                         col = col.s[si], lwd = lwd, cex = cex)
              }
            }}

        } # end of exlusion in the series
    } else {
      if (nobs == 1) {
        if (plot.se) {
          segments(year, u-2*se, year, u+2*se, col = "grey")
        } else {
          points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                 bg = ifelse(included == 1, col.s[si], "white"),
                 col = col.s[si], lwd = lwd, cex = cex)
        }
      } else {
        if (plot.se){
          for (obs in 2:nobs){
            polygon(c(year[obs-1],year[obs-1],year[obs],year[obs],year[obs-1]),
                    c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                      u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]),
                    col = adjustcolor("grey", alpha.f = 0.2), border = NA)
          }
        } else {
          lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))       #### place to link all data points with dashed lines 05/14/2018
          if (plot.points)
            points(u ~ year, pch = ifelse(hasbias.si[si] == 1, 23, 21), # change JR, 20140429
                   bg = ifelse(included == 1, col.s[si], "white"),
                   col = col.s[si], lwd = lwd, cex = cex)
        }
      }
    }



  }
  ##value<< List containing:
  return(list(legendcol.s = col.s, ##<< Vector of colours used to plot series.
              legendbg.s = col.newentry.s, ##<< Vector of background colours used to plot series (not used).
              legendtext.s = legendtext.si, ##<< Vector of series names.
              legendpch.s = legendpch.si ##<< Vector of legend plotting characters.
  ))
}
#----------------------------------------------------------------------
AddVRData <- function(# Add VR data and/or sampling errors to the plot
  u.Ls.i,
  year.Ls.i,
  se.Ls.i = NULL,
  included.Ls.i = NULL, # change JR, 3 Jun
  hasbias.Ls.i = NULL, # change JR, 20140429
  isincomplete.Ls.i = NULL, # change JR, 20140429
  newentry.s = NULL,
  source.s = NULL,
  col.s = NULL,
  plot.se = FALSE,
  plot.data.excl = FALSE,
  plot.points = TRUE,
  plot.lines = TRUE,
  plot.newobsPIs = FALSE,
  lwd = 2,cex=2,   ###add cex option   kai added 05/14/2018
  seriesnames.in.full = TRUE # change JR, 3 Sep 2013
) {
  # use for plot se or obs!
  nseries <- length(year.Ls.i)
  if (nseries == 0) return()
  series <- seq(1, nseries)
  if (is.null(col.s)) {
    col.s <- rep(c("black", RColorBrewer::brewer.pal(11, "BrBG")[1:4]), ceiling(nseries/4))[1:nseries]
    #col.s <- rep(c("black", "black", "black", "black", "black"), ceiling(nseries/4))[1:nseries]
  }
  # color for new entry
  col.newentry.s <- NULL
  if(!is.null(newentry.s)){
    col.newentry.s <- rep("black", nseries)
    col.newentry.s[newentry.s==1] <- "#c7202e" # comment out this line if don't want to highlight any VR
  }
  legendtext.si <- paste0(ifelse(grepl("SVR", source.s[series]), "", "VR "),
                          ifelse(grepl("WHO", source.s[series]),
                                 ifelse(grepl("Recalculated", source.s[series]), "WHO (Recalculated) ", "WHO "),
                                 source.s[series]))
  # ad-hoc change JR, 3 Sep 2013
  legendtext.si <- gsub("Population Growth Estimation Experiment", "Pop Growth Est Expmt", legendtext.si)
  # note: s in data and legend/col now refer to different ordering
  si <- 0
  legendpch.si <- rep(NA, nseries)
  for (s in series) {
    si <- si+1
    u <- u.Ls.i[[s]]
    nobs <- length(u)
    year <- year.Ls.i[[s]]
    if (!is.null(se.Ls.i)) {
      se <- se.Ls.i[[s]]
    } else {
      se <- rep(NA, nobs)
    }

    if (!is.null(included.Ls.i)) {
      included <- included.Ls.i[[s]] # change JR, 3 Jun
    } else {
      included <- rep(1, nobs)
    }

    if (!is.null(hasbias.Ls.i)) { # change JR, 20140429
      hasbias <- hasbias.Ls.i[[s]]
    } else {
      hasbias <- rep(0, nobs)
    }

    if (!is.null(isincomplete.Ls.i)) { # change JR, 20140429
      isincomplete <- isincomplete.Ls.i[[s]]
    } else {
      isincomplete <- rep(0, nobs)
    }

    legendpch.si[si] <- ifelse(sum(included == 1) > 0, 15, 0)# 19, 1), # change JR, 20140423

    #start to revise


    if (!plot.data.excl) {
      # u <- u[included == 1]
      # year <- year[included == 1]
      # se <- se[included == 1]
      # included <- included[included == 1]
      if(prod(unlist(included))!=0){ # if there's no exclusion in the series
        if (nobs == 1) {
          if (plot.se) {
            # segments(year, u-2*se, year, u+2*se, col = col.s[si])
            segments(year, u-2*se, year, u+2*se, col = "grey")
          } else {
            points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                   bg = ifelse(included == 1, col.s[si], "white"),
                   col = col.s[si], lwd = lwd,
                   cex = ifelse(hasbias | isincomplete, 2.5, cex)) # change JR, 20140429
          }
        } else {
          if (plot.se) {
            for (obs in 2:nobs) {
              polygon(c(year[obs-1], year[obs-1], year[obs], year[obs], year[obs-1]),
                      c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                        u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]),
                      col = adjustcolor("grey", alpha.f = 0.2), border = NA)
            }
          } else {
            if (plot.lines & !(all(as.logical(included) )& any(hasbias | isincomplete))) # change JR, 20140429
              lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))
            if (plot.points)
              points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                     bg = ifelse(included == 1, col.s[si], "white"), # change JR, 11 Jul
                     col = col.s[si], lwd = lwd,
                     cex = ifelse(hasbias | isincomplete, 2.5, cex)) # change JR, 20140429
      }}
      }else{ # there is exclusion in the series
        sub=splitseries(u=u,inclusion = included,year=year)
        sub_u=sub$sub_u
        sub_year=sub$sub_year

        for (i in 1:length(sub_u)){
          u=sub_u[[i]]
          year=sub_year[[i]]

          if (nobs == 1) {
            if (plot.se) {
              segments(year, u-2*se, year, u+2*se, col = col.s[si])
            } else {
              points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                     #bg = ifelse(included == 1, col.s[si], "white"),
                     bg= col.s[si],
                     col = col.s[si], lwd = lwd,
                     cex = ifelse(hasbias | isincomplete, 2.5, cex)) # change JR, 20140429
            }
          } else {
            if (plot.se) {
              for (obs in 2:nobs) {
                polygon(c(year[obs-1], year[obs-1], year[obs], year[obs], year[obs-1]),
                        c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                          u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]),
                        col = adjustcolor("grey", alpha.f = 0.2), border = NA)
              }
            } else {
              if (plot.lines & !(all(as.logical(included) )& any(hasbias | isincomplete))) # change JR, 20140429
                lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))
              if (plot.points)
                points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                       #bg = ifelse(included == 1, col.s[si], "white"), # change JR, 11 Jul
                       bg = col.s[si],
                       col = col.s[si], lwd = lwd,
                       cex = ifelse(hasbias | isincomplete, 2.5, cex)) # change JR, 20140429
            }}

        }# end of subseries
    }} else { # end of point excl
      if (nobs == 1) {
        if (plot.se) {
          segments(year, u-2*se, year, u+2*se, col = col.s[si])
        } else {
          points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                 bg = ifelse(included == 1, col.s[si], "white"),
                 col = col.s[si], lwd = lwd,
                 cex = ifelse(hasbias | isincomplete, 2.5, cex)) # change JR, 20140429
        }
      } else {
        if (plot.se) {
          for (obs in 2:nobs) {
            polygon(c(year[obs-1], year[obs-1], year[obs], year[obs], year[obs-1]),
                    c(u[obs-1]-2*se[obs-1], u[obs-1]+2*se[obs-1],
                      u[obs]+2*se[obs], u[obs]-2*se[obs], u[obs-1]-2*se[obs-1]),
                    col = adjustcolor("grey", alpha.f = 0.2), border = NA)
          }
        } else {
          if (plot.lines & !(all(as.logical(included) )& any(hasbias | isincomplete))) # change JR, 20140429
            lines(u ~ year, col = col.s[si], lwd = lwd, lty = ifelse(!plot.data.excl, 1, 2))  #### place to link all data points with dashed lines 05/14/2018
          if (plot.points)
            points(u ~ year, pch = ifelse(hasbias, 23, ifelse(isincomplete, 24, 22)), # change JR, 20140429
                   bg = ifelse(included == 1, col.s[si], "white"), # change JR, 11 Jul
                   col = col.s[si], lwd = lwd,
                   cex = ifelse(hasbias | isincomplete, 2.5, cex)) # change JR, 20140429
        }}


    }
  }
  ##value<< List containing:
  return(list(legendcol.s = col.s, ##<< Vector of colours used to plot series.
              legendbg.s = col.newentry.s, ##<< Vector of background colours used to plot series.
              legendtext.s = legendtext.si, ##<< Vector of series names.
              legendpch.s = legendpch.si ##<< Vector of legend plotting characters.
  ))
}
#----------------------------------------------------------------------
PlotCIs <- function(# Add confidence intervals to plot
  c,
  CIs.cqt,
  year.t,
  col.est,
  col.CI,
  lwd = 5
) {
  for (t in 2:length(year.t)) {
    polygon(c(year.t[t-1], year.t[t-1], year.t[t], year.t[t], year.t[t-1]),
            c(CIs.cqt[c,1,t-1], CIs.cqt[c,3,t-1], CIs.cqt[c,3,t], CIs.cqt[c,1,t], CIs.cqt[c,1,t-1]),
            col = col.CI,  border = NA)
    # the transparency is controlled by the color picked. e.g. rgb(1, 0, 0, 0.5)
  }
  # lines(CIs.cqt[c, 2, ] ~ year.t, col = col.est, lwd = lwd)
  ##value<< \code{NULL}.
  return(invisible())
}
#----------------------------------------------------------------
PlotLegend <- function(# Plot legend.
  legendtext.s, ##<< Vector of legend text labels.
  legendcol.s, ##<< Vector of legend colours.
  # YL2020: now legendbg.s is used for text color
  legendbg.s = NULL, ## vector of legend background colours for plotting characters.
  legendpch.s, ##<< Vector of legend plotting characters.
  seriesnames.in.full = TRUE, ##<< Display series names in full? If \code{FALSE}, only display series names
  ## for series with year 1990 or later if there are more than 20 series. # change JR, 3 Sep 2013
  cex,   ## Magnification size of legend, overwritten, no need to have input here
  pt.cex ## expansion factor(s) for the points. kai added 05/14/2018
) {
  par(mar = c(1,0,1,1))
  if (!seriesnames.in.full & length(legendtext.s) > 20) { # change JR, 3 Sep 2013
    select <- !grepl("190|191|192|193|194|195|196|197|198", legendtext.s)
    legendtext.s <- legendtext.s[select]
    legendcol.s <- legendcol.s[select]
    legendpch.s <- legendpch.s[select]
    if (!is.null(legendbg.s))
      legendbg.s <- legendbg.s[select]
  }
  # legendbg.s <- rep("#FF000020", length(legendtext.s))
  # YL: overwrite cex in certain conditions:
  if(length(legendtext.s)>20) cex <- 1.6-length(legendtext.s)/100
  if(length(legendtext.s)>40) cex <- 1.5-length(legendtext.s)/100
  # add by YS, if >30: 1; if 20~30, 1.2;if <20,1.5

  # set text length YL2020
  # set_text_length = {1,8X}, set a limit of 86 nchar per line of legend text
  set_text_length = 85
  n_long <- sapply(legendtext.s, function(x)nchar(x)>=set_text_length)
  message("n_long TRUE: ", legendtext.s[n_long])
  message("length: ", length(legendtext.s))

  strBreakInLines <- function(s) {
  # break a line that is too long, the X in (.{1,X})
    s2 = gsub('(.{1,85})(\\s|$)', '\\1\n', s)
    substr(s2, 1, nchar(s2)-1) # remove the "\n" in the end
  }

  if(any(n_long) & length(legendtext.s)>25) {
    cex= 1.5 - length(legendtext.s)/100
    message("smaller cex is ", cex)
  } else {

    for (i in 1: length(legendtext.s)){
      legendtext.s[i] <- sub("\n", " ", legendtext.s[i])
      legendtext.s[i] <- strBreakInLines(legendtext.s[i])
      message("legendtext.s[i] is ", legendtext.s[i])
    }
  }



  # if(cex == 1.39){
  # cex = ifelse(length(legendtext.s)>36, 1.5-length(legendtext.s)/100, cex) # add by YS, if >30: 1; if 20~30, 1.2;if <20,1.5
  # } else {
  #   # add by Kai, decrease the font size for country with too many data series. 05/14/2018
  #   cex = ifelse(length(legendtext.s)>36,cex*0.85,cex)
  # }
  plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  legend("left", legend = c(rev(legendtext.s)) ,
           col = c(rev(legendcol.s)),
           text.col = c(rev(legendbg.s)), # YL 2020
           # pt.bg = c(rev(legendbg.s)),
           pch = c(rev(legendpch.s)),
           cex = cex, lwd = cex,
         # text.width = 3, y.intersp = 1, # not useful
         trace = FALSE)
  return(invisible())
}
#----------------------------------------------------------------
EmptyPlot <- function() {
  plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}
#----------------------------------------------------------------
splitseries=function(
  inclusion,
  u,
  year
){
  ind_pos=which(inclusion==0)
  sub_inclusion=splitAt(inclusion,ind_pos)
  sub_u=splitAt(u,ind_pos)
  sub_year=splitAt(year,ind_pos)

  for(i in 1:length(sub_u)){
    sub_u[[i]]=sub_u[[i]][sub_inclusion[[i]]==1]
    sub_year[[i]]=sub_year[[i]][sub_inclusion[[i]]==1]
  }

  return(list(
    sub_u=sub_u,
    sub_inclusion=sub_inclusion,
    sub_year=sub_year
  ))

}

indirect_data_visibility=function(data,
                                  indirect_series_visibility=T){
  data.all=data
  if(indirect_series_visibility==F){
    for(c in 1:data.all$C){
      deletelist=c()
      b=which(duplicated(data.all$seriesyear.Lc.s[[c]])| duplicated(data.all$seriesyear.Lc.s[[c]], fromLast=TRUE))     #####filter the data series with the same series year  Kai Zhong 05/23/2018
      d=which(grepl("indirect",data.all$sourceid.Lc.s[[c]][b],ignore.case = T)==T)     #####filter the data series with the string "indirect"  Kai Zhong 05/23/2018
      e=which(data.all$sourceid.Lc.s[[c]] %in% data.all$sourceid.Lc.s[[c]][b][d])      #####get the location of those data series with the string "indirect"        Kai Zhong 05/23/2018
      f=which(grepl("household",data.all$sourceid.Lc.s[[c]][b],ignore.case = T)==T)      #####filter the data series with the string "household"  Kai Zhong 05/23/2018
      g=which(data.all$sourceid.Lc.s[[c]] %in% data.all$sourceid.Lc.s[[c]][b][f])     #####get the location of those data series with the string "household"        Kai Zhong 05/23/2018
      h=c(e,g)
      left=setdiff(b,h)            #####get the rest data series that shares the same series year and name     Kai Zhong 05/23/2018
      for(i in e){
        if(any(1 %in% unlist(data.all$included.Lcs.j[[c]][i]))==F & any(stringsim(gsub( " *\\(.*?\\) *", "",data.all$source.Lc.s[[c]][i],ignore.case=T),gsub( " *\\(.*?\\) *", "",data.all$source.Lc.s[[c]][left],ignore.case=T),method="cosine")==1)){
          #####detect whether indirect series is included or not         ######detect whether there exists any data series that share the same name with indirect data series if eliminating all contents in the parentheses    Kai Zhong 05/23/2018
          deletelist=c(deletelist,i)
          data.all$nseriesnonvr.c[c]=data.all$nseriesnonvr.c[c]-1           ####if existing, the number of non-vr series will minus one
        }
      }
      if(!is.null(deletelist)){
        #######delete indirect data series from the data structure based on their location   Kai Zhong 05/23/2018
        data.all$u.Lcs.j[[c]]=data.all$u.Lcs.j[[c]][-deletelist]
        data.all$sourceid.Lcs.j[[c]]=data.all$sourceid.Lcs.j[[c]][-deletelist]
        data.all$sourceid.Lc.s[[c]]=data.all$sourceid.Lc.s[[c]][-deletelist]
        data.all$source.Lc.s[[c]]=data.all$source.Lc.s[[c]][-deletelist]
        data.all$method.Lcs.j[[c]]=data.all$method.Lcs.j[[c]][-deletelist]
        data.all$se.Lcs.j[[c]]=data.all$se.Lcs.j[[c]][-deletelist]
        data.all$hasbias.Lc.s[[c]]=data.all$hasbias.Lc.s[[c]][-deletelist]
        data.all$seriesyear.Lc.s[[c]]=data.all$seriesyear.Lc.s[[c]][-deletelist]
        data.all$sourcetype.Lc.s[[c]]=data.all$sourcetype.Lc.s[[c]][-deletelist]
        data.all$surveyyear.Lc.s[[c]]=data.all$surveyyear.Lc.s[[c]][-deletelist]
        data.all$method.Lc.s[[c]]=data.all$method.Lc.s[[c]][-deletelist]
        data.all$isDHSdirectany.Lc.s[[c]]=data.all$isDHSdirectany.Lc.s[[c]][-deletelist]
        data.all$senonNA.Lcs.j[[c]]=data.all$senonNA.Lcs.j[[c]][-deletelist]
        data.all$included.Lcs.j[[c]]=data.all$included.Lcs.j[[c]][-deletelist]
        data.all$sourcetype.Lcs.j[[c]]=data.all$sourcetype.Lcs.j[[c]][-deletelist]
        data.all$interval.Lcs.j[[c]]=data.all$interval.Lcs.j[[c]][-deletelist]
        data.all$year.Lcs.j[[c]]=data.all$year.Lcs.j[[c]][-deletelist]
      }
    }
  }
  return(data.all=data.all)
}

splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

