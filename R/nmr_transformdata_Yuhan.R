
# tansform NMR data from .csv format to mcmc.meta format (for IMR/U5MR)
# created by Yuhan Sun, 20170130
# updated 202001


#' Prepare data file and results for NMR plot
#'
#' @param iso the iso selected to be plotted, if NULL, produces all 195 from
#'   dimnames(results)[[3]]
#' @param file the directory to NMR file, e.g. data_NMR_20200207_5year.csv
#' @param new_entry_date if NULL will use the default: last Oct.
#' @param adj default to "crisisandhivfree" no need to revise
#' @param include_excluded default to TRUE: whether to include data that are
#'   excluded from model fitting
#' @param resultsfile the directory to results file --- cqt1
#' @param expectedresultsfile the directory to results file --- expected
#' @param resultsfile2 the directory to results file --- cqt2
#' @param resultsfile3 the directory to results file --- cqt3
#' @param scale default to "NMR", allow "NMR" or "ratio"
#' @param year_range the year range for the zoomed plot, must have 0.5
#' @param new.cname.df a df that contains both iso and country name
#' @param igmefile  NULL as default
#' @param crisisadjfile dir to "dataPostAdj_U5MR.csv"
#'
#' @return a list of data and res.cqt(s)
#' @export transformdataforNMR
#'
transformdataforNMR = function(
  iso = NULL, ##<< the iso selected to be plotted, if NULL, produces all 195 from dimnames(results)[[3]]
  file, ##<< the directory to NMR file, e.g. data_NMR_20200207_5year.csv
  new_entry_date = NULL, # if NULL will use the default: last Oct.
  adj = "crisisandhivfree", ##<< adjustment to U5 data. Model is run on crisis and HIV free U5, useless
  # Should be FALSE for model fitting, true for plotting
  include_excluded = TRUE,        ##<< .
  resultsfile = NULL,     ##<< the estimates resultfile
  expectedresultsfile = NULL, ## the expected results
  resultsfile2 = NULL,
  resultsfile3 = NULL,
  scale = "NMR", ##<< default 'NMR', can be change to 'ratio'
  year_range = NULL, ## the year range for the zoomed plot, must have 0.5
  new.cname.df = NULL,          ##<< a df that contains both iso and country name
  igmefile = NULL, ##<< if set to NULL, no igme line would be plotted
  crisisadjfile = NULL ##default NULL, exclude data in the crisis year
){
  if(!scale%in%c("NMR", "ratio")) stop("Choose scale between `NMR` and `ratio`.")

  if(is.null(new_entry_date)) {
    new_entry_date <- last.October()
    message("Note: `new_entry_date` default to last Oct.")
  }
  # notice that for year to be matched later, should have 0.5 attached (YL)
  # load data
  # d is like the processed CME.info based on `file`
  d <- GetDataGlobalNMR(adj = adj,
                        file = file,
                        include_excluded = include_excluded,
                        new_entry_date = new_entry_date,
                        crisisadjfile = crisisadjfile)
  # dt5 is just the U5 median estimates
  du5 <- GetDataU5MRNMR(adj = adj)
  iso.u.c=du5$iso.u.c

  df=d$df
  df$seriesname.i=as.character(df$seriesname.i)
  df$sourcetype.i=as.character(df$sourcetype.i)
  df['seriesnameandyear']=paste(df$seriesname.i,df$seriesyear.i,sep='')
  df$seriesname.i=ifelse(df$sourcetype.i=='SVR'& (!grepl('SVR', df$seriesname.i)),
                         paste('SVR', df$seriesname.i), df$seriesname.i)

  # For setting the right dimension of res.cqt
  year.u.t <- du5$year.u.t
  # if `year_range` is provided, set the uplimit of year.u.t
  if(!is.null(year_range)) {
    if(round(max(year_range)) == max(year_range)) year_range = year_range + 0.5
    year.u.t = c(min(year.u.t) : max(year_range))

  } else {
    year_range <- year.u.t
  }


  # DJS edit to use single file for official country new.cname.df
  # iso.i are those with data (from dataset)
  iso.i = d$iso.code # YL 12/26

  # iso are the results to make, if would come from `resultsfile` later if `resultsfile` provided

  cat("Data obtained by GetDataGlobalNMR and GetDataU5MRNMR Loaded \n")

  if(!is.null(resultsfile)){
      load(resultsfile)
      if (scale=='ratio'){
        results=results_ratio.jtc
      }else{
        results=results.jtc}
  } else {
      results=NULL
    }
  if(is.null(iso)) iso <- dimnames(results)[[3]]
  message("Length of all iso is ", length(iso), ".")
  # If still null
  if(is.null(iso)) iso <- iso.i

  if (!is.null(expectedresultsfile)){
      load(expectedresultsfile)
      if (scale=='ratio'){
        results_ex=results_expectedratio.jtc
      }else {
        results_ex=results_expected.jtc}
  } else{
       results_ex=NULL
    }

  if (!is.null(resultsfile2)){
   load(resultsfile2)
   if (scale=='ratio'){
    results2=results_ratio.jtc
   }
   else {
    results2=results.jtc}
     } else{
   results2=NULL
  }

  if (!is.null(resultsfile3)){
   load(resultsfile3)
   if (scale=='ratio'){
    results3=results_ratio.jtc
   }else {
    results3=results.jtc}
  } else{
   results3=NULL
  }

  if (!is.null(igmefile) ){
    igmedata=read.csv(igmefile)
    } else {
          print ('No igme data found')
    }



  # for adding survey data
  u.Lcs.j=year.Lcs.j=se.Lcs.j=included.Lcs.j=nseriesnonvr.c=sourcetype.Lc.s=seriesyear.Lc.s=
    source.Lc.s=hasbias.Lc.s=isincompletevr.Lcs.j= has.no.data = method.Lc.s = newentry.Lc.s= list()
  maxyear.c=rep(NA,length(iso))
  nseriesnonvr.c=rep(NA,length(iso))
  name.c=rep(NA,length(iso))
  year.t=list()

  # for add vr data
  uvr.Lcs.j=yearvr.Lcs.j=sevr.Lcs.j=includedvr.Lcs.j=sourcevr.Lc.s=list()
  yearvr.Lc.j=uvr.Lc.j= newentryvr.Lc.s = list()

  # for estimations and expected
  # res.cqt=list()
  # res_ex.cqt=list()
  res.cqt=array(dim=c(length(iso),3,length(year.u.t)),dimnames = list(iso,c(0.05,0.5,0.95),year.u.t))
  res_ex.cqt=array(dim=c(length(iso),3,length(year.u.t)),dimnames = list(iso,c(0.05,0.5,0.95),year.u.t))

  #Other estimates to show
  res2.cqt=array(dim=c(length(iso),3,length(year.u.t)),dimnames = list(iso,c(0.05,0.5,0.95),year.u.t))
  res3.cqt=array(dim=c(length(iso),3,length(year.u.t)),dimnames = list(iso,c(0.05,0.5,0.95),year.u.t))

  u.ct=matrix(NA,ncol=84,nrow=length(iso))
  t=matrix(NA,ncol=84,nrow=length(iso))


  if (sum(iso %in% iso.i) != length(iso)){
    cat(paste(iso[!iso %in% iso.i], 'is not in the database','\n'))
    iso_no_data=iso[!iso %in% iso.i]
    iso_with_data=iso[iso %in% iso.i]
    isonumber=match(iso_with_data,iso)
    isonodatanumber=match(iso_no_data,iso)
  } else {
    iso_no_data=NULL
    iso_with_data=iso[iso %in% iso.i]
    isonumber=match(iso_with_data,iso)
    isonodatanumber=match(iso_no_data,iso)}


  for (c in isonumber){ # the ith country in the selected iso list

    iso.selected=as.character(iso[c])
    message("iso.selected is ",iso.selected)

    if (iso.selected %in% iso.i){
    has.no.data[[c]] <- NULL
    df.selected=df[df$iso.i==iso.selected,]
    name.selected = new.cname.df$OfficialName[new.cname.df$ISO3Code %in% iso.selected]
    #name.c[c]=name.selected
    vr.ind = df.selected$series.i %in%c('VR','SVR')

    #cat (paste(name.selected,'is readed in','\n'))

    # initialize lists
    # for add survey data
    u.Lcs.j[[c]]=year.Lcs.j[[c]]=se.Lcs.j[[c]]=included.Lcs.j[[c]]=sourcetype.Lc.s[[c]]=seriesyear.Lc.s[[c]]=
      source.Lc.s[[c]]=hasbias.Lc.s[[c]]=method.Lc.s[[c]]=newentry.Lc.s[[c]] = list()
    # for add vr data
    uvr.Lcs.j[[c]]=yearvr.Lcs.j[[c]]=sevr.Lcs.j[[c]]=includedvr.Lcs.j[[c]]=sourcevr.Lc.s[[c]]=
      isincompletevr.Lcs.j[[c]]=newentryvr.Lc.s[[c]] = list()

    sourcetype.list=vector()
    seriesyear.list=vector()
    source.list=vector()
    sourcevr.list=vector()
    newentry.list = vector()
    newentryvr.list = vector()
    #
    # for non-VR --------------------------------------------------------------
    n.list = unique(df.selected$seriesnameandyear[!vr.ind])
    if (sum(!vr.ind)!=0){ # there are non-vr series
    for (n in 1:length(n.list)){
     series.selected=n.list[n]
     if(scale=='ratio'){
       u.Lcs.j[[c]][[n]]=df.selected$ratio.i[df.selected$seriesnameandyear==series.selected]
       se.Lcs.j[[c]][[n]]=df.selected$se.ratio.i[df.selected$seriesnameandyear==series.selected]
     }else {
       u.Lcs.j[[c]][[n]]=df.selected$neonatal.i[df.selected$seriesnameandyear==series.selected]
       se.Lcs.j[[c]][[n]]=df.selected$se.i[df.selected$seriesnameandyear==series.selected]
       }

     year.Lcs.j[[c]][[n]]=df.selected$year.i[df.selected$seriesnameandyear==series.selected]
     included.Lcs.j[[c]][[n]]=df.selected$inclusion.i[df.selected$seriesnameandyear==series.selected]
     newentry.list <- c(newentry.list, unique(df.selected$newentry.i[df.selected$seriesnameandyear==series.selected]))
     sourcetype.list=c(sourcetype.list,unique(as.character(df.selected$sourcetype.i[df.selected$seriesnameandyear==series.selected])))
     seriesyear.list=c(seriesyear.list,unique(as.character(df.selected$seriesyear.i[df.selected$seriesnameandyear==series.selected])))
     source.list=c(source.list,unique(as.character(df.selected$seriesname.i[df.selected$seriesnameandyear==series.selected])))

    }}
    hasbias.Lc.s[[c]]=rep(0,length(n.list))
    sourcetype.Lc.s[[c]]=sourcetype.list
    seriesyear.Lc.s[[c]]=seriesyear.list
    source.Lc.s[[c]]=source.list
    nseriesnonvr.c[c]=length(n.list)
    method.Lc.s[[c]] <-  NULL # YL 2020
    newentry.Lc.s[[c]] <- newentry.list

    #
    # for VR --------------------------------------------------------------
    n.list.vr=unique(df.selected$seriesnameandyear[vr.ind])

    if (sum(vr.ind)!=0){ # if there are vr series
    for (n in 1:length(n.list.vr)){
      series.selected=n.list.vr[n]
      if(scale=='ratio'){
        uvr.Lcs.j[[c]][[n]]=df.selected$ratio.i[df.selected$seriesnameandyear==series.selected]
        sevr.Lcs.j[[c]][[n]]=df.selected$se.ratio.i[df.selected$seriesnameandyear==series.selected]
      } else {
        uvr.Lcs.j[[c]][[n]]=df.selected$neonatal.i[df.selected$seriesnameandyear==series.selected]
        sevr.Lcs.j[[c]][[n]]=df.selected$se.i[df.selected$seriesnameandyear==series.selected]
      }
      # more:
        yearvr.Lcs.j[[c]][[n]]=df.selected$year.i[df.selected$seriesnameandyear==series.selected]
        includedvr.Lcs.j[[c]][[n]]=df.selected$inclusion.i[df.selected$seriesnameandyear==series.selected]
        isincompletevr.Lcs.j[[c]][[n]]=(rep(0,sum(df.selected$seriesnameandyear==series.selected)))
        sourcevr.list=c(sourcevr.list,unique(as.character(df.selected$seriesname.i[df.selected$seriesnameandyear==series.selected])))
        newentryvr.list <- c(newentryvr.list, unique(df.selected$newentry.i[df.selected$seriesnameandyear==series.selected]))
    }
      sourcevr.Lc.s[[c]]=sourcevr.list
      yearvr.Lc.j[[c]]=unique(unlist(yearvr.Lcs.j[[c]]))
      uvr.Lc.j[[c]]=unlist(uvr.Lcs.j[[c]])
      newentryvr.Lc.s[[c]] <- newentryvr.list # YL 2020
    }

   maxyear.c[c]=max(df.selected$year.i)
   name.c[c]=as.character(new.cname.df$OfficialName[new.cname.df$ISO3Code %in% iso.selected])
   #isincompletevr.Lcs.j[[c]]=rep(0,length(n.list.vr))

   }

   ###### result
   # year series for estimation and expectation
   cn <- which(iso.u.c==paste(iso.selected)) # position of the iso code in the iso code list
   # ???
   minyear <- min(1990.5, min(df$year.i[paste(df$iso.i)==paste(iso.selected)],na.rm = T),na.rm = T) #change 0706 YL:???
   minyear <- min(year_range)
   minyear <- round(minyear)+0.5
   seq.years <- minyear : (round(max(year_range))+0.5) #DJS edited to use max(year_range) instead of to 2030

   ####=====estimation

   if (!is.null(resultsfile)){
   res = results[,,cn] #
   res_med=apply(res, 2, function(x) median(x,na.rm=T))
   res_upper=apply(res,2,function(x) quantile(x,0.95,na.rm = T))
   res_lower=apply(res,2,function(x) quantile(x,0.05,na.rm = T))
   #names(res_upper)=year.u.t
   res.cqt[c,1,] <- res_lower[1:which(names(res_lower)==max(year_range))] # DJS edit to only plot to max of year, so not to 2030 unless specified
   res.cqt[c,2,] <- res_med[1:which(names(res_med)==max(year_range))]
   res.cqt[c,3,] <- res_upper[1:which(names(res_upper)==max(year_range))]
   }


   ####=====estimation 2

   if (!is.null(resultsfile2)){
    res2 = results2[,,cn] #
    res2_med=apply(res2, 2, function(x) median(x,na.rm=T))
    res2_upper=apply(res2,2,function(x) quantile(x,0.95,na.rm = T))
    res2_lower=apply(res2,2,function(x) quantile(x,0.05,na.rm = T))
    names(res2_upper)=year.u.t

    res2.cqt[c,1,]=res2_lower[1:which(names(res2_lower)==max(year_range))]
    res2.cqt[c,2,]=res2_med[1:which(names(res2_med)==max(year_range))]
    res2.cqt[c,3,]=res2_upper[1:which(names(res2_upper)==max(year_range))]
   }

   ####=====estimation 3

   if (!is.null(resultsfile3)){
    res3 = results3[,,cn] #
    res3_med=apply(res3, 2, function(x) median(x,na.rm=T))
    res3_upper=apply(res3,2,function(x) quantile(x,0.95,na.rm = T))
    res3_lower=apply(res3,2,function(x) quantile(x,0.05,na.rm = T))
    names(res3_upper)=year.u.t

    res3.cqt[c,1,]=res3_lower[1:which(names(res3_lower)==max(year_range))]
    res3.cqt[c,2,]=res3_med[1:which(names(res3_med)==max(year_range))]
    res3.cqt[c,3,]=res3_upper[1:which(names(res3_upper)==max(year_range))]
   }


   ####=====expectation



   if(!is.null(expectedresultsfile)){
   res_ex=results_ex[,,cn]
   res_ex_med=apply(res_ex, 2, function(x) median(x,na.rm=T))
   res_ex_upper=apply(res_ex,2,function(x) quantile(x,0.95,na.rm = T))
   res_ex_lower=apply(res_ex,2,function(x) quantile(x,0.05,na.rm = T))
   names(res_ex_upper)=year.u.t

   res_ex.cqt[c,1,]=res_ex_lower[1:which(names(res_ex_lower)==max(year_range))]
   res_ex.cqt[c,2,]=res_ex_med[1:which(names(res_ex_med)==max(year_range))]
   res_ex.cqt[c,3,]=res_ex_upper[1:which(names(res_ex_upper)==max(year_range))]

   year.t[[c]]=seq.years
   }

   # for igme
   if (!is.null(igmefile) ){
     if (scale=='NMR'){
       igme.selected=igmedata[igmedata$ISO3Code==iso.selected,paste0("NMR.",1932:2015)]
       #colnames(igme.selected)=1932.5:2015.5
       u.ct[c,]=as.numeric(igme.selected)
       t[c,]=1932.5:2015.5
     } else{
       ratio.selected=igmedata[igmedata$ISO3Code==iso.selected,paste0("ratio.",1932.5:2015.5)]
       u.ct[c,]=as.numeric(ratio.selected)
       t[c,]=1932.5:2015.5
     }}

   #print(c)
  }# end the c loop

  message("End isonumber loop. ")


  # for no-data case --------------------------------------------------------
  if (!is.null(iso_no_data)){
      for (c in isonodatanumber){
      print(c)
      iso.selected=as.character(iso[c])
      cn<- which(iso.u.c==paste(iso.selected)) # position of the iso code in the iso code list
      cat(paste(iso.selected,'has no data in the database so only plot the results','\n'))
      name.c[c]=as.character(new.cname.df$OfficialName[new.cname.df$ISO3Code %in% iso.selected])

      ### construct
      has.no.data[[c]] <- TRUE
      u.Lcs.j[[c]]=year.Lcs.j[[c]]=se.Lcs.j[[c]]=included.Lcs.j[[c]]=sourcetype.Lc.s[[c]]=seriesyear.Lc.s[[c]]=
        source.Lc.s[[c]]=list()
      hasbias.Lc.s[[c]]=method.Lc.s[[c]] = newentry.Lc.s[[c]] = newentryvr.Lc.s[[c]] =vector()
      uvr.Lcs.j[[c]]=yearvr.Lcs.j[[c]]=sevr.Lcs.j[[c]]=includedvr.Lcs.j[[c]]=sourcevr.Lc.s[[c]]=
        isincompletevr.Lcs.j[[c]]=list()
      u.Lcs.j[[c]][[1]]=NA
      se.Lcs.j[[c]][[1]]=NA
      year.Lcs.j[[c]][[1]]=NA
      included.Lcs.j[[c]][[1]]=NA
      sourcetype.Lc.s[[c]]=NA
      seriesyear.Lc.s[[c]]=NA
      source.Lc.s[[c]]=NA
      uvr.Lcs.j[[c]][[1]]=NA
      sevr.Lcs.j[[c]][[1]]=NA
      yearvr.Lcs.j[[c]][[1]]=NA
      includedvr.Lcs.j[[c]][[1]]=NA
      isincompletevr.Lcs.j[[c]][[1]]=NA
      sourcevr.Lc.s[[c]]=NA
      yearvr.Lc.j[[c]]=NA
      uvr.Lc.j[[c]]=NA
      #### end of construct mcmc.meta

      ### construct result
      ###### result
      ####=====estimation

      if (!is.null(resultsfile)){
        res = results[,,iso.selected] #
        res_med=apply(res, 2, function(x) median(x,na.rm=T))
        res_upper=apply(res,2,function(x) quantile(x,0.95,na.rm = T))
        res_lower=apply(res,2,function(x) quantile(x,0.05,na.rm = T))
        names(res_upper)=year.u.t

        # res.cqt[c,1,]=res_lower
        # res.cqt[c,2,]=res_med
        # res.cqt[c,3,]=res_upper
        res.cqt[c,1,]=res_lower[1:which(names(res_lower)==max(year_range))] # DJS edit to only plot to max of year, so not to 2030 unless specified
        res.cqt[c,2,]=res_med[1:which(names(res_med)==max(year_range))]
        res.cqt[c,3,]=res_upper[1:which(names(res_upper)==max(year_range))]
      }

      # if(!is.null(expectedresultsfile)){
      #   res_ex=results_ex[,,iso_selected]
      #   res_ex_med=apply(res_ex, 2, function(x) median(x,na.rm=T))
      #   res_ex_upper=apply(res_ex,2,function(x) quantile(x,0.95,na.rm = T))
      #   res_ex_lower=apply(res_ex,2,function(x) quantile(x,0.05,na.rm = T))
      #   names(res_ex_upper)=year.u.t
      #
      #   res_ex.cqt[c,1,]=res_ex_lower
      #   res_ex.cqt[c,2,]=res_ex_med
      #   res_ex.cqt[c,3,]=res_ex_upper
      #
      #   year.t[[c]]=seq.years
      # }

      ############### Kai's change
      if (!is.null(resultsfile2)){
        res2 = results2[,,iso.selected] #
        res2_med=apply(res2, 2, function(x) median(x,na.rm=T))
        res2_upper=apply(res2,2,function(x) quantile(x,0.95,na.rm = T))
        res2_lower=apply(res2,2,function(x) quantile(x,0.05,na.rm = T))
        names(res2_upper)=year.u.t

        # res2.cqt[c,1,]=res2_lower
        # res2.cqt[c,2,]=res2_med
        # res2.cqt[c,3,]=res2_upper
        res2.cqt[c,1,]=res2_lower[1:which(names(res2_lower)==max(year_range))]
        res2.cqt[c,2,]=res2_med[1:which(names(res2_med)==max(year_range))]
        res2.cqt[c,3,]=res2_upper[1:which(names(res2_upper)==max(year_range))]
      }


      # for IGME --------------------------------------------------------------
      if (!is.null(igmefile) ){
        if (scale=='NMR'){
          igme.selected=igmedata[igmedata$ISO3Code==iso.selected,paste0("NMR.",1932:2015)]
          #colnames(igme.selected)=1932.5:2015.5
          u.ct[c,]=as.numeric(igme.selected)
          t[c,]=1932.5:2015.5
        } else{
          ratio.selected=igmedata[igmedata$ISO3Code==iso.selected,paste0("ratio.",1932.5:2015.5)]
          u.ct[c,]=as.numeric(ratio.selected)
          t[c,]=1932.5:2015.5
        }}
    }

  } #there is country with no data

  c = 195
  #add an extra c+1 to avoid error
    u.Lcs.j[[c+1]]=year.Lcs.j[[c+1]]=se.Lcs.j[[c+1]]=included.Lcs.j[[c+1]]=sourcetype.Lc.s[[c+1]]=seriesyear.Lc.s[[c+1]]=
      source.Lc.s[[c+1]]=hasbias.Lc.s[[c+1]]=method.Lc.s[[c+1]]=list()
    uvr.Lcs.j[[c+1]]=yearvr.Lcs.j[[c+1]]=sevr.Lcs.j[[c+1]]=includedvr.Lcs.j[[c+1]]=sourcevr.Lc.s[[c+1]]=
      isincompletevr.Lcs.j[[c+1]]=list()
    newentry.Lc.s[[c+1]] = newentryvr.Lc.s[[c+1]] = list() # YL
    u.Lcs.j[[c+1]][[1]]=NA
    se.Lcs.j[[c+1]][[1]]=NA
    year.Lcs.j[[c+1]][[1]]=NA
    included.Lcs.j[[c+1]][[1]]=NA
    # sourcetype.list=NA
    # seriesyear.list=NA
    # source.list=NA
    sourcetype.Lc.s[[c+1]]=NA
    seriesyear.Lc.s[[c+1]]=NA
    source.Lc.s[[c+1]]=NA
    uvr.Lcs.j[[c+1]][[1]]=NA
    sevr.Lcs.j[[c+1]][[1]]=NA
    yearvr.Lcs.j[[c+1]][[1]]=NA
    includedvr.Lcs.j[[c+1]][[1]]=NA
    isincompletevr.Lcs.j[[c+1]][[1]]=NA
    sourcevr.Lc.s[[c+1]]=NA
    yearvr.Lc.j[[c+1]]=NA
    uvr.Lc.j[[c+1]]=NA
    sourcevr.Lc.s[[c+1]]=NA
    yearvr.Lc.j[[c+1]]=NA
    uvr.Lc.j[[c+1]]=NA



    if (!is.null(igmefile)){
      ##igme=list()
      igme=list(u.ct=u.ct, t=t)
    } else {
      igme = NULL}

    if (is.null(resultsfile))
      res.cqt=NULL

    if (is.null(expectedresultsfile))
      res_ex.cqt=NULL



    data = list(
      name.c=name.c,
      c=c,
      u.Lcs.j=u.Lcs.j,
      year.Lcs.j=year.Lcs.j,
      se.Lcs.j=se.Lcs.j,
      included.Lcs.j=included.Lcs.j,
      nseriesnonvr.c=nseriesnonvr.c,
      sourcetype.Lc.s=sourcetype.Lc.s,
      method.Lc.s=method.Lc.s, # YL
      newentry.Lc.s=newentry.Lc.s, # YL
      newentryvr.Lc.s= newentryvr.Lc.s, # YL 2020
      seriesyear.Lc.s=seriesyear.Lc.s,
      source.Lc.s=source.Lc.s,
      hasbias.Lc.s=hasbias.Lc.s,
      maxyear.c=maxyear.c,
      uvr.Lcs.j=uvr.Lcs.j,
      yearvr.Lcs.j=yearvr.Lcs.j,
      sevr.Lcs.j=sevr.Lcs.j,
      includedvr.Lcs.j=includedvr.Lcs.j,
      sourcevr.Lc.s=sourcevr.Lc.s,
      isincompletevr.Lcs.j=isincompletevr.Lcs.j,
      yearvr.Lc.j=yearvr.Lc.j,
      uvr.Lc.j=uvr.Lc.j,
      has.no.data = has.no.data
  )

    df$sourcename.i <- paste(df$iso.i, df$seriesname.i, df$seriesyear.i, sep = "_")
    new_series = unique(df$sourcename.i[df$newentry.i==1])
  return(
    list(data=data,
         res.cqt=res.cqt,
         res2.cqt=res2.cqt,
         res3.cqt=res3.cqt,
         res_ex.cqt=res_ex.cqt,
         year.t=year.u.t,
         igme=igme,
         iso=iso,
         iso_no_data=iso_no_data,
         new_series = new_series,
         new_entry_date = new_entry_date,
         NMR_scale = scale)
  )
  # version 19/12/26
}



# import functions from Monica, added by YS in 201703
# Get NMR data for MCMC model ---------------------------------------------
GetDataGlobalNMR<- function(file, #data file, must be in form of log ratios
                            new_entry_date = NULL,
                            adj="crisisandhivfree", # adjustment to U5 data. Model is run on crisis and HIV free U5
                            include_excluded = F, #whether to include data that are excluded from model fitting. Should be FALSE for model fitting, true for plotting
                            remove_nodata = F, # remove countries with no data,
                            crisisadjfile=NULL
){

  # U5MR
  du5 <- GetDataU5MRNMR(adj=adj)
  print ('Get Data Global NMR')
  year.u.t <- du5$year.u.t
  iso.u.c <- du5$iso.u.c
  u.ct <- du5$u.ct

  # Ratio estimates
  # d<-read.csv(filerate)
  d <- check_ratio_and_logit(file)
  # YL2020: to mark new entries
  d <- get.new.series.mark.entry(d, new_entry_date)
  vars_wanted <- c("Country.Code", "Series.Name","Reference.Date", "Estimates",
                   "SE_Neonatal", "Series.Category", "Series.Year", "Start.date.of.Survey", #"HIV.Series.To.Exclude"
                   "Inclusion", "Exclusion.External.Info", "Visible", "VR.recalc", "Size",
                   'Neonatal','Estimates','Standard.Error.of.Estimates', "new_entry")
  d <- d[,..vars_wanted]
  d <- d[with(d, order(d$Country.Code, d$Series.Category, d$Reference.Date)), ]
  # if need to subset Inclusion == 1:
  ifelse(include_excluded, d<- d, d <- d[d$Inclusion==1|is.na(d$Inclusion),] )
  # only pick Visible ones
  d <- d[is.na(d$Visible)|d$Visible!=0,]

  ####
  r.i<-d$Estimates
  rold.i <- as.numeric(as.character(r.i))
  iso.i <- paste(d$Country.Code) # turn factor into character, same as as.character

  # add extra uncertainty to adjusted VR countries
  #grp1 <-  c("BGR", "CZE", "ESP", "GRC", "LVA", "ROU", "RUS", "SVK")
  #grp2 <-  c("BLR", "HUN", "LTU")

  #d$SE_Neonatal[d$Country.Code %in% grp1] <- d$SE_Neonatal[d$Country.Code %in% grp1] + 0.1
  #d$SE_Neonatal[d$Country.Code %in% grp2] <- d$SE_Neonatal[d$Country.Code %in% grp2] + 0.05
  #d$SE_Neonatal[d$Country.Code=="EST"&d$Reference.Date<1992.5] <-  d$SE_Neonatal[d$Country.Code=="EST"&d$Reference.Date<1992.5]+ 0.1

  # source type estimating missing SEs
  d$sourcetype.i <- ifelse(d$Series.Category == "VR", "VR",
                           ifelse(d$Series.Category=="SVR", "SVR", #change by Yuhan 0331
                                  ifelse(is.element(d$Series.Category, c("DHS")), "DHS",
                                         ifelse(is.element(d$Series.Category, c("Interim DHS", "Special DHS", "AIS", "MIS", "NDHS", "WFS")), "Other DHS",
                                                ifelse(d$Series.Category == "MICS", "MICS",
                                                       ifelse(d$Series.Category == "Census", "Census",
                                                              "Others")))))) # RHS, PAP, Others,in Others!
  cat('
  #Notes from GetDataGlobalNMR: Attention !:
  #reconstruct Ratio and SE.ratio from logit estimates
  #The formulas are:
      Ratio=invlogit(Estimates)
      SE.Ratio=0.5*(invlogit(Estimates+2*Standard.Error.Of.Estimates)-Ratio)
  ')
  # same as arm::invlogit:
  invlogit <- function(x) 1/(1+exp(-x))
  ratio.i <- invlogit(d$Estimates)

  se.i<-d$SE_Neonatal
  se.i <- as.numeric(as.character(se.i))
  se.ratio.i <- with(d,0.5*(invlogit(Estimates+2*Standard.Error.of.Estimates)-ratio.i))
  se.ratio.i <- as.numeric(as.character(se.ratio.i))

  series.i<-d$Series.Category
  seriesyear.i<-d$Series.Year
  seriesstart.i <- d$Start.date.of.Survey
  seriesandyear.i<-paste0(series.i, seriesyear.i)
  inclusion.i <- d$Inclusion
  recalc.i <- d$VR.recalc
  sourcetype.i <- d$sourcetype.i
  imputedse.i <- d$Imputed.SE
  seriesname.i <- d$Series.Name
  neonatal.i<- d$Neonatal
  newentry.i <- d$new_entry

  ## GETTING DATA IN FORM FOR MODEL INPUT

  # want to match the u.ct estimates with the rold.i estimates
  n <- nrow(d)
  u.i<-rep(NA, length = nrow(d))
  for (i in 1:n){
    iso<- as.factor(d$Country.Code[i])
    yr<- d$Reference.Date[i]
    u.i[i]<-u.ct[match(iso, iso.u.c), match(floor(yr), floor(year.u.t))]
  }

  # need to remove the NAs
  # Notes:
  # ratio=invlogit(rold.i)
  # NMR=invlogit(rold.i)*u.i
  # u.i: calculated NMR
  df<-data.frame(rold.i, u.i, se.i, iso.i, d$Reference.Date, series.i, seriesname.i,sourcetype.i,
                 seriesyear.i, seriesstart.i, seriesandyear.i, inclusion.i, recalc.i, neonatal.i,
                 ratio.i, se.ratio.i, newentry.i)
  nodata <- names(which(table(df$iso.i, df$inclusion.i)[,colnames(table(df$iso.i, df$inclusion.i))=="1"]==0))
  if(remove_nodata) df <- df[!(df$iso.i %in% nodata),]
  #df<-df[!is.na(df$neonatal.i),]
  df<-df[order(df$iso.i),]
  colnames(df)[which(colnames(df)=="d.Reference.Date")]<- "year.i"
  u.i<-df$u.i
  rold.i<-df$rold.i
  iso.i<-df$iso.i
  se.i<-df$se.i
  # year.i<-df$d.Reference.Date
  year.i <- df$year.i # DJS edit 2019-05-08
  series.i<-df$series.i
  seriesyear.i<-df$seriesyear.i
  seriesandyear.i<-df$seriesandyear.i
  sourcetype.i <- df$sourcetype.i
  vr.i<-ifelse(df$series.i=="VR", 1, 0)

  ## deal with crisis free data
  # " observations in crisis country-years will have Inclusion changed to 0: "
  ## deal with crisis free data
  # " observations in crisis country-years will have Inclusion changed to 0: "
  select.crisis=rep(FALSE,nrow(df))

  if (!is.null(crisisadjfile)){
    crisisfree=read.csv(crisisadjfile) #"C:/Users/dsharrow/Dropbox/UN IGME data/2019 Round Estimation/Code/input/dataPostAdj_U5MR.csv"
    iso.crisis.to.check=unique(crisisfree$countrycode.adj)
  } else {iso.crisis.to.check=NULL}

  if (length(iso.crisis.to.check) > 0) {
    for (iso in iso.crisis.to.check) {
      crisis.years.range <- range(crisisfree$year.adj[crisisfree$countrycode.adj == iso &
                                                        crisisfree$add.adj != 0])

      years.crisis.i <- crisisfree$year.adj[crisisfree$countrycode.adj==iso]
      for(i in 1:length(years.crisis.i)){
        select.crisis[df$iso.i == iso & floor(df$year.i) == floor(years.crisis.i[i])] <- TRUE
      } # edit DJS 2019-05-06 to handle multiple non-seqeuntial crisis, i.e. Japan

      #cat(paste0(iso,crisis.years.range,'\n'))
      # if any observation year is within range of crisis years, select.crisis is TRUE
      #if(as.character(iso)=="JPN"){
      #select.crisis <- select.crisis |
      # (df$iso.i == iso &
      #   df$d.Reference.Date == 2011.5) |
      # (df$iso.i == iso &
      #  df$d.Reference.Date == 2015.5)
      #  } else {
      # if(as.character(iso)=="JPN"){
      #  select.crisis <- select.crisis |
      #   (df$iso.i == iso &
      #     df$year.i == 2011.5) |
      #    (df$iso.i == iso &
      #     df$year.i == 2015.5)
      # } else {
      # select.crisis <- select.crisis |
      #   (df$iso.i == iso &
      #      df$year.i >= crisis.years.range[1] &
      #      df$year.i <= crisis.years.range[2])
      #
      # } # if/else JPN

    } # for loop, iso
  } # if
  ##

  if (any(select.crisis)) {
    crisis.countries.years <- unique(paste(df$iso.i,
                                           df$year.i)[select.crisis])
    cat(paste0(length(crisis.countries.years),
               " observations in crisis country-years will have Inclusion changed to 0: ",
               paste(crisis.countries.years, collapse = ", "), "\n"))
    #data.cmeinfo$Inclusion[select.crisis] <- 0 # change JR, 20150515
    df$inclusion.i[select.crisis] <- 0
  }



  # want to make a matrix that is c x i (c countries x i observations )

  n.c<-as.numeric(table(iso.i)) #number of observations for country c
  iso.code<- names(table(iso.i))
  iso.code<- iso.code[which(n.c>0)]
  n.c<-n.c[which(n.c>0)] # no observations for STP
  C<-length(iso.code)
  nmax<-max(table(iso.i))

  return(list(C=C, n.c=n.c, iso.code=iso.code, df=df))
}

#' Get U5MR data (medians) for MCMC model -------------------------------------------
#' Load isos, years, and estimates by year for each country
#'
#' @param d median data file
#' @param adj default to "crisisandhivfree"
GetDataU5MRNMR<- function(d=NULL, # median data file
                          adj="crisisandhivfree"){
  if(is.null(d)){
    load(u5median.crisisandhivfree_file)
    # d <- eval(parse(text=load(paste0("data/u5median.", adj, ".rda"))))
    d <- u5median.crisisandhivfree
  }
  year.u.t <- as.integer(colnames(d)) + 0.5
  iso.u.c <- rownames(d)
  colnames(d) <- NULL
  u.ct <- d
  u.ct <- data.matrix(u.ct)
  return (list(year.u.t=year.u.t, iso.u.c=iso.u.c, u.ct=u.ct))
}


#' Clean dataset for NMR
#'
#' @param file_dir dir to nmr dataset
#'
#' @return cleaned dt
#' @export
#'
check_ratio_and_logit <- function(file_dir){
  nmr <- fread(file_dir)
  nmr$Estimates.check.rates<-with(nmr,log(Neonatal/(U5MR-Neonatal)))
  nmr$Estimates.check.ratio<-with(nmr,log(Ratio/(1-Ratio)))
  nmr$Estimates<-with(nmr,ifelse(is.na(Estimates) & !is.na(Estimates.check.rates),Estimates.check.rates,Estimates))
  nmr$Estimates<-with(nmr,ifelse(is.na(Estimates) & !is.na(Estimates.check.ratio),Estimates.check.ratio,Estimates))

  nmr$ratio.c<-with(nmr,ifelse(is.na(Ratio) & !is.na(U5MR) & is.na(Neonatal),Neontal/U5MR,Ratio))
  nmr$Ratio<-with(nmr,ifelse(is.na(Ratio) & !is.na(ratio.c), ratio.c,Ratio))

  nmr$diffest1<-nmr$Estimates.check.rates-nmr$Estimates
  nmr$diffest2<-nmr$Estimates.check.ratio-nmr$Estimates
  t<-nmr$diffest1[!is.na(nmr$diffest1)]
  nmr$Estimates.check.rates<-NULL
  nmr$Estimates.check.ratio<-NULL
  nmr$diffest1<-NULL
  nmr$diffest2<-NULL
  nmr$ratio.c<-NULL
  nmr$Estimates<-with(nmr,ifelse(Estimates==-Inf,NA,Estimates))
  #order the database
  nmr <- nmr[with(nmr, order(Country.Code, -as.numeric(Average.date.of.Survey), Series.Name, Series.Type, -Reference.Date)),]
  # fwrite(nmr, file_dir)
  return(nmr)
}
