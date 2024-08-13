# updated 202001.
# For gender-specific

#' Prepare data file and results for sex-specific plot
#' @param dt_gender the datast for plotting
#' @param new_entry_date to mark new entries, supply a date like "2020-06"
#' @param indicator full indicator, use `get.match` to get it
#' @param indicator_label Short indicator, use `get.match` to get it
#' @param sex Sex (Female, Male, Sex Ratio)
#' @param iso country iso in desired order
#' @param resultsfile_cqt results file for cqt1
#' @param expectedresultsfile_cqt results file for cqt expected
#' @param resultsfile_cqt2 results file for cqt2
#' @param resultsfile_cqt3 results file for cqt3
#' @param new.cname.df dataset contains both iso and official country name
#' @param resultsiso the iso order for the results
#' @param year_range year range, default to `1950.5:2030.5` if not specified
#' @param crisisadjfile dir to "dataPostAdj_U5MR.csv"
#' @param mlinfo dir to "MLinfo.csv", used to mark `includedvr.Lcs.j` for IMR
#'
#' @return a list of data and res.cqt(s)
#' @importFrom dplyr left_join
#' @importFrom dplyr recode
#' @export transformdataSexSpecific
#'
transformdataSexSpecific <- function(
  dt_gender, ##<< the datast for plotting
  new_entry_date = NULL, # YL2020
  indicator,
  indicator_label,
  sex,
  iso,
  new.cname.df,
  resultsfile_cqt = NULL, ##<< the estimates from resultfile
  resultsiso, # the ordered iso from resultsfile
  expectedresultsfile_cqt = NULL, ## the expected results from resultfile
  resultsfile_cqt2=NULL,
  resultsiso2 = NULL,  # since ISO order could differ YL 2022.07
  resultsfile_cqt3=NULL,
  resultsiso3 = NULL,

  year_range = NULL,
  crisisadjfile = NULL, ##default NULL, exclude data in the crisis year
  mlinfo = NULL
){
  # ddata=read.csv(dir_dataset, stringsAsFactors = F)
  d = subset(dt_gender, Indicator==indicator & Visible==1)
  # filerate <- file.path("C:/Users",username,"Dropbox/CMEgender2015/Database/dataset_forplotting_2019-08-04_new_ID.csv")
  # dt_gender <- fread(filerate)
  # dd = subset(dt_gender, Indicator == "Under-five Mortality Rate" & Visible == 1)

  if(!is.null(crisisadjfile)){
    crisisadjfile1=read.csv(crisisadjfile,stringsAsFactors = F,header=T)[,2:3]
    crisisadjfile1$crisisornot=1
    colnames(crisisadjfile1)[1:2]=c("Country.Code","Reference.Date")
    # note that merge re-orders the db, left_join doesn't. We should use left_join here
    # d = merge(d, crisisadjfile1, by = c("Country.Code", "Reference.Date"), all.x = TRUE)
    d = dplyr::left_join(d, crisisadjfile1, by = c("Country.Code", "Reference.Date"))
    d$crisisornot=ifelse(is.na(d$crisisornot),0,1)
    d$Inclusion.U5MR=ifelse(d$crisisornot==1,0,d$Inclusion.U5MR)
    d$Inclusion.Gender=ifelse(d$crisisornot==1,0,d$Inclusion.Gender)
  }

  # Do we only use Inclusion.U5MR for U5MR or should we use for all indicators?
  if(indicator=="Under-five Mortality Rate"){
    #d$Inclusion.Gender=ifelse(d$Inclusion.Gender==1|d$Inclusion.U5MR==1|is.na(d$Inclusion.Gender),1,0)
    d$Inclusion.Gender=ifelse(d$Inclusion.U5MR==1,1,0)
    d$Inclusion.Gender=ifelse(d$Inclusion.U5MR==0,0,1)
  }
  # YL:
  d$Inclusion.Gender <- d$Inclusion.U5MR

  d$Sex.Ratio=suppressWarnings(as.numeric(as.character(d$Sex.Ratio)))
  d$Sex.Ratio=ifelse(is.nan(d$Sex.Ratio)|!is.finite(d$Sex.Ratio)|d$Sex.Ratio<0|d$Sex.Ratio>100,NA,d$Sex.Ratio)
  d$Sex.Ratio.SE=suppressWarnings(as.numeric(as.character(d$Sex.Ratio.SE)))
  d$Male=ifelse(is.na(d$Male) |!is.finite(d$Male) |d$Male<0,NA,d$Male)
  d$Female=ifelse(is.na(d$Female) |!is.finite(d$Female) |d$Female<0,NA,d$Female)
  df = d
  # df[Country.Code == "ARE", unique(Series.Name)]

  # This is the series type, not the seriesname
  # e.g. "Others Indirect", "Census Direct"
  df$Series.Name <- as.character(df$Series.Name)
  # If want to change legend name, could modify the Series.Name directly
  df$Series.Name <- ifelse(df$Series.Category=="SVR", gsub('[0-9]+',"", df$Series.Name), df$Series.Name)
  # df$Series.Name <- ifelse(grepl("WHO",df$Series.Name), "WHO", df$Series.Name) # YL 2021.08 comment out as this is taken care of in `AddVRData` in a better way

  df$sourcetype.i = ifelse(df$Series.Type!="VR",
                           ifelse(grepl("Household Deaths", df$Data.Collection.Method, ignore.case = TRUE) ,
                                  paste(df$sourcetype.i, df$Data.Collection.Method, sep=" "),
                                  paste(df$sourcetype.i, df$Series.Type, sep=" ")),
                           "VR")

  df$seriesnameandyear = ifelse(df$Series.Type!="VR",
                                paste0(df$Series.Name, " ", df$Series.Year, " (", df$sourcetype.i,")"),
                                df$Series.Name)
  iso.i <- unique(df$Country.Code)
  cat(paste0('Data Loaded','\n'))

  # new entry YL 2020
  if(is.null(new_entry_date)) {
    new_entry_date <- last.October()
    message("Note: `new_entry_date` default to last Oct.")
  }
  df <- get.new.series.mark.entry(df, new_entry_date)

  setorder(df, Country.Name, -End.date.of.Survey, Series.Name, Series.Type, -Reference.Date,  Inclusion.Gender)
  # df[Country.Code=="ARE", unique(Series.Name)]
  # For setting the right dimension of res.cqt
  year.u.t <- as.numeric(dimnames(resultsfile_cqt)[[3]]) # which is 1950: 2030
  if(length(year.u.t)==0) year.u.t <- 1950:2030
  year.u.t <- floor(year.u.t) + 0.5

  # if `year_range` is provided, set the uplimit of year.u.t
  if(!is.null(year_range)) {
    if(round(max(year_range)) == max(year_range)) year_range = year_range + 0.5
    year.u.t = c(min(year.u.t) : max(year_range))

  } else {
    year_range <- year.u.t
  }

  # for adding survey data
  u.Lcs.j=year.Lcs.j=se.Lcs.j=included.Lcs.j=nseriesnonvr.c=sourcetype.Lc.s= seriesyear.Lc.s=
    source.Lc.s=hasbias.Lc.s=isincompletevr.Lcs.j=method.Lc.s=newentry.Lc.s=list() #YL2020
  maxyear.c=rep(NA,length(iso))
  nseriesnonvr.c=rep(NA,length(iso))
  name.c=rep(NA,length(iso))
  year.t=list()

  # for add vr data
  uvr.Lcs.j=yearvr.Lcs.j=yearvr.Lc.j=sevr.Lcs.j=includedvr.Lcs.j=sourcevr.Lc.s=newentryvr.Lc.s=list()
  uvr.Lc.j=list()

  # for estimations and expected
  # YL: note here we use the `resultsiso` instead of `iso`
  res.cqt=array(dim=c(length(resultsiso),3,length(year.u.t)),dimnames = list(resultsiso,c(0.05,0.5,0.95),year.u.t))
  res_ex.cqt=array(dim=c(length(resultsiso),3,length(year.u.t)),dimnames = list(resultsiso,c(0.05,0.5,0.95),year.u.t))

  #Other estimates to show
  res2.cqt=array(dim=c(length(resultsiso2),3,length(year.u.t)),dimnames = list(resultsiso2,c(0.05,0.5,0.95),year.u.t))
  res3.cqt=array(dim=c(length(resultsiso3),3,length(year.u.t)),dimnames = list(resultsiso3,c(0.05,0.5,0.95),year.u.t))

  #u.ct=matrix(NA,ncol=84,nrow=length(iso))
  #t=matrix(NA,ncol=84,nrow=length(iso))

  if (sum(iso %in% iso.i) != length(iso)){
    cat(paste(iso[!iso %in% iso.i], 'is not in the database','\n'))
    iso_no_data=iso[!iso %in% iso.i]
    iso_with_data=iso[iso %in% iso.i]
    isonumber=match(iso_with_data,iso)
    isonodatanumber=match(iso_no_data,iso)
  } else {
    iso_no_data=NULL # usually iso_no_data = "FSM"
    iso_with_data=iso[iso %in% iso.i] # DS added 2021-08-08 or loop below would not work
    isonumber=match(iso_with_data,iso)# DS added 2021-08-08 or loop below would not work
    isonodatanumber=match(iso_no_data,iso)
  }




  ### begin for loop #######
  for (c in isonumber){ # the ith country in the selected iso list
    iso.selected=as.character(iso[c])
    if (iso.selected %in% unique(iso.i)){
      if(sex=="Male"){
        df.selected=df[df$Country.Code==iso.selected & !is.na(df$Male),]
      } else if(sex=="Female"){
        df.selected=df[df$Country.Code==iso.selected & !is.na(df$Female),]
      } else {df.selected=df[df$Country.Code==iso.selected,]}
      name.selected = new.cname.df$OfficialName[new.cname.df$ISO3Code==iso.selected]
      #name.c[c]=name.selected
      vr.ind=grepl("VR|SVR",df.selected$sourcetype.i)

      message("iso.selected is ", iso.selected)

      # initialize lists
      # for add survey data
      u.Lcs.j[[c]]=year.Lcs.j[[c]]=se.Lcs.j[[c]]=included.Lcs.j[[c]]=sourcetype.Lc.s[[c]]=seriesyear.Lc.s[[c]]=
        source.Lc.s[[c]]=hasbias.Lc.s[[c]]=method.Lc.s[[c]]=newentry.Lc.s[[c]]=list()

      # for add vr data
      uvr.Lcs.j[[c]]=yearvr.Lcs.j[[c]]=sevr.Lcs.j[[c]]=includedvr.Lcs.j[[c]]=sourcevr.Lc.s[[c]]=
        isincompletevr.Lcs.j[[c]]=newentryvr.Lc.s[[c]]=list()

      sourcetype.list=vector()
      seriesyear.list=vector()
      source.list=vector()
      sourcevr.list=vector()
      # YL2020
      newentry.list = vector()
      newentryvr.list = vector()

      #===========
      # for non-VR series:
      n.list=unique(df.selected$seriesnameandyear[!vr.ind])
      z=0
      if (sum(!vr.ind)!=0){ # there are non-vr series
        for (n in 1:length(n.list)){
          series.selected=n.list[n]
          if(sex=="Male" & all(is.na(df.selected$Male[df.selected$seriesnameandyear==series.selected]))!=TRUE){
            z=z+1
            u.Lcs.j[[c]][[z]]=df.selected$Male[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)]
            se.Lcs.j[[c]][[z]]=df.selected$Male.SE[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)]
            seriesyear.list=c(seriesyear.list,unique(as.character(df.selected$Series.Year[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)])))

            # YL revise to avoid duplicated legend, allowing plotting function remain unchanged
            sourcetype.list=c(sourcetype.list,
                              unique(as.character(df.selected$sourcetype.i[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)])))
            source.list=c(source.list, unique(as.character(df.selected$Series.Name[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)])))
            newentry.list <- c(newentry.list, unique(df.selected$new_entry[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)]))

            year.Lcs.j[[c]][[z]]=df.selected$Reference.Date[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)]
            #included.Lcs.j[[c]][[z]]=df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)]
            if(indicator=="Infant Mortality Rate"){
              included.Lcs.j[[c]][[z]]=rep(0,length(df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)]))
            } else {
              included.Lcs.j[[c]][[z]]=df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Male)]
            }
          }else if(sex=="Female" & all(is.na(df.selected$Female[df.selected$seriesnameandyear==series.selected]))!=TRUE){
            z=z+1
            u.Lcs.j[[c]][[z]]=df.selected$Female[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)]
            se.Lcs.j[[c]][[z]]=df.selected$Female.SE[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)]
            seriesyear.list=c(seriesyear.list,unique(as.character(df.selected$Series.Year[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)])))
            # YL revised
            sourcetype.list=c(sourcetype.list,
                              unique(as.character(df.selected$sourcetype.i[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)])))
            source.list=c(source.list,unique(as.character(df.selected$Series.Name[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)])))
            newentry.list <- c(newentry.list, unique(df.selected$new_entry[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)]))

            year.Lcs.j[[c]][[z]]=df.selected$Reference.Date[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)]
            #included.Lcs.j[[c]][[z]]=df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)]
            if(indicator=="Infant Mortality Rate"){
              included.Lcs.j[[c]][[z]]=rep(0,length(df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)]))
            } else {
              included.Lcs.j[[c]][[z]]=df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Female)]
            }
          } else if(sex!="Female" & sex!="Male" & all(is.na(df.selected$Sex.Ratio[df.selected$seriesnameandyear==series.selected]))!=TRUE){
            z=z+1
            u.Lcs.j[[c]][[z]]=df.selected$Sex.Ratio[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)]
            se.Lcs.j[[c]][[z]]=df.selected$Sex.Ratio.SE[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)]
            seriesyear.list=c(seriesyear.list,unique(as.character(df.selected$Series.Year[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)])))
            # YL revised 202001
            sourcetype.list=c(sourcetype.list, unique(as.character(df.selected$sourcetype.i[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)])))
            source.list=c(source.list,unique(as.character(df.selected$Series.Name[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)])))
            newentry.list <- c(newentry.list, unique(df.selected$new_entry[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)]))

            year.Lcs.j[[c]][[z]]=df.selected$Reference.Date[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)]
            #included.Lcs.j[[c]][[z]]=df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)]
            #kai made changes 04/10/2018 ->
            if(indicator=="Infant Mortality Rate"){
              included.Lcs.j[[c]][[z]]=rep(0,length(df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)]))
            } else {
              included.Lcs.j[[c]][[z]]=df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected & !is.na(df.selected$Sex.Ratio)]}
          }
        }
      }
      hasbias.Lc.s[[c]]=rep(0,length(n.list))
      method.Lc.s[[c]] <- NULL # YL
      sourcetype.Lc.s[[c]]=sourcetype.list
      seriesyear.Lc.s[[c]]=seriesyear.list
      source.Lc.s[[c]]=source.list
      nseriesnonvr.c[c]=length(n.list)
      newentry.Lc.s[[c]] <- newentry.list

      #================
      # for VR
      includeornot=NA
      n.list.vr=unique(df.selected$seriesnameandyear[vr.ind])
      if(indicator=="Infant Mortality Rate" & !is.null(mlinfo)){
        mlb3info=read.csv(mlinfo,header=TRUE,stringsAsFactors = F)
        imrinclude=subset(mlb3info,mlb3info$iso.c==iso.selected)
        includeornot=ifelse(imrinclude$imrmethod.c=="B3",1,0)
        if(nrow(imrinclude)==0) includeornot = 0 # YL 2022.07
      } else if(indicator=="Infant Mortality Rate" & is.null(mlinfo)){
        includeornot=0
      }
      if (sum(vr.ind)!=0){ # if there are vr series
        for (n in 1:length(n.list.vr)){
          series.selected=n.list.vr[n]
          if(sex=="Male"){
            uvr.Lcs.j[[c]][[n]]=df.selected$Male[df.selected$seriesnameandyear==series.selected]
            sevr.Lcs.j[[c]][[n]]=df.selected$Male.SE[df.selected$seriesnameandyear==series.selected]
          }else if(sex=="Female"){
            uvr.Lcs.j[[c]][[n]]=df.selected$Female[df.selected$seriesnameandyear==series.selected]
            sevr.Lcs.j[[c]][[n]]=df.selected$Female.SE[df.selected$seriesnameandyear==series.selected]
          } else {
            uvr.Lcs.j[[c]][[n]]=df.selected$Sex.Ratio[df.selected$seriesnameandyear==series.selected]
            sevr.Lcs.j[[c]][[n]]=df.selected$Sex.Ratio.SE[df.selected$seriesnameandyear==series.selected]
          }
          yearvr.Lcs.j[[c]][[n]]=df.selected$Reference.Date[df.selected$seriesnameandyear==series.selected]
          if(indicator=="Infant Mortality Rate" & includeornot==1){
            includedvr.Lcs.j[[c]][[n]]=df.selected$Inclusion.U5MR[df.selected$seriesnameandyear==series.selected]
          } else if(indicator=="Infant Mortality Rate" & includeornot==0){includedvr.Lcs.j[[c]][[n]]=rep(0,length(df.selected$Inclusion.U5MR[df.selected$seriesnameandyear==series.selected]))
          } else {includedvr.Lcs.j[[c]][[n]]=df.selected$Inclusion.U5MR[df.selected$seriesnameandyear==series.selected]}

          #includedvr.Lcs.j[[c]][[n]]=df.selected$Inclusion.Gender[df.selected$seriesnameandyear==series.selected]
          isincompletevr.Lcs.j[[c]][[n]]=(rep(0,sum(df.selected$seriesnameandyear==series.selected)))
          sourcevr.list=c(sourcevr.list,unique(as.character(df.selected$seriesnameandyear[df.selected$seriesnameandyear==series.selected])))
          newentryvr.list=c(newentryvr.list, unique(df.selected$new_entry[df.selected$seriesnameandyear==series.selected]))
        }
        sourcevr.Lc.s[[c]]=sourcevr.list
        newentryvr.Lc.s[[c]] <- newentryvr.list # YL 2020

        yearvr.Lc.j[[c]]=unlist(yearvr.Lcs.j[[c]])
        uvr.Lc.j[[c]]=unlist(uvr.Lcs.j[[c]])
      } else {
        yearvr.Lcs.j[[c]][[n]]=NA
        yearvr.Lc.j[[c]]=NA
        #yearvr.Lc.j[[c]]=unlist(yearvr.Lcs.j[[c]])
      }

      maxyear.c[c]=max(df.selected$Reference.Date)
      name.c[c]=as.character(new.cname.df$OfficialName[new.cname.df$ISO3Code==iso.selected])
      #isincompletevr.Lcs.j[[c]]=rep(0,length(n.list.vr))

    }



    ###### result
    # year series for estimation and expectation

    ####=====estimation 1 ###
    if(!is.null(resultsfile_cqt)){
      cn <- which(resultsiso == iso.selected) # position of the iso code in the iso code list
      if(length(cn)==1){
        # this means iso.selected has results
        if(sex=="Male"|sex=="Female"){
          res = resultsfile_cqt[cn,,1:length(year.u.t)]*1000
        } else {
          res = resultsfile_cqt[cn,,1:length(year.u.t)] # for ratio
        }
        res_med=res[2,]
        res_upper=res[3,]
        res_lower=res[1,]
      } else {
        # NA
        res_med  = rep(NA, length(year.u.t))
        res_upper= rep(NA, length(year.u.t))
        res_lower= rep(NA, length(year.u.t))
      }
      res.cqt[cn,1,]=res_lower
      res.cqt[cn,2,]=res_med
      res.cqt[cn,3,]=res_upper
    }

    ####=====estimation 2 ###
    if(!is.null(resultsfile_cqt2)){
      cn2 <- which(resultsiso2 == iso.selected) # position of the iso code in the iso code list
      if(length(cn2)==1){
        # this means iso.selected has results
        if(sex=="Male"|sex=="Female"){
          res = resultsfile_cqt2[cn2,,1:length(year.u.t)]*1000
        } else {
          res = resultsfile_cqt2[cn2,,1:length(year.u.t)] # for ratio
        }
        res_med=res[2,]
        res_upper=res[3,]
        res_lower=res[1,]
      } else {
        # NA
        res_med  = rep(NA, length(year.u.t))
        res_upper= rep(NA, length(year.u.t))
        res_lower= rep(NA, length(year.u.t))
      }
      res2.cqt[cn2,1,]=res_lower
      res2.cqt[cn2,2,]=res_med
      res2.cqt[cn2,3,]=res_upper
    }

    ####=====estimation 3 ###
    if(!is.null(resultsfile_cqt3)){

      cn3 <- which(resultsiso3 == iso.selected) # position of the iso code in the iso code list
      if(length(cn3)==1){
        # this means iso.selected has results
        if(sex=="Male"|sex=="Female"){
          res = resultsfile_cqt3[cn3,,1:length(year.u.t)]*1000
        } else {
          res = resultsfile_cqt3[cn3,,1:length(year.u.t)] # for ratio
        }
        res_med=res[2,]
        res_upper=res[3,]
        res_lower=res[1,]
      } else {
        # NA
        res_med  = rep(NA, length(year.u.t))
        res_upper= rep(NA, length(year.u.t))
        res_lower= rep(NA, length(year.u.t))
      }
      res3.cqt[cn3,1,]=res_lower
      res3.cqt[cn3,2,]=res_med
      res3.cqt[cn3,3,]=res_upper
    }



    ####=====expectation


    if(sex=="Male"|sex=="Female"){
      if(!is.null(expectedresultsfile_cqt)){
        res_ex=expectedresultsfile_cqt[cn,,1:length(year.u.t)]*1000
        res_ex_med=res_ex[2,]
        res_ex_upper=res_ex[3,]
        res_ex_lower=res_ex[1,]
        res_ex.cqt[c,1,]=res_ex_lower
        res_ex.cqt[c,2,]=res_ex_med
        res_ex.cqt[c,3,]=res_ex_upper
      }} else {
        if(!is.null(expectedresultsfile_cqt)){
          res_ex=expectedresultsfile_cqt[cn,,1:length(year.u.t)]
          res_ex_med=res_ex[2,]
          res_ex_upper=res_ex[3,]
          res_ex_lower=res_ex[1,]
          res_ex.cqt[c,1,]=res_ex_lower
          res_ex.cqt[c,2,]=res_ex_med
          res_ex.cqt[c,3,]=res_ex_upper
        }
      }

    #print(c)
  }# end the loop for c


  if (!is.null(iso_no_data) & !identical(iso_no_data,character(0))){
    a=1 # the index in 'iso_no_data'; c is the index in the whole dataset
    for (c in isonodatanumber){
      print(c)
      iso.selected=as.character(iso[c])
      cn <- which(resultsiso==paste(iso.selected)) # position of the iso code in the iso code list
      cat(paste(iso.selected,'has no data in the database so only plot the results','\n'))
      name.c[c]=as.character(new.cname.df$OfficialName[new.cname.df$ISO3Code==iso.selected])

      ### construct
      u.Lcs.j[[c]]=year.Lcs.j[[c]]=se.Lcs.j[[c]]=included.Lcs.j[[c]]=sourcetype.Lc.s[[c]]=seriesyear.Lc.s[[c]]=
        source.Lc.s[[c]]=list()
      hasbias.Lc.s[[c]]=method.Lc.s[[c]]=newentry.Lc.s[[c]] = newentryvr.Lc.s[[c]] = vector()
      uvr.Lcs.j[[c]]=yearvr.Lcs.j[[c]]=sevr.Lcs.j[[c]]=includedvr.Lcs.j[[c]]=sourcevr.Lc.s[[c]]=
        isincompletevr.Lcs.j[[c]] =list()
      u.Lcs.j[[c]][[1]]=NA
      se.Lcs.j[[c]][[1]]=NA
      year.Lcs.j[[c]][[1]]=NA
      included.Lcs.j[[c]][[1]]=NA
      sourcetype.Lc.s[[c]]=NA
      seriesyear.Lc.s[[c]]=NA
      source.Lc.s[[c]]=NA
      uvr.Lcs.j[[c]][[1]]=NULL
      sevr.Lcs.j[[c]][[1]]=NA
      yearvr.Lcs.j[[c]][[1]]=NA
      includedvr.Lcs.j[[c]][[1]]=NA
      isincompletevr.Lcs.j[[c]][[1]]=NA
      sourcevr.Lc.s[[c]]=NA
      yearvr.Lc.j[[c]]=NA
      uvr.Lc.j[[c]]=NA
      #### end of construct mcmc.meta

      ### add results for no data countries

      ####=====estimation 1 ###
      if(!is.null(resultsfile_cqt)){
        cn <- which(resultsiso == iso.selected) # position of the iso code in the iso code list
        if(length(cn)==1){
          # this means iso.selected has results
          if(sex=="Male"|sex=="Female"){
            res = resultsfile_cqt[cn,,1:length(year.u.t)]*1000
          } else {
            res = resultsfile_cqt[cn,,1:length(year.u.t)] # for ratio
          }
          res_med=res[2,]
          res_upper=res[3,]
          res_lower=res[1,]
        } else {
          # NA
          res_med  = rep(NA, length(year.u.t))
          res_upper= rep(NA, length(year.u.t))
          res_lower= rep(NA, length(year.u.t))
        }
        res.cqt[cn,1,]=res_lower
        res.cqt[cn,2,]=res_med
        res.cqt[cn,3,]=res_upper
      }

      ####=====estimation 2 ###
      if(!is.null(resultsfile_cqt2)){
        cn2 <- which(resultsiso2 == iso.selected) # position of the iso code in the iso code list
        if(length(cn2)==1){
          # this means iso.selected has results
          if(sex=="Male"|sex=="Female"){
            res = resultsfile_cqt2[cn2,,1:length(year.u.t)]*1000
          } else {
            res = resultsfile_cqt2[cn2,,1:length(year.u.t)] # for ratio
          }
          res_med=res[2,]
          res_upper=res[3,]
          res_lower=res[1,]
        } else {
          # NA
          res_med  = rep(NA, length(year.u.t))
          res_upper= rep(NA, length(year.u.t))
          res_lower= rep(NA, length(year.u.t))
        }
        res2.cqt[cn2,1,]=res_lower
        res2.cqt[cn2,2,]=res_med
        res2.cqt[cn2,3,]=res_upper
      }

      ####=====estimation 3 ###
      if(!is.null(resultsfile_cqt3)){

        cn3 <- which(resultsiso3 == iso.selected) # position of the iso code in the iso code list
        if(length(cn3)==1){
          # this means iso.selected has results
          if(sex=="Male"|sex=="Female"){
            res = resultsfile_cqt3[cn3,,1:length(year.u.t)]*1000
          } else {
            res = resultsfile_cqt3[cn3,,1:length(year.u.t)] # for ratio
          }
          res_med=res[2,]
          res_upper=res[3,]
          res_lower=res[1,]
        } else {
          # NA
          res_med  = rep(NA, length(year.u.t))
          res_upper= rep(NA, length(year.u.t))
          res_lower= rep(NA, length(year.u.t))
        }
        res3.cqt[cn3,1,]=res_lower
        res3.cqt[cn3,2,]=res_med
        res3.cqt[cn3,3,]=res_upper
      }


      # if(!is.null(expectedresultsfile_cqt)){
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

      a=a+1
    }

  } #there is country with no data


  c=length(resultsiso)
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
  sourcetype.list=NA
  seriesyear.list=NA
  source.list=NA
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


  if (is.null(resultsfile_cqt))
    res.cqt=NULL

  if (is.null(expectedresultsfile_cqt))
    res_ex.cqt=NULL


  data=list(
    name.c=name.c,
    c=c,
    u.Lcs.j=u.Lcs.j,
    year.Lcs.j=year.Lcs.j,
    se.Lcs.j=se.Lcs.j,
    included.Lcs.j=included.Lcs.j,
    nseriesnonvr.c=nseriesnonvr.c,
    sourcetype.Lc.s=sourcetype.Lc.s,
    method.Lc.s=method.Lc.s,
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
    uvr.Lc.j=uvr.Lc.j
  )

  df$sourcename <- paste(df$Country.Code, df$seriesnameandyear, sep = "_")
  new_series = unique(df$sourcename[df$new_entry==1])
  return(
    list(sex=sex,
         indicator = indicator,
         indicator_label = indicator_label,
         data=data,
         res.cqt=res.cqt,
         res2.cqt=res2.cqt,
         res3.cqt=res3.cqt,
         res_ex.cqt=res_ex.cqt,
         year.t=year.u.t,
         iso=iso,
         new_series = new_series,
         new_entry_date = new_entry_date,
         iso_no_data=iso_no_data)
  )
}
