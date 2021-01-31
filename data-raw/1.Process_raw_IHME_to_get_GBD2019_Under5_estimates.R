# Process raw IGME downloaded data to get "GBD2019_Under5_estimates.xlsx"
# collect all estimates into one excel file with 5 sheets: U5MR, NMR, IMR, CMR, Ratio
# Lucia Hug
# 2020/10

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
# Format IHME output files
user <- Sys.getenv("USERPROFILE")

in.dir<- file.path(user,"/Dropbox/UN IGME data/2020 Round Estimation/Code/output/IHME")
# Under 5 by sex and total
u5mr.file<-"IHME_GBD_2019_MORTALITY_1950_2019_5Q0_WSHOCK_Y2020M07D31.CSV"
# IMR by sex and total
imr.file<-"IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_28_WSHOCK_Y2020M07D31.CSV"
# CMR by sex and total
cmr.file<-"IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_5_WSHOCK_Y2020M07D31.CSV"
# NMR total
est.file<-"GBD 2019_5q0Results_estimates 20201016.csv"


#read
setwd(in.dir)
u5mr<-read.csv(u5mr.file,stringsAsFactors = F)
imr<-read.csv(imr.file,stringsAsFactors = F)
cmr<-read.csv(cmr.file,stringsAsFactors = F)
est<-read.csv(est.file,stringsAsFactors = F)
codes<-read_xlsx("IHME codebook.xlsx")
igme.codes<-codes[!is.na(codes$ISO3Code),]

nmr<-filter(est,age_group%in%c("0-6 days","7-27 days"))%>%
  gather(bound,val,mean:lower)%>%
  select(-age_group)%>%
  unite(bd_age,bound,age_group_id)%>%
  spread(bd_age,val)%>%
  mutate(mean=1-(1-mean_3)*(1-mean_2),
         lower=NA, upper=NA,age_group_id=0,age_group="Neonatal")

nmr<-nmr[,names(est)]

ratio<-filter(est,age_group%in%c("0-6 days","7-27 days","<5 years"))%>%
  gather(bound,val,mean:lower)%>%
  select(-age_group)%>%
  unite(bd_age,bound,age_group_id)%>%
  spread(bd_age,val)%>%
  mutate(nmr=1-(1-mean_3)*(1-mean_2),mean=nmr/mean_1,
         lower=NA, upper=NA,age_group_id=0,age_group="ratio NMR/U5MR")

ratio<-ratio[,names(est)]

# write.csv(nmr,"GBD 2019_NMRResults_estimates.csv",row.names=F)
sheets <- list("U5MR" = u5mr[u5mr$metric_name=="Probability of death" & u5mr$location_id %in% igme.codes$`Location ID`,],
               "NMR" = nmr[nmr$location_id %in% igme.codes$`Location ID`,],
               "IMR" = imr[imr$metric_name=="Probability of death" & imr$location_id %in% igme.codes$`Location ID`,],
               "CMR"=cmr[cmr$metric_name=="Probability of death" & cmr$location_id %in% igme.codes$`Location ID`,],
               "Ratio"=ratio)
write_xlsx(sheets,
           file.path(user, "Dropbox/UN IGME data/2020 Round Estimation/Code/output/IHME/GBD2019_Under5_estimates.xlsx"))
