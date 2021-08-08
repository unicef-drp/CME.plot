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

dir_IHME_data<- file.path(user,"/Dropbox/UN IGME data/2020 Round Estimation/Code/output/IHME")
# Under 5 by sex and total
u5mr.file<-"IHME_GBD_2019_MORTALITY_1950_2019_5Q0_WSHOCK_Y2020M07D31.CSV"
# IMR by sex and total
imr.file<-"IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_28_WSHOCK_Y2020M07D31.CSV"
# CMR by sex and total
cmr.file<-"IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_5_WSHOCK_Y2020M07D31.CSV"
# NMR total
est.file<-"GBD 2019_5q0Results_estimates 20201016.csv"


# read in data
u5mr <-read.csv(file.path(dir_IHME_data, u5mr.file), stringsAsFactors = F)
imr <- read.csv(file.path(dir_IHME_data, imr.file), stringsAsFactors = F)
cmr <- read.csv(file.path(dir_IHME_data, cmr.file), stringsAsFactors = F)
est <- read.csv(file.path(dir_IHME_data, est.file), stringsAsFactors = F)


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
sheets <- list("U5MR" = u5mr[u5mr$metric_name=="Probability of death" & u5mr$location_id %in%IHME_codebook$Location_ID,],
               "NMR" = nmr[nmr$location_id %in%IHME_codebook$Location_ID,],
               "IMR" = imr[imr$metric_name=="Probability of death" & imr$location_id %in%IHME_codebook$Location_ID,],
               "CMR"=cmr[cmr$metric_name=="Probability of death" & cmr$location_id %in%IHME_codebook$Location_ID,],
               "Ratio"=ratio)
write_xlsx(sheets,
           file.path(user, "Dropbox/UN IGME data/2020 Round Estimation/Code/output/IHME/GBD2019_Under5_estimates.xlsx"))



# 5-24 --------------------------------------------------------------------
# Format IHME output files
user <- Sys.getenv("USERPROFILE")
dir_IHME_data<- file.path(user,"/Dropbox/UN IGME data/2020 Round Estimation/Code/output/IHME")
dir_list <- list(
  m5_9.file   = file.path(dir_IHME_data, "IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_6_WSHOCK_Y2020M07D31_5-9.CSV"),
  m10_14.file = file.path(dir_IHME_data, "IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_7_WSHOCK_Y2020M07D31_10-14.CSV"),
  m15_19.file = file.path(dir_IHME_data, "IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_8_WSHOCK_Y2020M07D31_15-19.CSV"),
  m20_24.file = file.path(dir_IHME_data, "IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_9_WSHOCK_Y2020M07D31_20-24.CSV")
)

library(data.table)
dt_ihme <- rbindlist(lapply(dir_list, fread))
dt_ihme[, table(age_group_name)]
dt_ihme <- dt_ihme[metric_name == "Probability of death" & location_id %in% IHME_codebook$Location_ID]
dt_ihme_5_24 <- get_dt_IHME_2019(ihme = dt_ihme)
dt_ihme_5_24[, ind:=as.factor(ind)]
levels(dt_ihme_5_24$ind)
levels(dt_ihme_5_24$ind) <- c("5q10", "5q15", "5q20", "5q5")
dt_ihme_5_24l <- melt.data.table(dt_ihme_5_24, measure.vars = c("lower", "mean", "upper"),
                      variable.name = "quintile", variable.factor = FALSE)
dt_ihme_5_24lw <- dcast.data.table(dt_ihme_5_24l, ...~ind)
get.5q0 <- function(q1, q4) (1 - (1 - q1 / 1E3) * (1 - q4 / 1E3)) * 1E3
dt_ihme_5_24lw[, `:=`(`10q5` = get.5q0(q1 = `5q5`, q4 = `5q10`),
                      `10q15` = get.5q0(q1 = `5q15`, q4 = `5q20`))]
ihme_5_24_l <- melt.data.table(dt_ihme_5_24lw, id.vars = c("location_id", "year",  "sex", "quintile"),
                               variable.name = "ind", variable.factor = FALSE
                               )
ihme_2019_5_24 <- dcast(ihme_5_24_l, ...~quintile)
# to check: location_id: 6 is China, 27 in u5mr.iso.c
fwrite(ihme_2019_5_24,
       file.path(Sys.getenv("USERPROFILE"), "Dropbox/UN IGME data/2020 Round Estimation/Code/output/IHME/GBD2019_5-24_estimates.csv"))



# WPP  --------------------------------------------------------------------


