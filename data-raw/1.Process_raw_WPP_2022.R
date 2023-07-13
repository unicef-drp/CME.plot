# prepare WPP 2022 for plotting

# data downloaded from https://population.un.org/wpp/Download/Standard/CSV/
# Extract the nqx directly from the abridged life table

# Life Tables
# 1950-2021, medium (ZIP, 68.72 MB)
# Abridged life tables up to age 100 by sex and both sexes combined providing a
# set of values showing the mortality experience of a hypothetical group of
# infants born at the same time and subject throughout their lifetime to the
# specific mortality rates of a given year, from 1950 to 2100. Only medium is
# available.


library("data.table")
USERPROFILE <- Sys.getenv("USERPROFILE")
dir.wpp <- file.path(USERPROFILE, "Dropbox/UNICEF Work/WPP/")
dir_wpp_files <- file.path(dir.wpp, "WPP 2022/WPP2022_Life_Table_Abridged_Medium_1950-2021.csv")
dir_wpp_files_proj <- file.path(dir.wpp, "WPP 2022/WPP2022_Life_Table_Abridged_Medium_2022-2100.csv")

# load all life tables
dt_wpp <- fread(dir_wpp_files)
dt_wpp[1,]
dt_wpp_proj <- fread(dir_wpp_files_proj)
dt_wpp_proj[1]
format.WPP.life.table <- function(dt_wpp){
  message("Range of Year is: ", paste(range(dt_wpp$MidPeriod), collapse = "-"))
  # dc <- CME.assistant::get.country.info.CME(2022)
  # dc$ISO3Code[!dc$ISO3Code %in% dt_wpp$ISO3_code]
  dt_wpp <- dt_wpp[!is.na(ISO3_code) & ISO3_code!=""]
  dt_wpp[, ISO3Code := ISO3_code]
  dt_wpp[ISO3_code=="XKX", ISO3Code := "RKS"]
  dt_wpp[, value:= qx * 1000]
  dt_wpp <- dt_wpp[AgeGrpStart<=20]
  dt_wpp[, Year:= floor(MidPeriod)]
  dt_wpp_2022 <- dcast.data.table(dt_wpp, ISO3Code + LocID + Year + Sex ~ AgeGrpStart)
  setnames(dt_wpp_2022, c("0", "1", "5", "10", "15", "20"), c("IMR", "CMR", "5q5", "5q10", "5q15", "5q20"))
  get.5q0 <- function(q1, q4) (1 - (1 - q1 / 1E3) * (1 - q4 / 1E3)) * 1E3
  dt_wpp_2022[, `:=`(
    U5MR = get.5q0(q1 = IMR, q4 = CMR),
    `10q5` = get.5q0(q1 = `5q5`, q4 = `5q10`),
    `10q15` = get.5q0(q1 = `5q15`, q4 = `5q20`))]
  dt_wpp_2022[, Sex:= dplyr::recode(Sex, "Female" = "f", "Male" = "m", "Total" = "both")]
  return(dt_wpp_2022)
}
dt_wpp_2022 <- format.WPP.life.table(dt_wpp)
dt_wpp_2022_proj <- format.WPP.life.table(dt_wpp_proj)

# dt_ws <- dcast(dt_wpp, ISO3Code + Year + AgeGrpStart ~ Sex, value.var = "value")
# dt_ws[, flag:=NA_character_]
# dt_ws[(Female > Total) & (Male > Total) , flag := "Both sexes higher than total"]
# dt_ws[(Female < Total) & (Male < Total) , flag := "Both sexes lower than total"]
# dt_ws[!is.na(flag), `:=`(pntf = abs(Female/Total - 1), pntm = abs(Male/Total - 1))]
# dt_ws[!is.na(flag), `:=`(pnt = pmin(pntf, pntm))]
# dt_ws[!is.na(flag),]
usethis::use_data(dt_wpp_2022, overwrite = TRUE)

fwrite(dt_wpp_2022,
       file.path(dir.wpp, "WPP 2022/WPP2022-LT_extract_0_24_wide_ind_1950-2021.csv"))

dt_wpp_2022_all_year <- rbindlist(list(dt_wpp_2022, dt_wpp_2022_proj))
setorder(dt_wpp_2022_all_year, ISO3Code)
fwrite(dt_wpp_2022_all_year,
       file.path(dir.wpp, "WPP 2022/WPP2022-LT_extract_0_24_wide_ind_1950-2100.csv"))
