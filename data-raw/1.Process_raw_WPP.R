# prepare WPP 2025 for plotting
# updated 2025.05

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

# If use LT:

# dir_wpp_files <- file.path(dir.wpp, "WPP 2022/WPP2022_Life_Table_Abridged_Medium_1950-2021.csv")
# dir_wpp_files_proj <- file.path(dir.wpp, "WPP 2022/WPP2022_Life_Table_Abridged_Medium_2022-2100.csv")
#
# dt_wpp <- fread(dir_wpp_files)
# dt_wpp[1,]
# dt_wpp_proj <- fread(dir_wpp_files_proj)
# dt_wpp_proj[1]


# If use the qx table:


dir_wpp_files.t <- file.path(dir.wpp, "WPP 2025/WPP2024_Probability_of_dying_by_age_(qx)_Abridged_Ages_Total.xlsx")
dir_wpp_files.f <- file.path(dir.wpp, "WPP 2025/WPP2024_Probability_of_dying_by_age_(qx)_Abridged_Ages_Female.xlsx")
dir_wpp_files.m <- file.path(dir.wpp, "WPP 2025/WPP2024_Probability_of_dying_by_age_(qx)_Abridged_Ages_Male.xlsx")

dt0 <- readxl::read_xlsx(dir_wpp_files.t, sheet = "Estimates")
str(dt0)
ncol <- ncol(dt0)
ncol_year <- which(colnames(dt0) == "Year")
dt0 <- readxl::read_xlsx(dir_wpp_files.t, sheet = "Estimates",
                         col_types = c(rep("text", (ncol_year-1)), rep("numeric", (ncol-ncol_year+1))))
str(dt0)

# load all life tables
f1 <- function(x) setDT(readxl::read_xlsx(x, sheet = "Estimates",
                                          col_types = c(rep("text", (ncol_year-1)), rep("numeric", (ncol-ncol_year+1)))))
dt1 <- f1(dir_wpp_files.t)
dt_wpp0 <- rbindlist(lapply(list(dir_wpp_files.t, dir_wpp_files.m, dir_wpp_files.f), f1))
dt_wpp0[1,]

# we want a long format LT variable.name = AgeGrpStart, value.name = qx
dt_wpp <- melt(dt_wpp0, id.vars = c("ISO3_code", "Sex", "Year"), measure.vars = c("0", "1", "5", "10", "15", "20"),
               variable.name = "AgeGrpStart", value.name = "qx", variable.factor = FALSE)

# next year, keep XKX, still revise to RKS for now
dt_wpp[ISO3_code == "XKX", ISO3_code := "RKS"]

format.WPP.life.table <- function(dt_wpp){
  message("Range of Year is: ", paste(range(dt_wpp$Year), collapse = "-"))
  # dc <- CME.assistant::get.country.info.CME(2025)
  # dc$ISO3Code[!dc$ISO3Code %in% dt_wpp$ISO3_code]
  stopifnot("ISO3_code" %in% colnames(dt_wpp))
  dt_wpp <- dt_wpp[!is.na(ISO3_code) & ISO3_code!=""]
  dt_wpp[, ISO3Code := ISO3_code]
  dt_wpp[, value:= qx * 1000]
  dt_wpp[, AgeGrpStart:= as.numeric(AgeGrpStart)]
  dt_wpp <- dt_wpp[AgeGrpStart<=20]
  dt_wpp[, Year:= floor(Year)]
  dt_wpp_2024 <- dcast.data.table(dt_wpp, ISO3Code  + Year + Sex ~ AgeGrpStart)
  setnames(dt_wpp_2024, c("0", "1", "5", "10", "15", "20"), c("IMR", "CMR", "5q5", "5q10", "5q15", "5q20"))
  get.5q0 <- function(q1, q4) (1 - (1 - q1 / 1E3) * (1 - q4 / 1E3)) * 1E3
  dt_wpp_2024[, `:=`(
    U5MR = get.5q0(q1 = IMR, q4 = CMR),
    `10q5` = get.5q0(q1 = `5q5`, q4 = `5q10`),
    `10q15` = get.5q0(q1 = `5q15`, q4 = `5q20`))]
  dt_wpp_2024[, Sex:= dplyr::recode(Sex, "Female" = "f", "Male" = "m", "Total" = "both")]
  return(dt_wpp_2024)
}
dt_wpp_2024 <- format.WPP.life.table(dt_wpp)
usethis::use_data(dt_wpp_2024, overwrite = TRUE)

# dt_wpp_2024_proj <- format.WPP.life.table(dt_wpp_proj)

# dt_ws <- dcast(dt_wpp, ISO3Code + Year + AgeGrpStart ~ Sex, value.var = "qx")
# dt_ws[, flag:=NA_character_]
# dt_ws[(Female > Total) & (Male > Total) , flag := "Both sexes higher than total"]
# dt_ws[(Female < Total) & (Male < Total) , flag := "Both sexes lower than total"]
# dt_ws[!is.na(flag), `:=`(pntf = abs(Female/Total - 1), pntm = abs(Male/Total - 1))]
# dt_ws[!is.na(flag), `:=`(pnt = pmin(pntf, pntm))]
# dt_ws[!is.na(flag),] # there are a few, but very small differences

#
fwrite(dt_wpp_2024,
       file.path(dir.wpp, "WPP 2025/WPP2024-Life Table qx_extract_0_24_wide_ind_1950-2023.csv"))
