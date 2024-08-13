# Process raw IGME downloaded data
# IHME 2021
# collect all estimates into one file with 5 indicators: U5MR, NMR, IMR, CMR, NMR/U5MR Ratio

# updated 2024.05
# https://ghdx.healthdata.org/gbd-2021
# GBD Results
# Search and download GBD 2021 estimates with the GBD Results tool.
# U5MR downloaded seperately, from 1950, LT from 1980

library("data.table")
# Format IHME output files
USERPROFILE <- Sys.getenv("USERPROFILE")

dir_IHME_data <- file.path(USERPROFILE, "Dropbox/UNICEF Work/IHME/GBD 2021")

dtLT <- rbindlist(lapply(list.files(file.path(dir_IHME_data, "raw"), full.names = TRUE), fread))

load("data/IHME_codebook.rda")

# subset age group
unique(dtLT[,.(age_name, age_id)])
recode_age_group <- c(
  "<28 days" = "NMR",
  "<1 year"  = "IMR",
  "<5 years" = "U5MR",
  "12-23 months" = "y1-2",
  "2-4 years"= "y2-4",
  "5-9 years" = "5q5",
  "10-14 years" = "5q10",
  "15-19 years" = "5q15",
  "20-24 years" = "5q20"
)
dtLT <- dtLT[age_name %in% names(recode_age_group)]
dtLT[, Shortind := dplyr::recode(age_name, !!!recode_age_group)]
dtLT[, Sex := dplyr::recode(sex_name, "Male" = "m", "Female" = "f", "Both" = "both")]
setnames(dtLT, "year", "Year")
dtLT[, `:=`(Median = val * 1000, Lower = lower * 1000, Upper = upper * 1000)]

dtLT1 <- dtLT[location_id %in% IHME_codebook$Location_ID ,.(location_id, Year, Sex, Shortind, Median, Lower, Upper)]
get.5q0 <- function(q1, q4) (1 - (1 - q1 / 1E3) * (1 - q4 / 1E3)) * 1E3

# the derived indicators don't have Lower and Upper
dtLTw <- dcast(dtLT1, location_id + Year + Sex ~ Shortind, value.var = "Median")
dtLTw[, `:=`(CMR = get.5q0(q1 = `y1-2`, q4 = `y2-4`))]
dtLTw[, `:=`(`10q5`  = get.5q0(q1 = `5q5`, q4 = `5q10`),
            `10q15` = get.5q0(q1 = `5q15`, q4 = `5q20`),
            `U5MR2`  = get.5q0(q1 = `IMR`,  q4 = `CMR`),  # for checking purpose, check against the downloaded `U5MR`
            Ratio = NMR / U5MR)]

dtLTw.extraind <- melt.data.table(dtLTw, id.vars = c("location_id",  "Year",  "Sex"),
                               variable.name = "Shortind", variable.factor = FALSE, value.name = "Median")
dtLTw.extraind <- dtLTw.extraind[Shortind %in% c("10q5", "10q15", "CMR", "Ratio") & !is.na(Median)]

dtLT2 <- rbindlist(list(dtLT1, dtLTw.extraind), fill = TRUE)
dtLT2 <- dtLT2[!Shortind %in% c("y1-2", "y2-4")]
dtLT2[, table(Sex, Shortind)]

setorder(dtLT2, location_id, -Shortind, -Year, Sex)
dt_gbd_2021 <- merge(IHME_codebook[,.(Location_ID, ISO3Code, OfficialName)], dtLT2, by.x = "Location_ID", by.y = "location_id")
fwrite(dt_gbd_2021, file.path(dir_IHME_data, "GBD2021_estimates.csv"))

# Save the data
# all cqt will be derived directly from it
usethis::use_data(dt_gbd_2021, overwrite = TRUE)


