# Process raw IGME downloaded data
# IHME 2021
# collect all estimates into one file with 5 indicators: U5MR, NMR, IMR, CMR, NMR/U5MR Ratio

# updated 2025.05
# https://vizhub.healthdata.org/gbd-results/
# GBD Results
# Search and download GBD 2021 estimates with the GBD Results tool.
# U5MR downloaded separately, from 1950, LT from 1980

library("data.table")
# Format IHME output files
USERPROFILE <- Sys.getenv("USERPROFILE")

dir_IHME_data <- file.path(USERPROFILE, "Dropbox/UNICEF Work/IHME/GBD 2023")

dtLT <- rbindlist(lapply(list.files(file.path(dir_IHME_data, "AllCause"), full.names = TRUE), fread))
dtLT[, table(age_name)]
load("data/IHME_codebook.rda")

# subset age group
unique(dtLT[,.(age_name, age_id)])
recode_age_group <- c(
  "<28 days" = "NMR",
  "<1 year"  = "IMR",
  "2-4 years"  = "CMR",
  "<5 years"   = "U5MR",
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
dtLT1[, table(Sex, Shortind)]
get.cme <- function(q1, q5) {(1 - (1 - q5 / 1E3) / (1 - q1 / 1E3)) * 1E3}
get.5q0 <- function(q1, q4) (1 - (1 - q1 / 1E3) * (1 - q4 / 1E3)) * 1E3

# the derived indicators don't have Lower and Upper
dtLTw <- dcast(dtLT1, location_id + Year + Sex ~ Shortind, value.var = "Median")
dtLTw[, `:=`(`10q5`  = get.5q0(q1 = `5q5`, q4 = `5q10`),
            `10q15`  = get.5q0(q1 = `5q15`, q4 = `5q20`),
            Ratio = NMR / U5MR)]

dtLTw.extraind <- melt.data.table(dtLTw, id.vars = c("location_id",  "Year",  "Sex"),
                               variable.name = "Shortind", variable.factor = FALSE, value.name = "Median")
dtLTw.extraind <- dtLTw.extraind[Shortind %in% c("10q5", "10q15", "CMR", "Ratio") & !is.na(Median)]

dtLT2 <- rbindlist(list(dtLT1, dtLTw.extraind), fill = TRUE)
dtLT2 <- dtLT2[!Shortind %in% c("y1-2", "y2-4")]
dtLT2[, table(Sex, Shortind)]
dtLT2[, table(Year)]
setorder(dtLT2, location_id, -Shortind, -Year, Sex)
dt_gbd_output <- merge(IHME_codebook[,.(Location_ID, ISO3Code, OfficialName)], dtLT2, by.x = "Location_ID", by.y = "location_id")
fwrite(dt_gbd_output, file.path(dir_IHME_data, "GBD2023_estimates.csv"))

# Save the data
# all cqt will be derived directly from it
usethis::use_data(dt_gbd_output, overwrite = TRUE)

ind_wanted <- c("NMR", "U5MR", "5q5", "5q10", "5q15", "5q20")
