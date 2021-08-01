devtools::load_all(".")

runname.global <- "GR20210615" ##<< Global run to use. Do not change

workdir <- "C:/Users/lyhel/Dropbox/UN IGME Data/2021 Round Estimation/Code"
iso.select <- "JPN" ##<< 3-character ISO country code of country to run for
runname <-  paste0(runname.global, "_", iso.select) ##<< Run name
output.dir <- file.path(workdir, "output", runname)
output.dir.2020 <- file.path(workdir, "output", "IGME2020")
res.cqt.Lw.2020 <- get.cqt.from.results(output_dir = output.dir.2020)

# crisis free
load(file.path(workdir, "output", runname, "res.crisisremoved.cqt.Lw.rda"))

load(file.path(output.dir.2020, "iso.c.rda"))
iso.c2 <- iso.c

# crisis free
load(file.path(workdir, "output", runname, "res.crisisremoved.cqt.Lw.rda"))
load(file.path(workdir, "output", runname, "year.t.rda"))
load(file.path(workdir, "output", runname, "res.cqt.Lw.rda"))
load(file.path(workdir, "output", runname, "iso.c.rda"))
load(file.path(workdir, "output", runname, "mcmc.meta.rda"))

res.cqt2 = res.cqt.Lw.2020$`0.5`
dimnames(res.cqt2)
res.cqt2.2 <- match.cqt.core(iso.c1 = iso.c, year.t1 = year.t, res.cqt2 = res.cqt2)

pooling_weight = 0.5
res.cqt2.1 <- obtain.matched.cqt(output.dir1 = output.dir,
                               output.dir2 = output.dir.2020,
                               pooling_weight = pooling_weight)
identical(dimnames(res.cqt2.1), dimnames(res.cqt2.2)) # TRUE
dimnames(res.cqt2.1)

# WPP and IHME
wpp.cqt <- match.cqt.iso(iso.c1 = iso.c, res.cqt2 = u5mr.wpp.cqt.2019)
dimnames(wpp.cqt)
ihme.cqt <- match.cqt.iso(iso.c1 = iso.c, res.cqt2 = u5mr.ihme.cqt.2019)
dimnames(ihme.cqt)

wpp_and_completeihme = list(wpp.cqt = wpp.cqt, ihme.cqt = ihme.cqt)
data.all <- mcmc.meta$data.all

year.ihme.cqt <- as.numeric(dimnames(ihme.cqt)[[3]])
year.wpp.cqt <- as.numeric(dimnames(wpp.cqt)[[3]])
c=1
PlotCIs(c = c, CIs.cqt = wpp.cqt, year.t = year.wpp.cqt, col.est = "red", col.CI = "green")

PlotDataAndEstimates2020(
  data.all = data.all,
  est.years = year.t,
  CIs.cqt = res.cqt2.1,
  wpp_and_completeihme = list(wpp.cqt = wpp.cqt, ihme.cqt = ihme.cqt),
  c = 1,
  zoom = FALSE
)

#  run from here ----------------------------------------------------------



library(CME.plot)
runname.global <- "GR20210615" ##<< Global run to use. Do not change

workdir <- "C:/Users/lyhel/Dropbox/UN IGME Data/2021 Round Estimation/Code"
iso.select <- "SDN" ##<< 3-character ISO country code of country to run for
runname <-  paste0(runname.global, "_", iso.select) ##<< Run name
output.dir <- file.path(workdir, "output", runname)
output.dir.2020 <- file.path(workdir, "output", "IGME2020")
res.cqt.Lw.2020 <- get.cqt.from.results(output_dir = output.dir.2020)
load(file.path(workdir, "output", runname, "res.crisisremoved.cqt.Lw.rda"))

# Only the included crises
savePlotResults(runname = runname,
                output.dir = output.dir,
                After_CC = TRUE,
                filename = "Selected crises",
                year.end = 2021,
                zoom.year.end = 2021,
                # res.cqt1 = res.cqt.Lw$`0.5`,
                res.cqt2 = res.cqt.Lw.2020$`0.5`,
                res.cqt3 = res.crisisremoved.cqt.Lw$`0.5`,
                legend1 = "Draft IGME 2021 (selected crises)",
                legend2 = "UN IGME 2020",
                legend3 = "Draft IGME 2021 crisis-free",
                wpp.cqt = u5mr.wpp.cqt.2019,
                ihme.cqt = u5mr.ihme.cqt.2019,
                col.CI = NULL, # suppress all the CI
                col.CI2 = NULL,
                col.ihme = NULL)


# crisis free
load(file.path(output.dir.2020, "res.crisisremoved.cqt.Lw.rda"))
load(file.path(output.dir.2020, "year.t.rda"))
load(file.path(output.dir.2020, "res.cqt.Lw.rda"))
load(file.path(output.dir.2020, "iso.c.rda"))
load(file.path(output.dir.2020, "mcmc.meta.rda"))

PlotDataAndEstimates2020(
  data.all = mcmc.meta$data.all,
  est.years = year.t,
  CIs.cqt = res.cqt.Lw$`0.5`,
  wpp_and_completeihme = list(wpp.cqt = wpp.cqt, ihme.cqt = ihme.cqt),
  # wpp_and_completeihme = list(wpp.cqt = u5mr.wpp.cqt.2019, ihme.cqt = u5mr.ihme.cqt.2019),
  c = 150,
  zoom = FALSE
)
