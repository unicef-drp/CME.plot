devtools::load_all(".")

runname.global <- "GR20220516_all" ##<< Global run to use. Do not change
workdir <- "C:/Users/lyhel/Dropbox/UN IGME Data/2022 Round Estimation/Code"
output.dir1 <- file.path(workdir, "output", runname.global)
output.dir2 <- file.path(workdir, "output", "IGME2020")
load(file.path(output.dir2, "res.cqt.Lw.rda"))
# res.cqt.Lw.2020 <- get.cqt.from.results(output_dir = output.dir2)
load(file.path(output.dir2, "res.cqt.Lw.rda"))
res.cqt.Lw.2020 <- res.cqt.Lw$`0.5`
res.cqt.Lw.2020[107,,]

res.cqt2 <- obtain.matched.cqt(output.dir1 = output.dir1,
                               output.dir2 = output.dir2,
                               pooling_weight = "0.5")
dimnames(res.cqt2)
res.cqt2[107,,]

load(file.path(output.dir1, "year.t.rda"))
load(file.path(output.dir1, "mcmc.meta.rda"))
u5mr.iso.c <- mcmc.meta$data.all$iso.c
usethis::use_data(u5mr.iso.c, overwrite = TRUE)

data.all <- mcmc.meta$data.all
res.cqt2 <- match.cqt.core(u5mr.iso.c1 = u5mr.iso.c, year.t1 = year.t, res.cqt2 = res.cqt2)

year.start = NULL; year.end = 2021
wpp.cqt <- set.cqt.year.limit(u5mr.wpp.cqt.2019, year_start = year.start, year_end = year.end)
ihme.cqt <- set.cqt.year.limit(CME.plot::u5mr.ihme.cqt.2019, year_start = year.start, year_end = year.end)
wpp_and_completeihme = list(wpp.cqt = wpp.cqt, ihme.cqt = ihme.cqt)

output.dir.21.CC <- file.path(Sys.getenv("USERPROFILE"),
                              "/Dropbox/IGME 5-14/2021 Round Estimation 10q5/output/10q5-IGME2021GLOBALRUN-4_all")
load(file.path(output.dir.21.CC, "res.cqt.Lw.rda"))
res.cqt.Lw.5_14 <- res.cqt.Lw$`0.5`

load(file.path(output.dir.21.CC, "year.t.rda"))
load(file.path(output.dir.21.CC, "mcmc.meta.rda"))
u5mr.iso.c <- mcmc.meta$data.all$u5mr.iso.c
res.cqt1 <- match.cqt.core(u5mr.iso.c1 = u5mr.iso.c, year.t1 = year.t, res.cqt2 = res.cqt.Lw.2020)

savePlotResults(runname = "test1",
                # new_entry_date = "2020-10",
                output.dir = output.dir.21.CC,
                wpp.cqt = m10q5.wpp.cqt.2019,
                ihme.cqt = m10q5.ihme.cqt.2019,
                remove_date_5_24 = 0,
                ylab = "10q5",
                # res.cqt1 = res.cqt.Lw.2020,
                res.cqt2 = res.cqt.Lw.5_14,
                legend2 = "UN IGME 2020",
                fig.dir = "fig",
                col.CI_IHME = NULL,
                zoom.year.start = 1990,
                n.countries = 1:20)

PlotDataAndEstimates2020(
  data.all = data.all,
  est.years = year.t,
  CIs.cqt = res.cqt2,
  wpp_and_completeihme = list(wpp.cqt = wpp.cqt, ihme.cqt = ihme.cqt),
  c = 1,
  zoom = FALSE
)

# crisis free
load(file.path(workdir, "output", runname2020, "res.cqt.Lw.rda"))

load(file.path(output.dir2, "u5mr.iso.c.rda"))
u5mr.iso.c2 <- u5mr.iso.c

# crisis free
load(file.path(workdir, "output", runname, "res.crisisremoved.cqt.Lw.rda"))
load(file.path(workdir, "output", runname, "year.t.rda"))
load(file.path(workdir, "output", runname, "res.cqt.Lw.rda"))
load(file.path(workdir, "output", runname, "u5mr.iso.c.rda"))
load(file.path(workdir, "output", runname, "mcmc.meta.rda"))

res.cqt2 = res.cqt.Lw.2020$`0.5`
dimnames(res.cqt2)
res.cqt2.2 <- match.cqt.core(u5mr.iso.c1 = u5mr.iso.c, year.t1 = year.t, res.cqt2 = res.cqt2)

pooling_weight = 0.5
res.cqt2.1 <- obtain.matched.cqt(output.dir1 = output.dir,
                               output.dir2 = output.dir2,
                               pooling_weight = pooling_weight)
identical(dimnames(res.cqt2.1), dimnames(res.cqt2.2)) # TRUE
dimnames(res.cqt2.1)

# WPP and IHME
wpp.cqt <- match.cqt.core(u5mr.iso.c1 = u5mr.iso.c, year.t1 = year.t, res.cqt2 = u5mr.wpp.cqt.2019)
dimnames(wpp.cqt)
ihme.cqt <- match.cqt.core(u5mr.iso.c1 = u5mr.iso.c, year.t1 = year.t, res.cqt2 = u5mr.ihme.cqt.2019)
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

# one-country run

library(CME.plot)
runname.global <- "GR20210615" ##<< Global run to use. Do not change

workdir <- "C:/Users/lyhel/Dropbox/UN IGME Data/2021 Round Estimation/Code"
iso.select <- "SDN" ##<< 3-character ISO country code of country to run for
runname <-  paste0(runname.global, "_", iso.select) ##<< Run name
output.dir <- file.path(workdir, "output", runname)
output.dir2 <- file.path(workdir, "output", "IGME2020")
res.cqt.Lw.2020 <- get.cqt.from.results(output_dir = output.dir2)
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
                pdf.or.png = "pdf",
                col.CI = NULL, # suppress all the CI
                col.CI2 = NULL,
                col.CI_IHME = NULL)


