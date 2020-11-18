# Data preparation and exploration ##################################
# PH 252E: Group Project
# October 15, 2020

# Preliminaries (packages and such)
library(here)
source(here::here("scripts", "00-preliminaries.R"))

# Data ####
frm <- read.csv(here::here("data", "frmgham2.csv"))
frm <- as.data.table(frm)
names(frm) <- tolower(names(frm))

str(frm)
length(unique(frm$randid))

# Does education vary over time?
lapply(c("educ", "cigpday", "bmi", "sex"), function(x) {
	frm[,.(x = n_distinct(get(x))), by = .(randid)]$x %>% table
})

# Remove prevalent cases
prevalent.who <- unique(frm[prevstrk == 1 & period == 1, randid])
frm <- frm[!randid %in% prevalent.who, ]

# Treatments
frm[, `:=`(totchol = ifelse(totchol < 125 | totchol > 200, 1, 0))]
names(frm)[grep("anychd", names(frm))] <- "chd"

# Make outcomes time-varying
frm[,`:=`(endtime = shift(time, -1)), by = .(randid)]
frm[is.na(endtime), endtime := 8766]
frm[timestrk > endtime, stroke := 0]
frm[timestrk > endtime, stroke := 1]
frm[timeap > endtime | stroke == 1, cens := 0]
frm[timeap <= endtime, cens := 1]
frm[timechd > endtime, anychd := 0]
frm[timehyp > endtime, hyperten := 0]

frm.wide <- dcast(
	frm, randid ~ period,
	value.var = c("cigpday", "bmi", "anychd", "hyperten", "cens", "totchol", "stroke"))

frm.wide <- merge(frm.wide, frm[,.(educ = educ[1], sex = sex[1]), by = .(randid)], on = "randid")
