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
frm <- merge(frm, frm[,.(period = 1:3), by = .(randid)], on = "randid", all = T)

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
frm[, `:=`(totchol = as.numeric(ifelse(totchol < 125 | totchol > 200, 1, 0)))]
names(frm)[grep("anychd", names(frm))] <- "chd"

# Make outcomes time-varying
frm[,`:=`(endtime = shift(time, -1)), by = .(randid)]
frm[is.na(endtime), endtime := 8766]
frm[timestrk > endtime, stroke := 0]
frm[timestrk <= endtime, stroke := 1]
frm[timeap > endtime | stroke == 1, cens := 0]
frm[timeap <= endtime & timeap < 8766, cens := 1]
frm[timechd > endtime, anychd := 0]
frm[timehyp > endtime, hyperten := 0]

# Impute where NA
frm[,`:=`(
	sex = zoo::na.locf(sex, na.rm = F),
	educ = zoo::na.locf(educ, na.rm = F),
	cigpday = zoo::na.locf(cigpday, na.rm = F),
	bmi = zoo::na.locf(bmi, na.rm = F),
	chd = zoo::na.locf(chd, na.rm = F),
	hyperten = zoo::na.locf(hyperten, na.rm = F),
	totchol = zoo::na.locf(totchol, na.rm = F),
	stroke = zoo::na.locf(stroke, na.rm = F)
)]
frm[,`:=`(
	sex = zoo::na.locf(sex, fromlast = T),
	educ = zoo::na.locf(educ, fromlast = T),
	cigpday = zoo::na.locf(cigpday, fromlast = T),
	bmi = zoo::na.locf(bmi, fromlast = T),
	chd = zoo::na.locf(chd, fromlast = T),
	hyperten = zoo::na.locf(hyperten, fromlast = T),
	totchol = zoo::na.locf(totchol, fromlast = T),
	stroke = {stroke[is.na(stroke)] <- 0
	stroke}
)]
frm[,`:=`(
	cens = {
		tmp <- 0
		tmp[!is.na(cens)] <- cens[!is.na(cens)]
		tmp <- zoo::na.locf(tmp, na.rm = F)
		is.na(tmp) <- 0
		tmp
	}
), by = .(randid)]
frm[,`:=`(
	time = {
		time[is.na(time) & period == 2] <- time[period == 1]  + 2200
		time[is.na(time) & period == 3] <- time[period == 2]  + 2200
		time
	},
	age = {
		age[is.na(age) & period == 2] <- floor(age[period == 1]  + 2200/365)
		age[is.na(age) & period == 3] <- floor(age[period == 2]  + 2200/365)
		age
	}
	), by = .(randid)]

frm <- frm[,.(randid, time, age, period, cigpday, bmi, chd, hyperten, cens, totchol, stroke, educ, sex)]

frm.wide <- dcast(
	frm, randid ~ period,
	value.var = c("time", "age", "cigpday", "bmi", "chd", "hyperten", "cens", "totchol", "stroke"))

frm.wide <- merge(frm.wide, frm[,.(educ = educ[1], sex = sex[1]), by = .(randid)], on = "randid")