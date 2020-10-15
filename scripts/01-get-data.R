# Data preparation and exploration ##################################
# PH 252E: Group Project
# October 15, 2020

# Preliminaries (packages and such)
library(here)
source(here::here("scripts", "00-preliminaries.R"))

# Data
frm <- read.csv(here::here("data", "frmgham2.csv"))
frm <- as.data.table(frm)
names(frm) <- tolower(names(frm))

str(frm)
length(unique(frm$randid))

# Does education vary over time?
frm[,.(educ = n_distinct(educ)), by = .(randid)]$x %>% apply(2, table)
