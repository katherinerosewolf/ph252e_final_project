# Data preparation and exploration ##################################
# PH 252E: Group Project
# October 15, 2020

# Preliminaries (packages and such)
library(here)
source(here::here("scripts", "00-preliminaries.R"))
# Get data
source(here::here("scripts", "01-get-data.R"))
setDT(frm)
frm[,`:=`(
	totchol = totchol <= 200 & totchol >= 125)]

# Exploratory graphing
frm %>% ggplot(
	aes(x = bmi)
) + geom_histogram(aes(y = ..density..)) +
	geom_function(fun = function(x) {
		dgamma(x, 47, 1.85)
	}, color = "red") +
	facet_wrap(. ~ period) +
	mytheme

frm %>% ggplot(
	aes(x = cigpday)
) + geom_histogram(aes(y = ..density..)) +
	geom_function(fun = function(x) {
		dnorm(x, 20, 10)
	}, color = "red") +
	facet_wrap(. ~ period) +
	mytheme

n <- nrow(frm.wide)

# Exogenous nodes ####
for (k in 1:3) {
	sapply(paste("U", c("chd", "hyp", "cens", "A", "Y"),
			k, sep = "_"), function(x) {
				assign(x, runif(n))
			})

	assign(paste0("U_", "cigpday", "_", k), rnorm(n, 20, 5))
	assign(paste0("U_", "bmi", "_", k), rgamma(n, 40, 1.5))
}

# Endogenous nodes ####
# Education
educ.table <- table(frm[,.(educ = educ[1]), by = .(randid)]$educ)
educ <- cut(runif(n),
				 		c(0, cumsum(educ.table/sum(educ.table))),
				 		1:4,
				 		include.lowest = T)
sex <- ifelse(runif(n) > 0.55, 0, 1)

# Fit models to find parameters for simulation
for (k in 1:3) {
	assign(paste0("mod_", "chd", "_", k),
				 glm(as.formula(paste0(
				 	paste0("chd", "_", k, " ~ "),
				 	paste(c("factor(educ)", "sex",
				 					paste0("stroke_", k),
				 					if (k > 1) {
				 		paste0(c("chd", "hyperten", "cens"), "_", k - 1)
				 		}),
				 				collapse = " + "))),
				 	"binomial",
				 	frm.wide))
	assign(paste0("hyp", "_", k), rgamma(n, 40, 1.5))
	assign(paste0("cens", "_", k), rnorm(n, 20, 5))
	assign(paste0("A", "_", k), rgamma(n, 40, 1.5))
	assign(paste0("Y", "_", k), rnorm(n, 20, 5))
	assign(paste0("cigpday", "_", k), rnorm(n, 20, 5))
	assign(paste0("bmi", "_", k), rgamma(n, 40, 1.5))
}
