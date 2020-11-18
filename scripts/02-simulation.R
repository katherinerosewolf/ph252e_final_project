# Data preparation and exploration ##################################
# PH 252E: Group Project
# October 15, 2020

# Preliminaries (packages and such)
library(here); library(rlang)
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

generate_data <- function(n = nrow(frm.wide), obs = frm.wide) {
	educ.levels <- c("Less than 12 years", "High school diploma, GED", "Some college, vocational school", "College or")
	obs$educ <- factor(obs$educ, labels = educ.levels)
	obs$sex <- factor(obs$sex, labels = c("M", "W"))

	.FunEnv <- current_env()

	# Exogenous nodes ####
	for (k in 1:3) {
		sapply(paste("U", c("chd", "hyperten", "cens", "totchol", "stroke"),
								 k, sep = "_"), function(x) {
								 	assign(x, runif(n), envir = .FunEnv)
								 })

		assign(paste0("U_", "cigpday", "_", k), list(runif(n), rnorm(n, 20, 5) - 20))
		assign(paste0("U_", "bmi", "_", k), rgamma(n, 40, 1.5) - 40 / 1.5)
	}

	# Endogenous nodes ####
	# Education
	educ.table <- table(frm[,.(educ = educ[1]), by = .(randid)]$educ)
	educ <- cut(runif(n),
							c(0, cumsum(educ.table/sum(educ.table))),
							educ.levels,
							include.lowest = T)
	sex <- factor(ifelse(runif(n) > 0.55, "M", "W"))

	# List of predictors at each time point
	L.pred <- lapply(1:3, function(k) {
		c("educ", "sex",
			if (k > 1) {
				c(paste0("stroke_", k - 1),
					paste0("time", "_", k - 1),
					paste0("age", "_", k - 1),
					paste0(c("chd", "hyperten", "cens"), "_", k - 1),
					paste0("totchol_", (k -1):1))
			})
	})

	C.pred <- lapply(1:3, function(k) {
		c(paste0("time", "_", k),
			paste0("age", "_", k),
			"educ", "sex",
			if (k > 1) {
				c(paste0("stroke_", k - 1),
					paste0(c("chd", "hyperten"), "_", k),
					paste0("cens_", (k -1)),
					paste0("totchol_", (k -1):1))
			})
	})

	A.pred <- lapply(1:3, function(k) {
		c(paste0("time", "_", k),
			paste0("age", "_", k),
			"educ", "sex",
			if (k > 1) {
				c(paste0("stroke_", k - 1),
					paste0(c("chd", "hyperten", "cens"), "_", k),
					paste0("totchol_", (k -1):1))
			})
	})

	Y.pred <- lapply(1:3, function(k) {
		c(paste0("time", "_", k),
			paste0("age", "_", k),
			"educ", "sex",
			if (k > 1) {
				c(paste0("stroke_", k - 1),
					paste0(c("chd", "hyperten", "cens"), "_", k),
					paste0("totchol_", k:1))
			})
	})

	# Fit models to find parameters for simulation
	for (k in 1:3) {
		sapply(c("chd", "hyperten"), function(x = 'chd') {
			assign(paste0("mod_", x, "_", k),
						 glm(as.formula(paste0(
						 	paste0(x, "_", k, " ~ "),
						 	paste(L.pred[[k]],
						 				collapse = " + "))),
						 	"binomial",
						 	obs), envir = .FunEnv)
		})

		assign(paste0("mod_bmi_", k),
					 glm(as.formula(paste0(
					 	paste0("bmi_", k, " ~ "),
					 	paste(L.pred[[k]],
					 				collapse = " + "))),
					 	"gaussian",
					 	obs), envir = .FunEnv)

		assign(paste0("mod_cigpday_", k),
					 glm(as.formula(paste0(
					 	paste0("cigpday_", k, " ~ "),
					 	paste(L.pred[[k]],
					 				collapse = " + "))),
					 	"gaussian",
					 	obs[cigpday_1 > 0]), envir = .FunEnv)

		assign(paste0("mod_cens_", k),
					 glm(as.formula(paste0(
					 	paste0("cens_", k, " ~ "),
					 	paste(C.pred[[k]],
					 				collapse = " + "))),
					 	"binomial",
					 	obs), envir = .FunEnv)

		assign(paste0("mod_totchol_", k),
					 glm(as.formula(paste0(
					 	paste0("totchol_", k, " ~ "),
					 	paste(A.pred[[k]],
					 				collapse = " + "))),
					 	"binomial",
					 	obs), envir = .FunEnv)

		assign(paste0("mod_stroke_", k + 1),
					 glm(as.formula(paste0(
					 	paste0("stroke_", k, " ~ "),
					 	paste(Y.pred[[k]],
					 				collapse = " + "))),
					 	"binomial",
					 	obs), envir = .FunEnv)

	}

	randid <- obs$randid

	# Time
	time_1 <- obs$time_1
	time_2 <- obs$time_2
	time_3 <- obs$time_3

	# Age
	age_1 <- obs$age_1
	age_2 <- obs$age_2
	age_3 <- obs$age_3

	# The rest of the nodes
	for (k in 1:3) {
		# k <- 1
		sapply(c("chd", "hyperten"), function(x) {
			assign(paste0(x, "_", k),
						 as.numeric(
						 	get(paste0("U_", x, "_", k), envir = .FunEnv) <
						 		predict(
						 			get(paste0("mod_", x, "_", k), envir = .FunEnv),
						 			newdata = as.data.frame(sapply(L.pred[[k]], get, envir = .FunEnv, simplify = F)))),
						 envir = .FunEnv
			)}
		)
		assign(paste0("bmi_", k),
					 get(paste0("U_bmi_", k), envir = .FunEnv) +
					 	predict(
					 		get(paste0("mod_bmi_", k), envir = .FunEnv),
					 		newdata = as.data.frame(sapply(L.pred[[k]], get, envir = .FunEnv, simplify = F))),
					 envir = .FunEnv
		)
		cigpday <- get(paste0("U_cigpday_", k), envir = .FunEnv)[[2]] +
					 	predict(get(paste0("mod_cigpday_", k), envir = .FunEnv),
					 		newdata = as.data.frame(sapply(L.pred[[k]], get, envir = .FunEnv, simplify = F)))
		nocigs <- as.numeric(get(paste0("U_cigpday_", k), envir = .FunEnv)[[1]] <= 0.07)
		cigpday[nocigs] <- 0
		cigpday <- round(cigpday)
		assign(paste0("cigpday_", k),
					 cigpday,
					 envir = .FunEnv
		)
		assign(paste0("cens_", k),
					 as.numeric(
					 	get(paste0("U_cens_", k), envir = .FunEnv) <
					 		predict(
					 			get(paste0("mod_cens_", k), envir = .FunEnv),
					 			newdata = as.data.frame(sapply(C.pred[[k]], get, envir = .FunEnv, simplify = F)))),
					 envir = .FunEnv
		)
		assign(paste0("totchol_", k),
					 as.numeric(
					 	get(paste0("U_totchol_", k), envir = .FunEnv) <
					 		predict(
					 			get(paste0("mod_totchol_", k), envir = .FunEnv),
					 			newdata = as.data.frame(sapply(A.pred[[k]], get, envir = .FunEnv, simplify = F)))),
					 envir = .FunEnv
		)
		assign(paste0("stroke_", k),
					 as.numeric(
					 	get(paste0("U_stroke_", k), envir = .FunEnv) <
					 		predict(
					 			get(paste0("mod_stroke_", k + 1), envir = .FunEnv),
					 			newdata = as.data.frame(sapply(Y.pred[[k]], get, envir = .FunEnv, simplify = F)))),
					 envir = .FunEnv
		)
	}

	return(as.data.table(sapply(names(obs), get, envir = .FunEnv)))
}

frm.sim <- generate_data()

# Simulated versus observed...
cbind(frm[order(period, bmi),.(bmi, period = paste0("bmi_", period))],
			melt(frm.sim[,.(randid, bmi_1, bmi_2, bmi_3)],
					 id = 1,
					 variable.name = "period", variable.factor = F,
					 value.name = "simulated bmi")[order(period, `simulated bmi`),-"period"]) %>% ggplot(
					 	aes(x = bmi, y = `simulated bmi`)
					 ) +
	facet_wrap(. ~ period) +
	geom_function(fun = function(x) {x}, color = "red") +
	geom_point() +
	mytheme

cbind(frm[order(period, cigpday),.(cigpday, period = paste0("cigpday_", period))],
			melt(frm.sim[,.(randid, cigpday_1, cigpday_2, cigpday_3)],
					 id = 1,
					 variable.name = "period", variable.factor = F,
					 value.name = "simulated cigpday")[order(period, `simulated cigpday`),-"period"]) %>% ggplot(
					 	aes(x = cigpday, y = `simulated cigpday`)
					 ) +
	facet_wrap(. ~ period) +
	geom_function(fun = function(x) {x}, color = "red") +
	geom_point() +
	mytheme
