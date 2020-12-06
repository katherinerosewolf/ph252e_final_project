# Data preparation and exploration ##################################
# PH 252E: Group Project
# October 15, 2020

# Preliminaries (packages and such)
library(here); library(rlang)
source(here::here("scripts", "00-preliminaries.R"))
# Get data
source(here::here("scripts", "01-get-data.R"))
setDT(frm)

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

generate_data <- function(n = nrow(frm.wide),
													obs = frm.wide,
													intervention_A_1 = NULL,
													intervention_A_2 = NULL,
													intervention_A_3 = NULL,
													intervention_C = NULL) {
	# default_warnings <- getOption("warn")
	# options(warn = -1)

	if (length(intervention_A_1) == 1) {intervention_A_1 <- rep(intervention_A_1, n)}
	if (length(intervention_A_2) == 1) {intervention_A_2 <- rep(intervention_A_2, n)}
	if (length(intervention_A_3) == 1) {intervention_A_3 <- rep(intervention_A_3, n)}
	if (length(intervention_C) == 1) {intervention_C <- rep(intervention_C, n)}

	educ.levels <- c("Less than 12 years", "High school diploma, GED", "Some college, vocational school", "College or")
	obs$educ <- factor(obs$educ, labels = educ.levels)
	obs$sex <- factor(obs$sex, labels = c("M", "W"))

	age.cutpoints <- list(
		with(obs, c(-Inf, quantile(age_1, seq(0.2, 0.8, 0.2)), Inf)),
		with(obs, c(-Inf, quantile(age_2, seq(0.2, 0.8, 0.2)), Inf)),
		with(obs, c(-Inf, quantile(age_3, seq(0.2, 0.8, 0.2)), Inf))
	)

	obs$age.cat_1 <- with(obs, cut(age_1, age.cutpoints[[1]]))
	obs$age.cat_2 <- with(obs, cut(age_2, age.cutpoints[[2]]))
	obs$age.cat_3 <- with(obs, cut(age_3, age.cutpoints[[3]]))


	obs[,`:=`(
		stroke_2 = {
			stroke_2[stroke_1 == 1] <- 1
			stroke_2}
	), by = .(randid)]

	obs[,`:=`(
		stroke_3 = {
			stroke_3[stroke_2 == 1] <- 1
			stroke_3}
	), by = .(randid)]

	table(melt(obs[,.(stroke_1, stroke_2, stroke_3, randid)], id.vars = "randid")[,.(variable, value)])

	# merge(
	# 	melt(obs[,.(`1` = totchol_1, `2` = totchol_2, `3` = totchol_3, randid)], id.vars = "randid",
	# 			 variable.name = "period",
	# 			 value.name = "totchol"),
	# melt(obs[,.(`1` = stroke_1, `2` = stroke_2, `3` = stroke_3, randid)], id.vars = "randid",
	# 		 variable.name = "period",
	# 			 value.name = "stroke"),
	# 	on = "randid") -> foo
	#
	# foo[,`:=`(sum_chol = cumsum(totchol)), by = .(randid)]
	#
	# summary(glm(stroke ~ sum_chol, "binomial", foo[period == 3]))

	.FunEnv <- current_env()

	# Exogenous nodes ####
	for (k in 1:3) {
		sapply(paste("U", c("hyperten", "cens", "totchol", "stroke"),
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
			if (k > 1) {paste0("time_", k)},
			paste0("age.cat", "_", k),
			if (k > 1) {
				c(paste0("cigpday", "_", (k - 1):1),
					paste0(c("chd"), "_", (k - 1)),
					paste0(c("hyperten"), "_", (k - 1):1),
					paste0("totchol_", (k - 1):1))
			})
	})

	C.pred <- lapply(1:3, function(k) {
		c(paste0("age.cat", "_", k),
			if (k > 1) {paste0("time_", k)},
			"educ", "sex",
			paste0("cigpday_", k:1),
			paste0(c("chd"), "_", k),
			paste0(c("hyperten"), "_", k:1),
			if (k > 1) {paste0("totchol_", (k - 1):1)}
		)
	})

	A.pred <- lapply(1:3, function(k) {
		c(if (k > 1) {paste0("time_", k)},
			paste0("age.cat", "_", k),
			"educ", "sex",
			paste0(c("chd"), "_", k),
			paste0(c("hyperten"), "_", k:1),
			paste0("cigpday", "_", k:1),
			if (k > 1) {paste0("totchol_", (k - 1):1)})
	})

	Y.pred <- lapply(1:3, function(k) {
		c(if (k > 1) {paste0("time_", k)},
			paste0("age.cat", "_", k),
			"educ", "sex",
			paste0("cigpday", "_", k:1),
			paste0(c("chd"), "_", k),
			paste0(c("hyperten"), "_", k:1),
			paste0("totchol_", k:1))
	})

	# Fit models to find parameters for simulation
	for (k in 1:3) {
		sapply(c("hyperten"), function(x = 'hyperten') {
			assign(paste0("mod_", x, "_", k),
						 glm(as.formula(paste0(
						 	paste0(x, "_", k, " ~ "),
						 	paste(L.pred[[k]],
						 				collapse = " + "))),
						 	"binomial",
						 	if (k > 1) {
						 		obs[get(paste0("stroke_", k - 1)) == 0 &
						 					get(paste0("cens_", k - 1)) == 0]
						 	} else {obs}), envir = .FunEnv)
		})

		assign(paste0("mod_bmi_", k),
					 glm(as.formula(paste0(
					 	paste0("bmi_", k, " ~ "),
					 	paste(L.pred[[k]],
					 				collapse = " + "))),
					 	"gaussian",
					 	if (k > 1) {
					 		obs[get(paste0("stroke_", k - 1)) == 0 &
					 					get(paste0("cens_", k - 1)) == 0]
					 	} else {obs}), envir = .FunEnv)

		assign(paste0("mod_cigpday_", k),
					 glm(as.formula(paste0(
					 	paste0("cigpday_", k, " ~ "),
					 	paste(L.pred[[k]],
					 				collapse = " + "))),
					 	"gaussian",
					 	if (k > 1) {
					 		obs[get(paste0("stroke_", k - 1)) == 0 &
					 					get(paste0("cens_", k - 1)) == 0]
					 	} else {obs}), envir = .FunEnv)

		assign(paste0("mod_cens_", k),
					 glm(as.formula(paste0(
					 	paste0("cens_", k, " ~ "),
					 	paste(C.pred[[k]],
					 				collapse = " + "))),
					 	"binomial",
					 	if (k > 1) {
					 		obs[get(paste0("stroke_", k - 1)) == 0 &
					 					get(paste0("cens_", k - 1)) == 0]
					 	} else {obs}), envir = .FunEnv)

		assign(paste0("mod_totchol_", k),
					 glm(as.formula(paste0(
					 	paste0("totchol_", k, " ~ "),
					 	paste(A.pred[[k]],
					 				collapse = " + "))),
					 	"binomial",
					 	if (k > 1) {
					 		obs[get(paste0("stroke_", k - 1)) == 0 &
					 					get(paste0("cens_", k - 1)) == 0]
					 	} else {obs}), envir = .FunEnv)

		assign(paste0("mod_stroke_", k + 1),
					 glm(as.formula(paste0(
					 	paste0("stroke_", k, " ~ "),
					 	paste(Y.pred[[k]],
					 				collapse = " + "))),
					 	"binomial",
					 	if (k > 1) {
					 		obs[get(paste0("stroke_", k - 1)) == 0 &
					 					get(paste0("cens_", k - 1)) == 0]
					 	} else {obs}), envir = .FunEnv)

	}

	randid <- obs$randid

	# Time
	time_1 <- rep(0, n)
	time_2 <- rnorm(n, mean(obs$time_2), sd(obs$time_2))
	time_3 <- rnorm(n, mean(obs$time_3), sd(obs$time_3))

	# Age at enrollment
	age_1 <- runif(n, min(obs$age_1), max(obs$age_1))

	# Age
	age_2 <- age_1 + time_2/365
	age_3 <- age_2 + time_3/365

	age.cat_1 <- cut(age_1, age.cutpoints[[1]])
	age.cat_2 <- cut(age_2, age.cutpoints[[2]])
	age.cat_3 <- cut(age_3, age.cutpoints[[3]])

	# chd
	chd_1 <- rbinom(n, 1, mean(obs$chd_1))
	chd_3 <- chd_2 <- chd_1

	# The rest of the nodes
	for (k in 1:3) {
		# k <- k + 1
		invisible(sapply(c("hyperten"), function(x) {
			assign(paste0(x, "_", k),
						 as.numeric(
						 	get(paste0("U_", x, "_", k), envir = .FunEnv) <
						 		predict(
						 			get(paste0("mod_", x, "_", k), envir = .FunEnv),
						 			newdata = as.data.frame(sapply(L.pred[[k]], get, envir = .FunEnv, simplify = F)),
						 			type = "response")),
						 envir = .FunEnv
			)}
		))
		assign(paste0("bmi_", k),
					 get(paste0("U_bmi_", k), envir = .FunEnv) +
					 	predict(
					 		get(paste0("mod_bmi_", k), envir = .FunEnv),
					 		newdata = as.data.frame(sapply(L.pred[[k]], get, envir = .FunEnv, simplify = F)),
					 		type = "response"),
					 envir = .FunEnv
		)
		cigpday <- get(paste0("U_cigpday_", k), envir = .FunEnv)[[2]] +
			predict(get(paste0("mod_cigpday_", k), envir = .FunEnv),
							newdata = as.data.frame(sapply(L.pred[[k]], get, envir = .FunEnv, simplify = F)),
							type = "response")
		nocigs <- as.numeric(get(paste0("U_cigpday_", k), envir = .FunEnv)[[1]] <= 0.07)
		cigpday[nocigs] <- 0
		cigpday <- round(cigpday)
		assign(paste0("cigpday_", k),
					 cigpday,
					 envir = .FunEnv
		)
		cens <- as.numeric(
			get(paste0("U_cens_", k), envir = .FunEnv) <
				predict(
					get(paste0("mod_cens_", k), envir = .FunEnv),
					newdata = as.data.frame(sapply(C.pred[[k]], get, envir = .FunEnv, simplify = F)),
					type = "response"))
		if (k > 1) {
			cens[get(paste0("cens_", k - 1), envir = .FunEnv) == 1] <- 1
		}
		if (!is.null(intervention_C)) {
			cens <- intervention_C
		}
		assign(paste0("cens_", k),# 0,
					 cens,
					 envir = .FunEnv
		)
		if (is.null(get(paste0("intervention_A_", k))[1])) {
			assign(paste0("totchol_", k),
						 as.numeric(
						 	get(paste0("U_totchol_", k), envir = .FunEnv) <
						 		predict(
						 			get(paste0("mod_totchol_", k), envir = .FunEnv),
						 			newdata = as.data.frame(sapply(A.pred[[k]], get, envir = .FunEnv, simplify = F)),
						 			type = "response")),
						 envir = .FunEnv
			) } else {
				assign(paste0("totchol_", k),
							 get(paste0("intervention_A_", k)),
							 envir = .FunEnv
				)
			}
		stroke <- as.numeric(
			get(paste0("U_stroke_", k), envir = .FunEnv) <
				predict(
					get(paste0("mod_stroke_", k + 1), envir = .FunEnv),
					newdata = as.data.frame(sapply(Y.pred[[k]], get, envir = .FunEnv, simplify = F)),
					type = "response"))
		if (k > 1) {stroke[get(paste0("stroke_", k - 1), envir = .FunEnv) != 0] <- 1}
		assign(paste0("stroke_", k),
					 stroke,
					 envir = .FunEnv
		)
	}

	sex <- as.numeric(sex)
	educ <- as.numeric(educ)

	# options(warn = default_warnings)

	return(as.data.table(sapply(names(obs)[-1], get, envir = .FunEnv)))
}

set.seed(252)
frm.sim <- generate_data()

# # Post-intervention data
# Psi <- rbindlist(apply(
# 	expand.grid(0:1, 0:1, 0:1), 1, function(a = c(0, 0, 1)) {
#
# 		a <- unlist(a)
#
# 		sim <- generate_data(n = 5e5,
# 												 intervention_A_1 = a[1],
# 												 intervention_A_2 = a[2],
# 												 intervention_A_3 = a[3],
# 												 intervention_C = 0)
#
# 		# sim[stroke_1 == 1, `:=`(totchol_2 = 0, totchol_3 = 0)]
# 		# sim[stroke_2 == 1 & stroke_1 == 0, `:=`(totchol_3 = 0)]
#
# 		sim <- merge(
# 			melt(sim[,.(`1` = totchol_1, `2` = totchol_2, `3` = totchol_3, randid = 1:.N)],
# 			 id.vars = "randid", variable.name = "period", value.name = "totchol"),
# 			melt(sim[,.(`1` = stroke_1, `2` = stroke_2, `3` = stroke_3, randid = 1:.N)],
# 					 id.vars = "randid", variable.name = "period", value.name = "stroke"),
# 			on = "randid")[,.(period, sum_chol = cumsum(totchol), stroke), by = .(randid)]
#
# 		sim <- sim[,-"randid", with = F]
#
# 		return(sim)
# 	}))
# saveRDS(Psi, here::here("resources", "psi_simulation.rds"))
Psi <- readRDS(here::here("resources", "psi_simulation.rds"))

summary(glm(stroke ~ sum_chol, "quasibinomial",
						Psi[,.(stroke = mean(stroke)), by = .(sum_chol)]))

# # Simulated versus observed...
# cbind(frm[order(period, bmi),.(bmi, period = paste0("bmi_", period))],
# 			melt(frm.sim[,.(randid, bmi_1, bmi_2, bmi_3)],
# 					 id = 1,
# 					 variable.name = "period", variable.factor = F,
# 					 value.name = "simulated bmi")[order(period, `simulated bmi`),-"period"]) %>% ggplot(
# 					 	aes(x = bmi, y = `simulated bmi`)
# 					 ) +
# 	facet_wrap(. ~ period) +
# 	geom_function(fun = function(x) {x}, color = "red") +
# 	geom_point() +
# 	mytheme
#
# cbind(frm[order(period, cigpday),.(cigpday, period = paste0("cigpday_", period))],
# 			melt(frm.sim[,.(randid, cigpday_1, cigpday_2, cigpday_3)],
# 					 id = 1,
# 					 variable.name = "period", variable.factor = F,
# 					 value.name = "simulated cigpday")[order(period, `simulated cigpday`),-"period"]) %>% ggplot(
# 					 	aes(x = cigpday, y = `simulated cigpday`)
# 					 ) +
# 	facet_wrap(. ~ period) +
# 	geom_function(fun = function(x) {x}, color = "red") +
# 	geom_point() +
# 	mytheme
