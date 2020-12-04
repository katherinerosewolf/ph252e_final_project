frm <- fread(here::here("data", "frmgham2.csv"))
names(frm) <- tolower(names(frm))
frm[, `:=`(totchol = as.numeric(ifelse(totchol < 125 | totchol > 200, 1, 0)))]

# Summary table
df <- as.data.table(as.data.frame(frm))
setorder(df, randid, age)

# Individual-level summary
tab1.sum <- df[,.(
	'\\textbf{Sex}' = NA,
	'\\hspace{10pt}Male' = ifelse(sex[1] == 1, 1, 0),
	'\\hspace{10pt}Female' = ifelse(sex[1] == 2, 1, 0),
	"\\multicolumn{3}{l}{\\textbf{Observed individuals at each examination period}} \\\\ \n %" = NA,
	"\\hspace{10pt}Period 1" = sum(period == 1),
	"\\hspace{10pt}Period 2" = sum(period == 2),
	"\\hspace{10pt}Period 3" = sum(period == 3),
	"\\multicolumn{3}{l}{\\textbf{Educational attainment}} \\\\ \n %" = NA,
	"\\hspace{10pt}Less than high school" = educ[1] == 1,
	"\\hspace{10pt}High school diploma, GED" = educ[1] == 2,
	"\\hspace{10pt}Some college" = educ[1] == 3,
	"\\hspace{10pt}College degree" = educ[1] == 4,
	"\\textbf{Deceased by end of follow-up}" = max(death),
	"\\multicolumn{3}{l}{\\textbf{Duration of healthy cholesterol}} \\\\ \n %" = NA,
	"\\hspace{10pt}Always healthy" = sum(totchol) == 0,
	"\\hspace{10pt}Healthy for 2 periods" = sum(totchol) == 1,
	"\\hspace{10pt}Healthy for 1 period" = sum(totchol) == 2,
	"\\hspace{10pt}Never healthy" = sum(totchol) == 3,
	"\\textbf{Stroke by end of follow-up}" = max(stroke),

	# Years since follow-up
	"\\textbf{Years of follow-up}" = max(time)/365,
	'\\textbf{Age at first exam}' = age[period == 1],
	'\\textbf{Average cigarettes per day}' = mean(cigpday),
	'\\textbf{Average BMI}' = mean(bmi)
),
by = .(randid)][,-'randid']

# Population-level summary
table.break <- "Years of follow-"
tab1 <- rbind(
	t(apply(tab1.sum[,1:(grep(table.break, names(tab1.sum)) - 1)], 2, function(x) {
		return(
			c(mean(x, na.rm = T) * sum(!is.na(x)),
				mean(x, na.rm = T) * 100,
				NA
				# NA,
				# NA
			)
		)
	})),
	t(apply(tab1.sum[,grep(table.break, names(tab1.sum)):ncol(tab1.sum)], 2,
					function(x) {
						return(
							c(
								# mean(x, na.rm = T),
								# sd(x, na.rm = T)
								# min(x, na.rm = T),
								median(as.numeric(x), na.rm = T),
								quantile(as.numeric(x), 0.25, na.rm = T),
								quantile(as.numeric(x), 0.75, na.rm = T)
								# max(x, na.rm = T)
							)
						)
					}))
)

tab1[!is.finite(as.matrix(tab1))] <- NA

# Table names
colnames(tab1) <- c('n', '%', "temp")#, 'Minimum', 'Median', 'Maximum')

# Digits
tab1.digits <- matrix(2, ncol = 3,#5
											nrow = nrow(tab1))
tab1.digits[grep("Age", rownames(tab1), ignore.case = T), ] <- 0
tab1.digits[grep("year ", rownames(tab1), ignore.case = T), ] <- 0
tab1.digits[1:(grep(table.break, rownames(tab1), ignore.case = T) - 1), -1] <- 0

tab1 <- matrix(
	sapply(1:length(tab1), function(i) {
		as.character(round(as.vector(tab1)[i], as.vector(tab1.digits)[i]))
	}),
	ncol = ncol(tab1),
	nrow = nrow(tab1),
	dimnames = dimnames(tab1)
)

# Pretty counts
tab1[1:grep("Years of", rownames(tab1)), 1] <- sapply(
	tab1[1:grep("Years of", rownames(tab1)), 1],
	function (i) {
		if (!is.na(i)) {
			prettyNum(as.numeric(i), big.mark = '\\\\,')
		} else {NA}
	})

# Pretty percents
tab1[1:(grep("Years of fo", rownames(tab1)) - 1), 2] <- sapply(
	tab1[1:(grep("Years of fo", rownames(tab1)) - 1), 2],
	function (i) {
		if (!is.na(i)) {
			paste0(as.numeric(i), '\\%')
		} else {NA}
	})

# Quartiles one column
tab1[!is.na(tab1[,3]),2] <- paste0(
	tab1[!is.na(tab1[,3]),2],
	",\\,",
	tab1[!is.na(tab1[,3]),3]
)

tab1 <- tab1[,-3]

# Second column gets parentheses
tab1[!is.na(tab1[,1]), 2] <- paste0(
	"(", tab1[!is.na(tab1[,1]), 2], ")"
)

tab1 <- as.data.frame(tab1)

colnames(tab1) <- c("$n$", "(\\%)")

tab1 %>% xtable(
	align = "p{0.4\\linewidth}ll") %>% print(
	include.rownames = T,
	hline.after =  c(-1, 0, grep("Years of follow-up", rownames(.)) - 1, nrow(.)),
	add.to.row = list(
		pos = list(grep("Years of follow-up", rownames(.)) - 1
							 # nrow(.)
							 ),
		command = c(
			"\\midrule\n & Mean & (Q1, Q3) \\\\"
			# "\\bottomrule \n \\multicolumn{3}{l}{Statistics presented are mean (Q1, Q3), unless otherwise indicated.} \\\\\n")
	))) %>% clipr::write_clip()
