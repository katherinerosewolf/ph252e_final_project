# Preliminaries, packages, helper functions, etc ###################
# October 25, 2020

library(tidyverse)
library(xtable)
library(pander)
library(tikzDevice)
library(knitr)
library(data.table)

# Yihui's softwrap
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
	# this hook is used only when the linewidth option is not NULL
	if (!is.null(n <- options$linewidth)) {
		x = knitr:::split_lines(x)
		# any lines wider than n should be wrapped
		if (any(nchar(x) > n)) x = strwrap(x, width = n)
		x = paste(x, collapse = '\n')
	}
	hook_output(x, options)
})

# Run LuaLaTex
lualatex <- function(
	pattern = ".*\\.tex",
	directory = here::here("reports"),
	magick = T,
	break_after = 30,
	png_density = 400) {

	dir.create(directory, F, T)
	if (magick) {dir.create(paste0(gsub("/$", "", directory), "/images"), F, T)}

	if (!grepl("\\.tex$", pattern)) {
		pattern <- paste0(pattern, "\\.tex")
	}

	if (grepl("Windows", Sys.info()['sysname'], ignore.case = T)) {
		# Windows
		library(stringr)
		directory <- gsub("/", "\\", directory)
		invisible(sapply(
			grep(pattern, list.files(directory), value = T), function(
				file.tex) {system((paste(
					paste0("cd \"", directory, "\"\n"),
					paste0("for %i in (", file.tex, ") do"),
					"lualatex \"$i\"; del \"%~ni.log\"; del \"%~ni.aux\";",
					if (magick) {
						paste0("magick -density ", png_density, " \"%~ni.pdf\" \".\\images\\%~ni.png\"")
						} else {NULL},
					sep = " ")), timeout = break_after)}
		))
	}
	if (grepl("Darwin", Sys.info()['sysname'], ignore.case = T)) {
		# OS X
		invisible(sapply(
			grep(pattern, list.files(directory), value = T), function(
				file.tex) {system((paste(
					paste0("cd \"", directory, "\";"),
					paste0("for i in \"", file.tex,"\"; do {"),
					"lualatex \"$i\"; rm \"${i%.tex}.aux\"; rm \"${i%.tex}.log\"; rm \"${i%.tex}.out\";",
					if (magick) {
						paste0("magick -density ", png_density, " \"${i%.tex}.pdf\" \"./images/${i%.tex}.png\";")
						} else {NULL},
					"} done", sep = "\n")), timeout = break_after)}
		))
	}
}

# My ggplot theme for pdf
mytheme <- theme_bw() +
	theme(
		axis.text = element_text(size = 6, color = "black"),
		axis.title = element_text(size = 8),
		plot.title = element_text(size = 8),
		plot.subtitle = element_text(size = 7),
		legend.title = element_text(size = 8),
		legend.text = element_text(size = 8),
		strip.text = element_text(size = 8, margin = margin(2.5, 2.5, 2.5, 2.5, "pt"))
		# legend.position="none"
	)

# My ggplot theme for html
mytheme.web <- theme_bw() +
	theme(
		axis.text = element_text(size = 12, color = 'black'),
		axis.title = element_text(size = 12),
		plot.title = element_text(size = 12),
		plot.subtitle = element_text(size = 10),
		legend.title = element_text(size = 12),
		legend.text = element_text(size = 12),
		strip.text = element_text(size = 12)
		# legend.position="none"
	)

# Save default TikZ options
tikzLualatexPackages.option <- getOption("tikzLualatexPackages")
# Set TikZ Options
options(
	tikzLatexPackages = c(
		"\\usepackage{tikz}",
		"\\usepackage[active,tightpage]{preview}",
		"\\PreviewEnvironment{pgfpicture}",
		"\\setlength\\PreviewBorder{0pt}",
		# "\\usepackage{bm}",
		"\\input{/Users/kevinchen/HeadRs/stathead.sty}",
		"\n"
	),
	tikzDefaultEngine = 'luatex',
	tikzLualatexPackages = c(
		# "\\usepackage[utf8]{inputenc}",
		"\\usepackage{amssymb}",
		"\\usepackage[no-math]{fontspec}\n",
		paste0(
			"\\setmainfont{Arial}",
			ifelse(Sys.info()["sysname"] == "Darwin" &
						 	Sys.info()["login"] == "kevinchen",
						 "\n",
						 "[Extension = .ttf,
			UprightFont = *,
			BoldFont = *bd,
			talicFont = *i,
			BoldItalicFont = *bi]\n")),
		"\\usepackage[italic]{mathastext}",
		"\\usepackage{tikz}\n",
		"\\usepackage[active,tightpage,psfixbb]{preview}\n",
		"\\PreviewEnvironment{pgfpicture}\n",
		"\\setlength\\PreviewBorder{0pt}\n",
		"\\input{/Users/kevinchen/HeadRs/stathead.sty}",
		"\n"
	)
)

# Set xtable and kable options
options(
	xtable.comment = F,
	xtable.include.rownames = F,
	xtable.booktabs = T,
	xtable.sanitize.text.function = function(x) {
		x
	},
	xtable.floating = F,
	# floatrow does not respect H
	xtable.table.placement = "H",
	xtable.caption.placement = 'top',
	knitr.kable.NA = "")

# Set pander table split options
panderOptions('table.split.table', 500)
