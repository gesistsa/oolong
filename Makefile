all: vignettes overviewgh overviewghmd README

.FORCE:

vignettes: .FORCE
	cat vig_head.Rmd vig_body.Rmd > vignettes/overview.Rmd

overviewgh: vignettes
	cat gh_head.Rmd vig_body.Rmd > overview_gh.Rmd

overviewghmd: overviewgh
	Rscript -e "rmarkdown::render('overview_gh.Rmd')"

README: overviewghmd
	Rscript -e "devtools::document()"
	Rscript -e "rmarkdown::render('README.Rmd')"
