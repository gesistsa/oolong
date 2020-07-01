all: vignettes overviewgh overviewghmd README

.FORCE:

vignettes: .FORCE
	cat vig_head.Rmd | sed 's/{title}/Overview/g' > vig_temp.Rmd
	cat vig_temp.Rmd vig_body.Rmd > vignettes/overview.Rmd
	rm vig_temp.Rmd

overviewgh: vignettes
	cat gh_head.Rmd | sed 's/{title}/Overview/g' > gh_temp.Rmd
	cat gh_temp.Rmd vig_body.Rmd > overview_gh.Rmd
	rm gh_temp.Rmd

overviewghmd: overviewgh
	Rscript -e "rmarkdown::render('overview_gh.Rmd')"

btm: .FORCE
	cat vig_head.Rmd | sed 's/{title}/BTM/g' > vig_temp.Rmd
	cat vig_temp.Rmd btm.Rmd > vignettes/btm.Rmd
	rm vig_temp.Rmd
	cat gh_head.Rmd | sed 's/{title}/BTM/g' > gh_temp.Rmd
	cat gh_temp.Rmd btm.Rmd > btm_gh.Rmd
	rm gh_temp.Rmd
	Rscript -e "rmarkdown::render('btm_gh.Rmd')"

README: overviewghmd
	Rscript -e "devtools::document()"
	Rscript -e "devtools::install(quick = TRUE, upgrade = 'never')"
	Rscript -e "rmarkdown::render('README.Rmd')"
	Rscript -e "devtools::check()"
