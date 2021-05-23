all: vignettes overviewgh ghdocs build

.FORCE:

vignettes: .FORCE
	cat vig_head.Rmd | sed 's/{title}/Overview/g' > vig_temp.Rmd
	cat vig_temp.Rmd vig_body.Rmd > vignettes/overview.Rmd
	rm vig_temp.Rmd
	cat vig_head.Rmd | sed 's/{title}/BTM/g' > vig_temp.Rmd
	cat vig_temp.Rmd btm.Rmd > vignettes/btm.Rmd
	rm vig_temp.Rmd
	cat vig_head.Rmd | sed 's/{title}/Deploy/g' > vig_temp.Rmd
	cat vig_temp.Rmd deploy.Rmd > vignettes/deploy.Rmd
	rm vig_temp.Rmd

overviewgh: vignettes
	cat gh_head.Rmd | sed 's/{title}/Overview/g' > gh_temp.Rmd
	cat gh_temp.Rmd vig_body.Rmd > overview_gh.Rmd
	rm gh_temp.Rmd
	cat gh_head.Rmd | sed 's/{title}/BTM/g' > gh_temp.Rmd
	cat gh_temp.Rmd btm.Rmd > btm_gh.Rmd
	rm gh_temp.Rmd
	cat gh_head.Rmd | sed 's/{title}/Deploy/g' > gh_temp.Rmd
	cat gh_temp.Rmd deploy.Rmd > gh_temp2.Rmd
	cat gh_temp2.Rmd | sed 's;figures;vignettes/figures;g' > deploy_gh.Rmd
	rm gh_temp.Rmd gh_temp2.Rmd

build: vignettes
	Rscript -e "devtools::document()"
	Rscript -e "devtools::install(quick = TRUE, upgrade = 'never')"
	Rscript -e "devtools::check()"

ghdocs: overviewgh
	Rscript -e "rmarkdown::render('README.Rmd')"
	Rscript -e "rmarkdown::render('overview_gh.Rmd')"
	Rscript -e "rmarkdown::render('btm_gh.Rmd')"
	Rscript -e "rmarkdown::render('deploy_gh.Rmd')"
