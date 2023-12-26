all: build

.FORCE:

build:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::install(quick = TRUE, upgrade = 'never')"
	Rscript -e "devtools::check()"
