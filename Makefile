.PHONY: drake deploy refs postgrad all

all: drake
refs: analysis/paper/references.bib

analysis/paper/references.bib: analysis/paper/paper.Rmd
	./getcitations

drake: refs analysis/drake.R
	./analysis/drake.R make --paper

deploy:
	./analysis/scripts/deploy-paper.sh

postgrad:
	./analysis/drake.R make --postgrad

submodel: ./analysis/paper/submodel-rtm.Rmd
	Rscript -e "rmarkdown::render('analysis/paper/submodel-rtm.Rmd')"
