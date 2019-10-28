.PHONY: drake deploy refs postgrad all html pdf paper

all: drake
refs: analysis/paper/references.bib

analysis/paper/references.bib: analysis/paper/paper.Rmd
	./getcitations

drake: analysis/drake.R
	./analysis/drake.R make

paper: html pdf
html: analysis/paper/paper.html
analysis/paper/paper.html: refs analysis/paper/paper.Rmd
	Rscript -e "rmarkdown::render('analysis/paper/paper.Rmd', 'html_document')"

pdf: analysis/paper/paper.pdf
analysis/paper/paper.pdf: refs analysis/paper/paper.Rmd
	Rscript -e "rmarkdown::render('analysis/paper/paper.Rmd', 'bookdown::pdf_document2')"

deploy:
	./analysis/scripts/deploy-paper.sh

postgrad:
	./analysis/drake.R make --postgrad

submodel: ./analysis/paper/submodel-rtm.Rmd
	Rscript -e "rmarkdown::render('analysis/paper/submodel-rtm.Rmd')"
