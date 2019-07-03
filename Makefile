.PHONY: drake publish refs all

all: drake
refs: analysis/paper/references.bib

analysis/paper/references.bib: analysis/paper/paper.Rmd
	./getcitations

drake: refs analysis/drake.R
	./analysis/drake.R make

deploy:
	./analysis/scripts/deploy-paper.sh
