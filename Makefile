

all: titanic_survival.html

titanic_survival.html: titanic_survival.Rmd
	R -e "rmarkdown::render('titanic_survival.Rmd', 'all')"
	open titanic_survival.html

.PHONY: clean
clean: 
	rm -rf *.html
