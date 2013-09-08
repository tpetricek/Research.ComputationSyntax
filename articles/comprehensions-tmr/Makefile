default: build

build:	
	latex comprefun.tex
	mv comprefun.aux comprefun.bib.aux
	bibtex comprefun.bib
	mv comprefun.bib.bbl comprefun.bbl
	latex comprefun.tex

%.tex: %.lhs force
	lhs2TeX --poly $< > $@

%.pdf: %.tex force
	pdflatex $<
	bibtex $(<:.tex=)
	pdflatex $<
	pdflatex $<

clean:
	rm -f *.log *.aux *.toc *.out *.blg *.bbl *~

bib : 

.PHONY : force

TMR.zip: Author.* Makefile tmr.*
	make clean
	zip TMR.zip Author.* Makefile tmr.*

export: TMR.zip
	scp TMR.zip byorgey@code.haskell.org:public_html/TMR/