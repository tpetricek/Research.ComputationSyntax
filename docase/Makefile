default: build

clean:
	rm -f *._tex
	rm -f *.aux
	rm -f *.bbl
	rm -f *.blg
	rm -f *.dvi
	rm -f *.pdf
	rm -f *.log
	rm -f *.ptb

build: clean	
	lhs2tex -o docase._tex docase.tex
	latex -interaction=nonstopmode docase._tex
	mv docase.aux docase.bib.aux
	bibtex docase.bib
	mv docase.bib.bbl docase.bbl
	pdflatex -interaction=nonstopmode docase._tex