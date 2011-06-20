.SUFFIXES: .lhs .mkd .html .tex .pdf

PANDOC := pandoc --no-wrap -sS
HSCOLOUR := HsColour -lit

.lhs.mkd:
	cat $< | $(HSCOLOUR) -css > $@

.lhs.html:
	cat $< | $(PANDOC) -f markdown+lhs -t html+lhs --webtex -5 -c style.css > $@

.lhs.tex:
	cat $< | $(HSCOLOUR) -latex | $(PANDOC) -t latex> $@

tex.pdf:
	pdflatex $< && pdflatex $< && pdflatex $<
