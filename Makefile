SCRIBBLE = raco scribble

MAIN = paper.scrbl

FILES = $(MAIN) \
        bib.rkt 

LATEX_FILES = tex/paper.tex \
              tex/acmart.cls \
              tex/debugging-1.png \
			  tex/debugging-2.png

pdf/paper.pdf: $(FILES)
	@mkdir -p pdf
	$(SCRIBBLE) --dest pdf --pdf $(MAIN)

$(LATEX_FILES): $(FILES)
	@mkdir -p tex
	$(SCRIBBLE) --dest tex --latex $(MAIN)

source.zip: $(LATEX_FILES)
	zip $@ $(LATEX_FILES)
