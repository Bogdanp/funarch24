SCRIBBLE = raco scribble

MAIN = paper.scrbl

STYLE_OPTS = ++style overrides.tex

FILES = $(MAIN) \
        bib.rkt \
        acm-metadata.tex

LATEX_FILES = tex/paper.tex \
              tex/acmart.cls \
              tex/debugging-1.png \
              tex/debugging-2.png

pdf/paper.pdf: $(FILES)
	@mkdir -p pdf
	$(SCRIBBLE) --dest pdf --pdf $(MAIN)

$(LATEX_FILES): $(FILES)
	@mkdir -p tex
	$(SCRIBBLE) $(STYLE_OPTS) --dest tex --latex $(MAIN)
	printf '%s\n' '/copyrightyear.#1/d' wq | ed -s tex/paper.tex

source.zip: $(LATEX_FILES)
	zip $@ $(LATEX_FILES)
