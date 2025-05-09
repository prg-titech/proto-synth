MAKEFLAGS += --silent

LATEX := platex
LFLAGS := -halt-on-error -synctex=1 -kanji=UTF8 -shell-escape
DVIPDF := dvipdfmx

TARGET := analysys

all: $(TARGET).pdf

$(TARGET).pdf: $(TARGET).tex
	$(LATEX) $(LFLAGS) $(TARGET)
	$(LATEX) $(LFLAGS) $(TARGET)
	$(DVIPDF) $(TARGET)

clean:
	rm -f *.pdf *.dvi *.aux *.log *.synctex.gz *.out

.PHONY: all clean
