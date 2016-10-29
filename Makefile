YASEL_BIN=$(HOME)/.local/bin/yasel
LICENSES:=$(wildcard licenses/*)
SNIPPETS:=$(patsubst licenses/%,snippets/text-mode/%,$(LICENSES))

.PHONY: all

all: $(SNIPPETS)

$(YASEL_BIN):
	npm install -g yasel

snippets/text-mode/%: $(YASEL_BIN)
	yasel -k "C-z s l" licenses/ snippets/
