YASEL_BIN=$(HOME)/.local/bin/yasel
LICENSES:=$(wildcard licenses/*)
SNIPPETS:=$(patsubst licenses/%,snippets/text-mode/%,$(LICENSES))

.PHONY: all

$(YASEL_BIN): /usr/bin/npm
	npm install -g yasel

snippets/text-mode/%: $(YASEL_BIN)
	yasel licenses/ snippets/

all: $(SNIPPETS)
