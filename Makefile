YASEL_BIN=$(HOME)/.local/bin/yasel
LICENSES:=$(wildcard licenses/*)
SNIPPETS:=$(patsubst licenses/%,snippets/text-mode/%,$(LICENSES))
HOOKS_DIR:=$(shell git rev-parse --git-dir)/hooks
HOOKS:=$(wildcard .git-hooks/*)

.PHONY: all hooks

all: $(SNIPPETS) hooks

$(YASEL_BIN):
	npm install -g yasel

snippets/text-mode/%: $(YASEL_BIN)
	yasel -k "C-z s l" licenses/ snippets/

hooks: $(patsubst .git-hooks/%,$(HOOKS_DIR)/%,$(HOOKS))

$(HOOKS_DIR)/%: .git-hooks/%
	install -m 0755 -D $< $@
