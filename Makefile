CASK_BIN=$(HOME)/.local/bin/cask
YASEL_BIN=$(HOME)/.local/bin/yasel
CASK=.cask/
LICENSES:=$(wildcard licenses/*)
SNIPPETS:=$(patsubst licenses/%,snippets/text-mode/%,$(LICENSES))

.PHONY: all update install

all: install

$(CASK_BIN):
	ln -s lisp/cask/bin/cask $@

$(YASEL_BIN): /usr/bin/npm
	npm install -g yasel

$(CASK): $(CASK_BIN)
	cask install

snippets/text-mode/%: $(YASEL_BIN)
	yasel licenses/ snippets/

install: $(CASK) $(SNIPPETS)

update: /usr/bin/git $(CASK)
	( 	git pull --rebase 					&& \
		cask update 						&& \
		yasel licenses/ snippets/ 				   \
	)

