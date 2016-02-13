CASK_BIN=$(HOME)/.local/bin/cask
YASEL_BIN=$(HOME)/.local/bin/yasel
CASK=.cask/
LICENSES:=$(wildcard licenses/*)
SNIPPETS:=$(patsubst licenses/%,snippets/text-mode/%,$(LICENSES))
PWD:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all update install

all: install

$(CASK_BIN):
	ln -s $(PWD)/lisp/cask/bin/cask $@

$(YASEL_BIN): /usr/bin/npm
	npm install -g yasel

$(CASK): $(CASK_BIN)
	cask install
	cask update

snippets/text-mode/%: $(YASEL_BIN)
	yasel licenses/ snippets/

install: $(CASK) $(SNIPPETS)

update: /usr/bin/git $(CASK)
	( 	git fetch 						&& \
		git rebase --autostash FETCH_HEAD 			&& \
		cask install						&& \
		cask update 						&& \
		yasel licenses/ snippets/ 				   \
	)

