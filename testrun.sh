#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"
[ -d .testrun ] || mkdir .testrun
cd .testrun
[ -h .emacs.d ] || ln -s .. .emacs.d

env HOME="$(pwd)" emacs

rm .emacs.d
cd ..
rm -rf .testrun
