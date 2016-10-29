# -*- mode: snippet -*-
# name: Import
# key: import
# --
const ${1:$(upcase-initials (replace-regexp-in-string "\\.*\\\." "" yas-text))} = imports.$1;