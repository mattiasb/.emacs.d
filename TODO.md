# TODO

## Emacs 27

### Bugs

- Find out why `RET` doesn't work.

### From Changelog

See https://www.masteringemacs.org/article/whats-new-in-emacs-27-1

- Move to `$XDG_CONFIG_HOME/emacs`.
- Look at LSP again, and specifically what libjansson brings.
- Look at the changes for packages (`early-init.el` etc).
- Look at the new `emacs.service` at `/usr/share/emacs/27.1/etc/emacs.service`.
  Specifically `Type=notify`.
- Move to `display-fill-column-indicator` over `fci-mode`.
- Look at `package-quickstart`.
- Find out what `package-activate-all` does.
- See if I can remove the `custom-save-all` advice since `print-quoted` defaults
  to `t` now.
