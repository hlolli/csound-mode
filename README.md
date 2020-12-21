[![License GPL 3][badge-license]][copying]
[![MELPA Stable](https://stable.melpa.org/packages/csound-mode-badge.svg)](https://stable.melpa.org/#/csound-mode)
[![Melpa Status](http://melpa.milkbox.net/packages/csound-mode-badge.svg)](http://melpa.milkbox.net/#/csound-mode)


# csound-mode
This package provides both a basic major mode for editing Csound files,
as well as a REPL for fast feedback when composing/sound-designing using Csound.

`csound-mode` provides a set of essential features for interactive development:
* REPL
* Interactive code evaluation
* Code completion
* ElDoc
* Indentation rules
* Syntax highlighting and rainbow delimited score parameters

## Installation

You can install `csound-mode` from `MELPA` using the following command:

<kbd>M-x package-install [RET] csound-mode [RET]</kbd>

Alternatively, [download latest release.](https://github.com/hlolli/csound-mode/releases/download/v9.2.0/csound-mode-0.2.0.zip)
and add it manually to load-path like shown here:

```el
;; Change directory path according to csound-mode dir location.
(add-to-list 'load-path "~/.emacs.d/csound-mode/")
(require 'csound-mode)
```

## Requirements

- Emacs 25+
- Csound 6.10+ (any release/compilation after 1. December 2017)

## Usage

`csound-mode` comes with major-mode-hooks, meaning that every time a csound file(.csd/.orc/.sco) is opened in emacs, `csound-mode` will be automatically loaded as major mode. While making it easier to install, this could potentially overwrite other major-mode you have set for csound files.

If you're using `csound-mode` directly from the git repo, and you happen to use the `use-package` macro. Then this could be used in your init.el file.

```Clojure
(use-package csound-mode
  :mode (("\\.csd\\'" . csound-mode)
  	 ("\\.orc\\'" . csound-mode)
  	 ("\\.sco\\'" . csound-mode)
  	 ("\\.udo\\'" . csound-mode))
  :load-path "packages/csound-mode/")
```

## Keybindings
<kbd>C-c C-p</kbd> `csound-play` Same as doing `csound filename -odac`

<kbd>C-c C-r</kbd> `csound-render` Same as doing `csound filename -o filename.wav`

<kbd>C-c C-z</kbd> `csound-repl-start`

<kbd>C-M-x</kbd>/<kbd>C-c C-c</kbd> `csound-evaluate-region`

<kbd>C-x C-e</kbd> `csound-evaluate-line`

<kbd>C-c C-l</kbd> `csound-repl-interaction-evaluate-last-expression`

<kbd>C-c C-s</kbd> `csound-score-align-block` cursor needs to be within a score block

<kbd>M-.</kbd> `csound-score-find-instr-def` cursor needs to be within a score block


## Run the tests

The tests depend on the package _test-simple.el_.
Run the tests locally from the command line

```
emacs --batch --no-site-file --no-splash --load test/csound-mode-tests.el
```



[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[COPYING]: http://www.gnu.org/copyleft/gpl.html
