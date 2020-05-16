# Changelog

## [master]
### Minor Changes
- `csound-play-flags` and `csound-render-flags` customizeable variables added

## [0.2.1]
### Minor Changes
- bytecode compilation errors fixed
- non-rainbow score syntax highlight improved
- minidocs and syopsis updated
- requiring highligh for missing hlt-highlight symbols
- new flash colors

## [0.2.0]
### Major Changes
- Csound API Removed in favour of UDP (requires Csound 6.10+)
- No more dependency on Emacs `modules`
- `csound-repl-plot-ftgen` removed

### Other Changes
- Repl: +/. in p2 score snippets supported
- Repl: + in p3 score snippet supported
- Score: `csound-score--align-cols` improved to decrease col width as well
- Indentation: `csound-indentation-aggressive-score` customizeable variable added
- Font-lock: Bug causing slower font-locking fixed
- Keybinding `C-c C-c` added for region evaluation
- Repl: Newline in the prompt supported via <Ctrl-Return>
- Repl: Completions added to the promt
- Csound-mode repl welcome string prints the audio configuration from the csound-mode buffer

## [0.1.2]
- Font-lock: Single line comments within score repaired
- Font-lock: p-field variables in light color mode are darker
- Font-lock: multiline comment in score implemented
- Font-lock: <CsLicense> xml tag added
- Syntax-table: multiline and single-line not conflicting
- Repl: csoundAPI Installation script now finds the makefile
- Repl: add instance specific environment variable options
- Score: all score events start immediately based on the lowest p2 value in block
- Eldoc: functional syntax with specific rate, colon seperated, implemented

## [0.1.1]

### Bug fixes
- Csound eldoc function was missing from the major-mode function
