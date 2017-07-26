# Changelog
## [master]
- Repl: +/. in p2 score snippets supported
- Repl: + in p3 score snippet supported
- Score: `csound-score--align-cols` improved to decrease col width as well
- Indentation: `csound-indentation-aggressive-score` customizeable variable added


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
- Csound'd eldoc function was missing from the major-mode function
