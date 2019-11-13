with import <nixpkgs> {};

let init-el = pkgs.writeText "init.el" ''
    (setq byte-compile-error-on-warn t)
    (setq byte-compile--use-old-handlers nil)
    (setq-default indent-tabs-mode nil)
    (add-to-list 'load-path "./")
    (package-initialize)
    '';
    emacs25WithPackages = (pkgs.emacsPackagesGen pkgs.emacs25).emacsWithPackages;
    emacs25 = emacs25WithPackages (epkgs: (with epkgs.melpaPackages; [
      shut-up
      test-simple
      multi
      dash
      highlight
    ]));
    emacs26WithPackages = (pkgs.emacsPackagesGen pkgs.emacs26).emacsWithPackages;
    emacs26 = emacs26WithPackages (epkgs: (with epkgs.melpaPackages; [
      shut-up
      test-simple
      multi
      dash
      highlight
    ]));
in stdenv.mkDerivation {
  name = "csound-mode-test";
  buildInputs = with pkgs; [
    emacs25
  ];
  shellHook = ''
    testEmacs25 () {
      ${emacs25}/bin/emacs -nw -q -batch -l ${init-el} \
        -l ert -l test/csound-mode-tests.el -f ert-run-tests-batch-and-exit
    }
    testEmacs26 () {
      ${emacs26}/bin/emacs -nw -q -batch -l ${init-el} \
        -l test-simple -l test/csound-mode-tests.el -f test-simple-run
    }
    export -f testEmacs25
    export -f testEmacs26
  '';
}
