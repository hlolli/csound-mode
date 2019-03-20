{ pkgs ? import <nixpkgs> {}}:

let emacsWithPackages = (pkgs.emacsPackagesNgGen pkgs.emacs).emacsWithPackages;
    emacs = emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      buttercup
      with-simulated-input
      undercover
    ]));
    # testCoverage = pkgs.writeText

in pkgs.stdenv.mkDerivation rec {
  src = ../.; # "${pkgs.copyPathToStore ./.}";
  name = "csound-mode-test";
  outputs = [ "out" ];
  phases = [ "buildPhase" "installPhase" ];
  buildInputs = with pkgs; [
    emacs
    coreutils
  ];
  buildPhase = ''
    echo "Running csound-mode tests ..."
    cp -r ${src}/* ./
    ls
    cd test
    ${emacs}/bin/emacs -batch -f package-initialize \
      -f  buttercup-run-discover \
      | tee $TMP/test_results.txt

  '';
  installPhase = ''
   mkdir -p $out
   cp $TMP/test_results.txt $out
  '';
}
