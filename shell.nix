let
  pkgs = import <nixpkgs> {};
  py = pkgs.python3.withPackages (py_pkg: with py_pkg;
    [ numpy
      matplotlib
    ]);
in
pkgs.mkShell {
  packages = with pkgs; [
    gcc
    pkg-config
    py
  ];
  shellHook = ''
    export PYTHONHOME=${py}
  '';
}
