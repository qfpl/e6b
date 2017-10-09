{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    exitcode = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "exitcode";
      rev = "758639382d38a6afff6c5d2e4f37b600932f5f3a";
      sha256 = "01qbh7kcf6h0ilrfivknfm226x48xmn1gx348aqkgrc2x1l4z8g6";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      exitcode = import sources.exitcode { inherit nixpkgs compiler; };
    };
  };

  e6b = modifiedHaskellPackages.callPackage ./e6b.nix {};

in

  e6b

