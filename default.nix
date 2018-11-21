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
      rev = "bdcd9f3ed7db539163eb7b3d9bd0c27e543163d7";
      sha256 = "1pai4x3q82z2nkc7cqv8q15n2n71mam4g357ix1g0ma9zpq8mfjw";
    };
   
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "536b0a9243802347c299e077b5d85beb80d3a4a1";
      sha256 = "10wx0z5cd8dajr3rdskaq64v42ppa8dbb3rs3jyj872218xjz6nr";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      exitcode = import sources.exitcode { inherit nixpkgs compiler; };
      hedgehog       = self.callHackage "hedgehog" "0.6" {};
      tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
      polyparse = self.callHackage "polyparse" "1.12.1" {};
    };
  };

  e6b = modifiedHaskellPackages.callPackage ./e6b.nix {};

in

  e6b

