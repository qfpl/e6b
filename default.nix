{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    tasty-hspec = pkgs.fetchFromGitHub {
      owner = "mitchellwrosen";
      repo = "tasty-hspec";
      rev = "563d7c491d0fb5ad0c341ab0135d8787f6b34e50";
      sha256 = "0c8mzxsyymnx00bs5vr1hgya5v87as2k211vcmj17g2wigmjfqrb";
    };

    exitcode = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "exitcode";
      rev = "28f57c842c8864542fde0efae8788ce7c2523fea";
      sha256 = "1v7aski1vvxxskg20xlgvgfvxv2yjyvdl3aszvvw0g6ak2jwgwsf";
    };
   
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "5e9ebc1ffa2e40894ed884b637285022278f98e9";
      sha256 = "085bkmbqa34aks2hgfhxkl2vq8x1qrk5n4nzmvp35nqgcc53cksg";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      tasty-hspec = self.callCabal2nix "tasty-hspec" sources.tasty-hspec { };
      exitcode = import sources.exitcode { inherit nixpkgs compiler; };
      hedgehog       = self.callHackage "hedgehog" "0.6" {};
      tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
      concurrent-output = pkgs.haskell.lib.doJailbreak super.concurrent-output;
      polyparse = self.callHackage "polyparse" "1.12.1" {};
    };
  };

  e6b = modifiedHaskellPackages.callPackage ./e6b.nix {};

in

  e6b

