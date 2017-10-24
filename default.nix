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
   
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "97ef00aa45c70213a4f0ce348a2208e3f482a7e3";
      sha256 = "0qm0ay49wc0frxs6ipc10xyjj654b0wgk0b1hzm79qdlfp2yq0n5";
    };

    tasty-hedgehog = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "tasty-hedgehog";
      rev = "0.1.0.1";
      sha256 = "04pmr9q70gakd327sywpxr7qp8jnl3b0y2sqxxxcj6zj2q45q38m";
    };

    digit = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "digit";
      rev = "c970c918b3cb44fb17fdc7fce7096142e944431e";
      sha256 = "12hx79p2hrah8m344rm9a3ys0k253248vv7ykrgay7qndw1xl5wk";
    };

  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      exitcode = import sources.exitcode { inherit nixpkgs compiler; };
      tasty-hedgehog = import sources.tasty-hedgehog {};
      digit = import sources.digit {};
    };
  };

  e6b = modifiedHaskellPackages.callPackage ./e6b.nix {};

in

  e6b

