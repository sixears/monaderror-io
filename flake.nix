{
  description = "An AsIOError class for compound errors, and some MonadError handling utilities";

  inputs = {
    nixpkgs.url       = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url   = github:sixears/flake-build-utils/r1.0.0.13;

    base0.url         = github:sixears/base0/r0.0.4.9;
    has-callstack.url = github:sixears/has-callstack/r1.0.1.14;
    more-unicode.url  = github:sixears/more-unicode/r0.0.17.10;
  };

  outputs = { self, nixpkgs, build-utils
            , base0, has-callstack, more-unicode }:
    build-utils.lib.hOutputs self nixpkgs "monaderror-io" {
      deps = {
        inherit base0 has-callstack more-unicode;
      };
      ghc = p: p.ghc8107; # for tfmt

      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, deepseq, lens, mtl, text-printer
                    }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "monaderror-io";
            version = "1.2.5.13";
            src = ./.;
            libraryHaskellDepends = [
              base deepseq lens mtl text-printer
            ] ++ map (p: pkg p) [ base0 has-callstack more-unicode ];
            testHaskellDepends = [ base ];
            description = "An AsIOError class for compound errors, and some MonadError handling utilities";
            license = lib.licenses.mit;
          };
    };
}
