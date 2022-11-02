{
  description = "An AsIOError class for compound errors, and some MonadError handling utilities";

  inputs = {
    nixpkgs.url       = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url   = "github:sixears/flake-build-utils/r1.0.0.3";

    base0.url         = "github:sixears/base0/r0.0.4.1";
    has-callstack.url = "github:sixears/has-callstack/r1.0.1.4";
    more-unicode.url  = "github:sixears/more-unicode/r0.0.17.2";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils
            , base0, has-callstack, more-unicode }:
    build-utils.lib.hOutputs self nixpkgs "monaderror-io" {
      deps = {
        inherit base0 has-callstack more-unicode;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
