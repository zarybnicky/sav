{
  outputs = { self, nixpkgs }: let
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    compiler = "ghc8102";
  in {
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = prev.lib.composeExtensions (prev.haskell.packageOverrides or (_: _: {})) (hself: hsuper: let
          makeLiquid = x: prev.haskell.lib.dontHaddock (prev.haskell.lib.addBuildTool x final.z3);
        in {
          liquid-demo = makeLiquid (hself.callCabal2nix "liquid-demo" ./. {});
          liquid-base = makeLiquid (prev.haskell.lib.unmarkBroken hsuper.liquid-base);
          liquid-ghc-prim = makeLiquid (prev.haskell.lib.unmarkBroken hsuper.liquid-ghc-prim);
          liquid-bytestring = makeLiquid (prev.haskell.lib.unmarkBroken hsuper.liquid-bytestring);
          liquid-containers = makeLiquid (prev.haskell.lib.unmarkBroken hsuper.liquid-containers);
          liquid-vector = makeLiquid (prev.haskell.lib.unmarkBroken hsuper.liquid-vector);
          liquidhaskell = prev.haskell.lib.unmarkBroken hsuper.liquidhaskell;
          stylish-haskell = prev.haskell.lib.doJailbreak hsuper.stylish-haskell;
          data-tree-print = prev.haskell.lib.doJailbreak hsuper.data-tree-print;
          hslogger = prev.haskell.lib.dontCheck hsuper.hslogger;
          ghc-exactprint = hself.callHackageDirect {
            pkg = "ghc-exactprint";
            ver = "0.6.3.3";
            sha256 = "KqCJ8VXqqMW9ihp8XXew8QgNN0DWRF1ysg9ejOd0m20=";
          } {};
          brittany = hself.callHackageDirect {
            pkg = "brittany";
            ver = "0.13.1.0";
            sha256 = "JYmfnoHy40AF+iW7tzJX31Evm8ty/40+/I4rARl4VZw=";
          } {};
        });
      };
    };
    devShell.x86_64-linux = pkgs.haskell.packages.ghc8102.shellFor {
      packages = p: [ p.liquid-demo ];
      buildInputs = [ pkgs.z3 pkgs.haskell.packages.ghc8102.haskell-language-server ];
      withHoogle = true;
    };
  };
}
