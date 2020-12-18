{
  outputs = { self, nixpkgs }: let
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
  in {
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = prev.lib.composeExtensions (prev.haskell.packageOverrides or (_: _: {})) (hself: hsuper: {
          liquid-demo = prev.haskell.lib.justStaticExecutables (
            hself.callCabal2nix "liquid-demo" ./. {}
          );
        });
      };
    };
    devShell.x86_64-linux = pkgs.haskellPackages.shellFor {
      packages = p: [ p.liquid-demo ];
      buildInputs = [ pkgs.haskell-language-server ];
      withHoogle = true;
    };
  };
}
