{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: 
      with nixpkgs.legacyPackages.${system};
      let
        name = "garden";
        project = devTools:
          let addBuildTools = (lib.trivial.flip haskell.lib.addBuildTools) devTools;
          in haskellPackages.developPackage {
            root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
            name = name;
            returnShellEnv = !(devTools == [ ]);
            modifier = (lib.trivial.flip lib.trivial.pipe) [
              addBuildTools
              haskell.lib.dontHaddock
              haskell.lib.enableStaticLibraries
              haskell.lib.justStaticExecutables
              haskell.lib.disableLibraryProfiling
              haskell.lib.disableExecutableProfiling
            ];
          };
      in {
        packages.pkg = project [ ];
        defaultPackage = self.packages.${system}.pkg;
        devShell = project (with haskellPackages; [
          cabal-fmt
          cabal-install
          haskell-language-server
          hlint
          fourmolu
        ]);
      });
}
