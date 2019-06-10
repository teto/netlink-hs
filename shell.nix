{ 
nixpkgs ? import <nixpkgs> {},
# nixpkgs ? import https://raw.githubusercontent.com/Infinisil/all-hies/master/nixpkgs.nix {},
compiler ? nixpkgs.haskell.packages.ghc865
}:

with nixpkgs;

let
  # ce sont ces dependences
  my_nvim = genNeovim [ haskellPackages.netlink ] nvimConfig;


  nvimConfig = {
    withHaskell = true;
  };

  my_netlink = haskell.lib.addBuildDepends compiler.netlink [ compiler.fast-logger ];
in
# haskellPackages.shellFor {
compiler.shellFor {
  # the dependencies of packages listed in `packages`, not the
  packages = p: with p; [
    # (import ./. { inherit compiler;})
    my_netlink
  ];
  withHoogle = true;
  # haskellPackages.stack
  nativeBuildInputs = with compiler; [
    all-hies.versions.ghc865

    cabal-install
    # haskellPackages.bytestring-conversion
    # gutenhasktags
    # haskdogs # seems to build on hasktags/ recursively import things
    hasktags
    # haskellPackages.nvim-hs
    # haskellPackages.nvim-hs-ghcid

    # for https://hackage.haskell.org/package/bytestring-conversion-0.2/candidate/docs/Data-ByteString-Conversion-From.html
  ];

  # export HIE_HOOGLE_DATABASE=$NIX_GHC_DOCDIR as DOCDIR doesn't exist it won't work
  shellHook = ''
    # check if it's still needed ?
    export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
    echo ${my_nvim}

    echo "cabal configure --enable-library-profiling"
    echo "cabal build"
  '';
}

