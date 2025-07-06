# prix - Value-First Work Planning

> **TODO** Provide minimum viable documentation.

## Development

Big, long build command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 prix -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

To run checks, tests and build the codebase in the development environment, run:

```sh
cabal-dev-test-build
```

You can pass `-c` to clean the build artifacts first:

```sh
cabal-dev-test-build -c
```

As of Cabal 3.12, you can now run the above as an external `cabal` command:

```sh
cabal dev-test-build [-c]
```

<!-- REFERENCES -->

[Nix]: https://nixos.org
[Flakes]: https://wiki.nixos.org/wiki/Flakes
[hpack]: https://github.com/sol/hpack
[cabal]: https://www.haskell.org/cabal
