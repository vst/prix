# Haskell Project Template

This is an opinionated template for creating Haskell projects. It uses
[Nix] [Flakes], [hpack] and [cabal].

> **TODO** Provide minimum viable documentation.

## Quickstart

Create your repository from this template, clone it on your computer
and enter its directory.

Then, run following to configure your project:

```sh
bash ./run-template.sh
```

It will prompt some questions and configure your project according to
your answers.

Once it is configured, provision `direnv`. You can copy the `.envrc.tmpl`:

```sh
cp .envrc.tmpl .envrc
```

Then, you can run the following command to allow `direnv` to activate the
development environment:

```sh
direnv allow
```

Alternatively, you can simply run the following command to activate the
development environment:

```sh
nix develop
```

And run the big, long build command as given in the next section.

Finally, you can remove the `run-template.sh` script:

```sh
rm run-template.sh
```

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
    cabal run -O0 haskell-template-hebele -- --version &&
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
