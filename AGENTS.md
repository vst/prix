# Repository Guidelines

## Project Structure & Module Organization
- Source: `src/` (primary modules under `Prix.*`, helpers under `Zamazingo.*`).
- Executable: `app/Main.hs` (CLI entrypoint `prix`).
- Tests: `test/spec/` (Spec runner) and `test/doctest/` (Doctest).
- Scripts: `scripts/gh-prix-project-item-list/` (GraphQL via `gh`, `jq`).
- Config: `config.yaml` in repo root or `~/.config/prix/config.yaml`.
- Nix/Dev tooling: `flake.nix`, `flake.lock`, `fourmolu.yaml`, `.hlint.yaml`, `.editorconfig`.

## Build, Test, and Development Commands
- Enter dev shell: `nix develop` (or `direnv allow` once, then auto‑enter).
- All‑in‑one: `cabal dev-test-build [-c]` (provided via flake; runs format, lint, tests, build, docs).
- Manual loop:
  - Generate cabal: `hpack`
  - Format: `fourmolu -i app/ src/ test/`; non‑HS: `prettier --write .`; Nix: `nixpkgs-fmt`.
  - Lint: `hlint app/ src/ test/`
  - Build/Run: `cabal build -O0`; `cabal run -O0 prix -- --help`
  - Test: `cabal v1-test`
  - Docs: `cabal haddock -O0`
- Static binary (Docker required): `./build-static.sh`.

## Coding Style & Naming Conventions
- Indentation: 2 spaces (`.editorconfig`, `fourmolu.yaml`).
- Formatter: Fourmolu is the source of truth; commit formatted code.
- Linting: Keep `hlint` clean; prefer aliases from `.hlint.yaml` (e.g., `Options.Applicative` as `OA`, `Data.Text` as `T`, `Zamazingo.Text` as `Z.Text`).
- Modules: `PascalCase` (`Prix.Config`, `Prix.Project`); functions/values `lowerCamelCase`.
- Extensions: Use only those commonly enabled in repo (e.g., `OverloadedStrings`, `RecordWildCards`).

## Testing Guidelines
- Frameworks: Doctest (`test/doctest`) and a spec runner (`test/spec`).
- Conventions: Prefer doctests for small, pure functions and enums; place spec files under `test/spec`.
- Run: `cabal v1-test` (or `cabal test` if preferred). No strict coverage requirement; add meaningful tests for new behavior.

## Commit & Pull Request Guidelines
- Commit style: Conventional Commits observed in history (e.g., `feat(cli): ...`, `chore(deps): ...`).
- PRs: clear description, rationale, and scope; link related issues; include CLI output examples if changing commands; update docs (README/AGENTS) when needed; ensure format/lint/tests pass.

## Security & Configuration Tips
- GitHub access: authenticate `gh` CLI before commands/scripts; avoid committing tokens.
- Config example location: `./config.yaml` or `~/.config/prix/config.yaml` with `inception` and `projects` entries.
- Data files: CLI writes under XDG data dir (e.g., `~/.local/share/prix/project-items.json`).
