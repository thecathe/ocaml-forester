# Project instructions

## Git integration
- The integration branch is `redirect-meta`, the fork's standing branch (pinned by
  the forest's flake). Keep the primary checkout on it; never merge into or rebase
  onto `main`.
- Integrate finished branches with a local `git merge --no-ff <branch>` into
  `redirect-meta`. Never squash.
- Do not push or open PRs unless explicitly asked.

## Build and test
- The opam switches under `~/.opam` don't run here (their binaries can't start).
  Build and test with the flake's dev shell instead:
  `nix develop -c dune build @lib/frontend/test/runtest`.
