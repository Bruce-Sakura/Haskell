# Haskal — Haskell Assignments

A minimal workspace containing Haskell assignment code used for coursework and practice.

## Repository layout

Top-level files and folders:

- `test.hs` — small example that defines and evaluates a simple Expr AST.
- `As1/` — Assignment 1
	- `Assignment1.hs`
- `As2/` — Assignment 2
	- `Assignment2.hs`
	- `presubmit.sh` — convenience script to run checks/tests for As2
	- `T` — supplementary file or folder used by As2
	- `t.hs` — helper / test file
	- `Types.hs` — shared types used by As2
- `As3/` — Assignment 3
	- `Assignment3.hs`
	- `Types.hs`

> Note: File contents and names may change during development. Update this README when you add, remove, or rename files.

## Prerequisites

- GHC (Glasgow Haskell Compiler) and GHCi. Install via your package manager or from https://www.haskell.org/.

Check versions:

```bash
ghc --version
ghci --version
```

## Quick start

Open GHCi and load a file interactively:

```bash
ghci test.hs
# at the ghci prompt you can run
# ghci> eval (Add (Val 3) (Val 3))
# 6
```

Load an assignment file:

```bash
ghci As1/Assignment1.hs
# or
:l As1/Assignment1.hs
```

For Assignment 2, run the presubmit checks from the `As2` directory (make executable if needed):

```bash
cd As2
chmod +x presubmit.sh   # if not already executable
./presubmit.sh
```

## Working with Git and GitHub (common workflows)

Fetch remote changes and rebase your local work on top:

```bash
git fetch Haskell
git pull --rebase Haskell main
```

If you encounter conflicts during rebase, resolve them, then:

```bash
git add <resolved-files>
git rebase --continue
```

Push your changes to the remote named `Haskell`:

```bash
git push Haskell main
```

If your remote is named `origin`, replace `Haskell` with `origin`.

## Development notes

- Keep assignments in separate folders to avoid name collisions in GHCi.
- If you change shared `Types.hs`, update imports in dependent files.
- Use a backup branch before rebasing/force-pushing:

```bash
git branch backup-before-change
```

## Contributing

This appears to be a personal or course workspace. If you'd like collaborators, consider adding a `LICENSE` and `CONTRIBUTING.md`.

## Contact / Author

Repository: `Haskell` (owner: Bruce-Sakura)

If you want additional examples, tests, or CI integration, tell me what to include and I will update this README.