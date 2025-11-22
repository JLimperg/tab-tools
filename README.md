# tab-tools

The `tabt` command line program provides some additional functionality for the [Tabbycat](https://tabbycat.readthedocs.io/en/stable/) and [open\_tab](https://github.com/julmaxi/open_tab_v2) tab programs.

## Building

- Install the [`qrencode`](https://github.com/fukuchi/libqrencode) library.
  On Linux or MacOS, you should be able to use your OS package manager.
- Install [`ghcup`](https://www.haskell.org/ghcup/).
- Install `cabal`: `ghcup install cabal`
- `cabal update`
- `cabal build`  
- If you don't have the right GHC version installed, the previous command fails.
  In that case, install the version of GHC that it reports as missing:  
  `ghcup install ghc <version>`  
  Then `cabal build` again.

If `cabal` reports that it can't find the `qrencode` library, you need to
create a `cabal.project.local` file in this directory and add the location of
the library. E.g. on MacOS with `qrencode` installed via Homebrew:

```
package haskell-qrencode
  extra-lib-dirs: /opt/homebrew/lib
  extra-include-dirs: /opt/homebrew/include
```

## Running

`cabal run -- tabt --help`

This prints a help message with further instructions.

## Installing

`cabal install`

This builds the `tabt` program (if necessary) and installs it at `~/.local/bin`.
Make sure this location is on your PATH.
