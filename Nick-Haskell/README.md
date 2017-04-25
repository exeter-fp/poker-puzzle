[![CI Status](http://img.shields.io/travis/exeter-fp/poker-puzzle.svg?style=flat)](https://travis-ci.org/exeter-fp/poker-puzzle)

# Haskell solution for Project Euler Poker puzzle

Built using [Haskell for Mac](http://haskellformac.com).

The key files are:

* `Model.hs`
* `Parsing.hs`
* `PokerPuzzle.hs` - this is where the bulk of the code lives
* `Main.hs`

All other files are [Haskell for Mac](http://haskellformac.com) boiler-plate.

## Command-line building

From [Deploying Haskell for Mac projects on OS X and Linux](http://blog.haskellformac.com/blog/deploying-haskell-for-mac-projects-on-os-x-and-linux):

1. Download a [stack binary](https://www.haskellstack.org)
2. Execute the following shell commands:

```bash
$ cd PokerPuzzle.hsproj
$ stack setup --no-system-ghc
$ stack build
```

now run it

```bash
$ stack exec PokerPuzzle
```

```bash
Player 1 has won 376 times
```

load in `ghci`:

```bash
$ stack ghci
```

```bash
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
[1 of 3] Compiling Model            ( Model.hs, interpreted )
[2 of 3] Compiling Parsing          ( Parsing.hs, interpreted )
[3 of 3] Compiling PokerPuzzle      ( PokerPuzzle.hs, interpreted )
Ok, modules loaded: PokerPuzzle, Model, Parsing.
[4 of 4] Compiling Main             ( /Users/nickager/Documents/programming/poker-puzzle/Nick-Haskell/PokerPuzzle.hsproj/Main.hs, interpreted )
Ok, modules loaded: PokerPuzzle, Model, Parsing, Main.
λ> main
Player 1 has won 376 times
376
λ> 
```
