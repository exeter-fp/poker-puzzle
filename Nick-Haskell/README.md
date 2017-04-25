[![CI Status](http://img.shields.io/travis/exeter-fp/poker-puzzle.svg?style=flat)](https://travis-ci.org/exeter-fp/poker-puzzle)

# Haskell solution for Project Euler Poker puzzle

Built using [Haskell for Mac](http://haskellformac.com).

The key files are:

* [`Model.hs`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Haskell/PokerPuzzle.hsproj/Model.hs)
* [`Parsing.hs`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Haskell/PokerPuzzle.hsproj/Parsing.hs)
* [`PokerPuzzle.hs`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Haskell/PokerPuzzle.hsproj/PokerPuzzle.hs) - this is where the bulk of the code lives
* [`Main.hs`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Haskell/PokerPuzzle.hsproj/Main.hs)

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

```
Player 1 has won 376 times
```

load in `ghci`:

```bash
$ stack ghci --ghci-options -XOverloadedStrings
```

```
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
[1 of 3] Compiling Model            ( Model.hs, interpreted )
[2 of 3] Compiling Parsing          ( Parsing.hs, interpreted )
[3 of 3] Compiling PokerPuzzle      ( PokerPuzzle.hs, interpreted )
Ok, modules loaded: PokerPuzzle, Model, Parsing.
[4 of 4] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: PokerPuzzle, Model, Parsing, Main.

λ> main
Player 1 has won 376 times

λ> isPlayer1Winner $ parseLine "5H 5C 6S 7S KD 2C 3S 8S 8D TD"
False
λ> isPlayer1Winner $ parseLine "5D 8C 9S JS AC 2C 5C 7D 8S QH"
True
λ> isPlayer1Winner $ parseLine "2D 9C AS AH AC 3D 6D 7D TD QD"
False
λ> isPlayer1Winner $ parseLine "4D 6S 9H QH QC 3D 6D 7H QD QS"
True
λ> isPlayer1Winner $ parseLine "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"
True
```
