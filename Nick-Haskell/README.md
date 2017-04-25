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


