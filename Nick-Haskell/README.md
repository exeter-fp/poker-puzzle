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
## Alternative Haskell implementations
* https://wiki.haskell.org/Euler_problems/51_to_60#Problem_54
* https://codereview.stackexchange.com/questions/110867/project-euler-problem-54-in-haskell

# Parallelising the solution 

To make the solution run in parallel I've changed [`Main.hs`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Haskell/PokerPuzzle.hsproj/Main.hs) from:

```haskell
   let numPlayer1Wins = length $ filter isPlayer1Winner hands
```

to:

```haskell
  let player1Wins = map (boolToInt. isPlayer1Winner) hands `using` parList rseq
  let numPlayer1Wins = sum player1Wins
```

### Build and test the parallel version

Build using the threaded runtime and turn on optimisations:

```bash
# stack clean
$ stack build --ghc-options -threaded --ghc-options -O2
```

Run using three OS threads:

```bash
$ stack exec PokerPuzzle -- +RTS -N3
```
or just use `-N` to use all available OS threads

Output execution diagnostics:

```bash
$ stack exec PokerPuzzle -- +RTS -N -s
```

To build for threadscope:

```bash
$ stack clean
$ stack build --ghc-options -threaded --ghc-options -O2 --ghc-options -rtsopts --ghc-options -eventlog
```

Running threadscope. First run `PokerPuzzle` with the `-l` option to generate an eventlog which can be used be threadscope:

```bash
$ stack exec PokerPuzzle -- +RTS -N -l
```

The run threadscope:

```bash
$ threadscope PokerPuzzle.eventlog 
```

![](threadscope.png)

### Performance comparision of the parallel vs serial solution

Disappointingly I saw little difference in the performance of the parallel version verses the concurrent version:

| Serial  | Parallel (-N8) |
| ------------- | ------------- |
|   | 0m0.139s  | 0m0.132s
| 0m0.117s  | 0m0.119s  |
| 0m0.119s  | 0m0.118s  |
| 0m0.124s  | 0m0.117s  |
| 0m0.122s  | 0m0.121s  |

The diagnostics show only 10% of the overall work was run in parallel, so perhaps not surprising.

#### Serial version

```
$ stack exec PokerPuzzle -- +RTS -s

Player 1 has won 376 times
      14,626,248 bytes allocated in the heap
         559,320 bytes copied during GC
         320,000 bytes maximum residency (2 sample(s))
          69,624 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0        27 colls,     0 par    0.001s   0.001s     0.0000s    0.0002s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0004s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.006s  (  0.007s elapsed)
  GC      time    0.001s  (  0.001s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.042s  (  0.008s elapsed)

  %GC     time       2.1%  (13.4% elapsed)

  Alloc rate    2,417,961,315 bytes per MUT second

  Productivity  97.6% of total user, 520.9% of total elapsed
```

#### Parallel version

```
$ stack exec PokerPuzzle -- +RTS -N -s
Player 1 has won 376 times
      15,334,144 bytes allocated in the heap
         735,264 bytes copied during GC
         358,288 bytes maximum residency (1 sample(s))
         139,312 bytes maximum slop
               5 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         5 colls,     4 par    0.002s   0.001s     0.0002s    0.0005s
  Gen  1         1 colls,     1 par    0.001s   0.000s     0.0004s    0.0004s

  Parallel GC work balance: 10.64% (serial 0%, perfect 100%)

  TASKS: 18 (1 bound, 17 peak workers (17 total), using -N8)

  SPARKS: 1000 (1000 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    0.013s  (  0.004s elapsed)
  GC      time    0.003s  (  0.001s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.052s  (  0.006s elapsed)

  Alloc rate    1,164,854,451 bytes per MUT second

  Productivity  92.7% of total user, 743.6% of total elapsed

```
