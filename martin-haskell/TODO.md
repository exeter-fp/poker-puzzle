# Next changes for Puzzle

- Remove `One` from `Rank` enum - it was never supposed to be there in the first place.
- Investigate guards to replace `if pred then Just Foo else Nothing` and `|` guards
- Try profiling with multiple dataset runs and compare eager applicative vs lazy find approaches to `bestHand`