# Haskell Poker Puzzle Solution

A solution to [Project Euler problem #54](https://projecteuler.net/problem=54) written in Haskell.


## Pre-requisites

This project is built using [Stack](https://docs.haskellstack.org/en/stable/README).  Follow the instructions on the Stack website to download a Stack binary and install, or use your Operating System's package manager.


## Building

- Run `stack setup` to set up stack and install the necessary version of GHC.
- Run `stack build` to build the project.


## Running Tests

- Run `stack test` to run the test cases located in [test/Spec.hs](test/Spec.hs).


## Executing the Project

- Run `stack exec poker-puzzle-exe` to run the project.


## Main Files

- [src/Model.hs](src/Model.hs) - underlying data model, and supporting functions to locate best hands.
- [src/Util.hs](src/Util.hs) - supporting utility functions that are independent of the problem.
- [src/Parsing.hs](src/Parsing.hs) - code to parse inputs into the model.
- [app/Main.hs](app/Main.hs) - main application entry point
