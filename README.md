# Hasnakell

Snake game in terminal, written in Haskell.

<img src="https://github.com/immoh/hasnakell/blob/master/doc/hasnakell-screenshot.png" width="529" height="444" />

This repository is mostly for me to learn Haskell. It is likely that the code is pretty bad.

## How to Play

Install [Cabal](https://www.haskell.org/cabal/) and type in your terminal:
```
cabal run
```

The game is best viewed if terminal height is close to game area height (15 lines).
You can change the direction of the snake using keys `i` (up), `k` (down), `j` (left) and `l` (right).
The game exits when the snake crosses itself.

## TODO

* Write tests
* Write rendering using a GUI library
