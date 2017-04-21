// Card ranks represented as simple ints

module RankModel

type Rank = int         // Jack = 11, Queen = 12, King = 13, Ace = 14

let rankFromInt = id

let rankFromDigit (d:char) : Rank = (int d) - 48

// Constants for tests
let Ace = 14
let King = 13
let Queen = 12
let Jack = 11
let Ten = 10
