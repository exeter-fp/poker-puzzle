// Card Rank  represented as enumeration (labeled ints)

module RankModel

type Rank =
| Two = 2
| Three = 3
| Four = 4
| Five = 5
| Six = 6
| Seven = 7
| Eight = 8
| Nine = 9
| Ten = 10
| Jack = 11
| Queen = 12
| King = 13
| Ace = 14

let rankFromInt i : Rank = enum i

let rankFromDigit (d:char) : Rank = ((int d) - 48) |> enum 

// Constants for tests
let Ace = Rank.Ace
let King = Rank.King
let Queen = Rank.Queen
let Jack = Rank.Jack
let Ten = Rank.Ten