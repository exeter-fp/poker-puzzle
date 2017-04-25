defmodule Cards do
    # Match suits and return keyword
    def suit("D"), do: :diamonds
    def suit("S"), do: :spades
    def suit("H"), do: :hearts
    def suit("C"), do: :clubs

    def rank(c) do
        ranks = %{ "A" => 14, "K" => 13, "Q" => 12, "J" => 11, "T" => 10 }
        r = ranks[c]
        if r === nil do
            String.to_integer c
        else r end            
    end

    def card(txt) do
       {(txt |> String.at(0) |> rank), (txt |> String.at(1) |> suit )}
    end

    def hand(txt) do
        txt 
        |> String.split(" ")
        |> Enum.map(&card/1)
    end
end

defmodule Scoring do
    
    def is_flush(h) do
        h |> Enum.map(fn {_r,s} -> s end) |> Enum.dedup |> Enum.count === 1
    end

end


parsed = Cards.hand( "5D 8C 9S JS AC")
parsed |> Scoring.is_flush
