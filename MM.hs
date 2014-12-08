-- Yasmina Kada
-- 10001567
-- Programmeertal

module MM

where

import Data.List
import Control.Monad (replicateM)

data Color = Red | Yellow | Blue | Green | Orange | Purple
			deriving (Eq, Show, Bounded ,Enum)

type Pattren = [Color]

--(# Black , # White )
type Feedback = ( Int , Int )

reaction :: Pattren -> Pattren -> Feedback
reaction secret guess =
	let 
		b = correctGuess secret guess
		w = correctColor secret guess - b
	in (b,w)

-- White: correct color - all correct color on right position
correctColor :: Pattren -> Pattren -> Int
correctColor _ [] = 0
correctColor [] _ = 0
correctColor (x:xs) secret
	| (x `elem` secret) = correctColor xs rest_secret + 1
	| otherwise		= correctColor xs rest_secret
	where rest_secret = delete x secret

-- Black: correct position and color.
correctGuess :: Pattren -> Pattren -> Int
correctGuess [] [] = 0
correctGuess (x:xs) (y:ys)
	| (x == y) = correctGuess xs ys + 1
	| otherwise		= correctGuess xs ys + 0

store2 = [ [a,b,c,d] | a <- allColors, b <- allColors, c <- allColors, d <- allColors]
	where allColors = [Red,Yellow,Blue,Green,Orange,Purple]

colors :: [ Color ]
colors = [ minBound .. maxBound]
store = replicateM 4 colors


naive_algorithm :: Pattren -> [Pattren]
naive_algorithm secret = build_list secret store []

build_list [] _ tried_list = tried_list -- Loop end
build_list secret store list
	| try_reaction /= (4,0) = build_list secret new_store (first:list) -- Loop end
	| otherwise		= (first:list)
    where 
		first = head store
		try_reaction = reaction secret first
		new_store = remove_wrong store secret first

-- Remove from store: reaction(y,x) /= reaction(secret,x)
----------- Aangepast zodat het zou moeten werken zoals beschreven in de opdracht; werkt niet.
remove_wrong store secret first = [ z | z  <- store, reaction z first /= reaction secret first]

-------- Zoals ik het eerst had, geeft wel de oplossing alleen in meer stappen
-------- enige verschil zit in de volgorde:
-- remove_wrong store secret first = [ z | z  <- store, reaction secret z /= reaction secret first]



benchmarking= fromIntegral(x) / fromIntegral(y)
	where 
		x = sum (lengthslist (get_all_outcomes store))
		y = length store

get_all_outcomes store = all_outcomes store []

all_outcomes [] list = list
all_outcomes (x:xs) list=  all_outcomes xs (y:list) 
	where
		y = naive_algorithm x

lengthslist all_outcomes = [ length a | a <- all_outcomes]