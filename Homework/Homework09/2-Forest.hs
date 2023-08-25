{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

{- import Data.List (isPrefixOf) -}


data Move = GoRight | GoLeft | GoForward deriving (Eq)

type Stamina = Int

moveWeight :: Move -> Int

moveWeight GoForward = 3
moveWeight _         = 4

type Forest = [Move]

testForest = [GoForward, GoLeft , GoRight] :: Forest







solveForest':: Forest ->([Move],Stamina) -> (Forest,[Move],Stamina)

{- solveForest' forest (moves,stamina)
                    | [] (ms,st) = ([],ms,st)
                    | m:forest' ((m':ms),st)  = if (m == m') then solveForest' forest' (ms,st - moveWeight m) 
                                                                 else solveForest' m:forest' (ms,st - moveWeight m')
                    | f  ([],st) = (f,[], st)
-}


solveForest' [] (ms, st) = ([], ms, st)
solveForest' (m:forest') ((m':ms),st)  = if (m == m') then solveForest' forest' (ms,st - moveWeight m) 
                                                                 else solveForest' (m:forest') (ms,st -moveWeight m')
solveForest' f ([],st) = (f,[],st)

solveForest :: Forest -> [Move] -> String
solveForest forest moves = let (f,ms,st) = solveForest' forest (moves,10)
                                in case f of 
                                     [] ->   "YOU'VE FOUND THE EXIT!!"
                                     _  -> if (ms == [] && st > 0) then  "You have " ++ show st ++" stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."                 
                                                                  else   "You ran out of stamina and died -.-!"