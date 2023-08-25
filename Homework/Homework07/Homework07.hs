-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?

-- minBound and maxBound values

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.

--  minBound :: Word   -- 0
--  maxBound :: Word   -- 18446744073709551615
--  minBound :: Int    -- -9223372036854775808
--  maxBound :: Int    -- 9223372036854775807

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?

{- 
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
-}

-- Question 4
-- Add the most general type signatures possible to the functions below.
-- Then uncomment the functions and try to compile.

-- f1 :: (Fractional a, Show a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

-- f2 :: (Eq a, Enum a, Bounded a) => a -> a 
f2 x = if x == maxBound then minBound else succ x


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.
