{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
-}

data Box a = Empty | Has a  deriving (Show)

class Container c where
    isEmpty :: c a -> Bool
    contains :: (Eq a) => c a -> a -> Bool
    replace :: c a -> b -> c b

    unwrap' :: (Container c, Eq a) => c a -> a 

instance Container Box where
    isEmpty Empty  = True
    isEmpty (Has _)  = False

    contains Empty _ = False
    contains (Has x) y = x == y

    replace  _  x = Has x

    unwrap' (Has x) = x 
    


{-
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a deriving (Show)

instance Container (MailedBox t d) where
  isEmpty (EmptyMailBox _ _) = True
  isEmpty (MailBoxTo _ _ _)    = False

  (EmptyMailBox _ _) `contains` _ = False
  (MailBoxTo _ _ x) `contains` y    = x == y

  (EmptyMailBox tag d) `replace` x = MailBoxTo tag d x
  (MailBoxTo tag d content) `replace` x = MailBoxTo tag d x

  unwrap' (MailBoxTo tag d content) = content

-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Eq,Ord,Show)

data Experience = Programming | Managing | Leading deriving (Eq,Ord,Show)

type Address = String 

data Salary = USD Double | EUR Double deriving (Show)

instance Eq Salary where
  USD x == USD y = x == y
  EUR x == EUR y = x == y
  USD x == EUR y = x == y * 1.2
  EUR x == USD y = y == x * 1.2


instance Ord Salary where
  USD x <= USD y = x <=y
  EUR x <= EUR y = x <=y
  USD x <= EUR y = x <= (y * 1.2)
  EUR x <= USD y = (x * 1.2) <= y

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address    deriving (Eq,Show)


instance Ord Relationship where
  (Contractor p e s _) <= (Contractor p' e' s' _) = (p == p') && (e == e') && s <= s'
  (Employee p e s _) <= (Employee p' e' s' _) = (p == p') && (e == e') && s <= s'
  _ <= _  = False

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  } deriving (Eq,Show)


instance Ord Pokemon where
  (Pokemon _ _ g d) <= (Pokemon _ _ g' d') = g <= g' && d <= d'

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show)

-- (+):: Exp -> Exp -> Exp
-- (Exp d) + (Exp d') = Exp (d Prelude.+ d') 

-- 0 = Exp 0

-- Team memeber data
type TeamMember = (String, Exp)

-- List of memebers of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0

instance Num Exp where
  (+) (Exp x) (Exp y) = Exp (x + y)
  (*) (Exp x) (Exp y) = Exp (x * y)
  fromInteger x = Exp (fromInteger x)
  signum (Exp x) = Exp (signum x)
  abs (Exp x) = Exp (abs x)
  negate (Exp x) = Exp (negate x)