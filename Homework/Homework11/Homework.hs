import Data.List
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, listDirectory)
import Text.XHtml (thead)
import Data.Foldable (foldr)
import Control.Monad (foldM_)


{-
We imported some functions that you'll need to complete the homework.
FilePath is just a synonym for String. Although, make sure to follow the standard path
representation when using them (https://en.wikipedia.org/wiki/Path_(computing).
getCPUTime    :: IO Integer
doesFileExist :: FilePath -> IO Bool
listDirectory :: FilePath -> IO [FilePath]
You can hover over the functions to know what they do.
-}

{-
-- Question 1 --
Define an IO action that counts the number of files in the current directory
and prints it to the terminal inside a string message.
-}

listFiles :: IO ()

listFiles = do
  ld <- listDirectory "."
  putStrLn $ show $ length ld

{-
-- Question 2 --
Write an IO action that asks the user to type something, then writes the message
to a file called msg.txt, and after that, it reads the text from the msg.txt
file and prints it back. Use the writeFile and readFile functions.
-}

createMsg :: IO ()

createMsg = do
  putStrLn "Write a message:"
  msg <- getLine
  writeFile "./msg.txt" msg
  readmsg <- readFile "./msg.txt"
  putStrLn $ "Your stored message is: " ++ readmsg

{-
-- Context for Questions 3 and 4 --
In cryptography, prime numbers (positive integers only divisible by themselves and 1) play a fundamental
role in providing unbreakable mathematical structures. These structures, in turn, are leveraged to
establish secure encryption.
But, generating primes is a computational straining problem, as we will measure in the following exercise.
This is because, to know whether a number is a prime number, you first need to know all the previous primes
and then check that they are not a divisor of this number. So, this problem gets bigger and bigger!
Our lead cryptographer provided us with 3 different algorithms (primes1, primes2, and primes3). All three
correctly produce a list of all the prime numbers until a limit (that we provide as a parameter).
Our job is not to understand these algorithms but to measure which is the fastest and print the largest
prime number below our limit. Do it step by step, starting with question 3.
-}

primes1 :: Integer -> [Integer]
primes1 m = sieve [2 .. m]
 where
  sieve [] = []
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes2 :: Integer -> [Integer]
primes2 m = sieve [2 .. m]
 where
  sieve (x : xs) = x : sieve (xs \\ [x, x + x .. m])
  sieve [] = []

primes3 :: Integer -> [Integer]
primes3 m = turner [2 .. m]
 where
  turner [] = []
  turner (p : xs) = p : turner [x | x <- xs, x < p * p || rem x p /= 0]

{-
-- Question 3 --
Define an IO action that takes an IO action as input and calculates the time it takes to execute.
Use the getCPUTime :: IO Integer function to get the CPU time before and after the IO action.
The CPU time here is given in picoseconds (which is 1/1000000000000th of a second).
-}

timeIO :: IO a -> IO ()

timeIO  action = do
  startTime <- getCPUTime
  action
  endTime <- getCPUTime
  putStrLn $ show $ fromIntegral (endTime - startTime)  / 1e12






{-
-- Question 4 --
Write an action that retrieves a value from the standard input, parses it as an integer,
and compares the time all three algorithms take to produce the largest prime before the
limit. Print the number and time to the standard output.
-}

-- runAndPrint :: (Integer -> [Integer]) -> Integer -> IO ()

-- runAndPrint primes m = do
--     putStrLn $ "largest prime " ++ show (last $ primes m)


benchmark :: IO ()

benchmark = do
  input <- getLine
  let validInput = all (`elem` "1234567890") input
  if validInput
    then do 
      let test = read input :: Integer
      timeIO $ return $ primes1 test
      timeIO $ return $ primes2 test
      timeIO $ return $ primes3 test
    else do
            putStrLn "You  must enter an integer [Press Ctrl-C to end]" 
            benchmark

{-
 -- Question 5 -- EXTRA CREDITS -- (In case the previous ones were too easy)
Write a program that prints the directory tree structure from the current folder.
Below you can see an example output of how such a structure looks like:
.
├── foo1.hs
├── foo2.hs
├── bar1
    ├── bar2
    ├── foo3.hs
    ├── foo4.hs
    └── bar3
        └── foo5.hs
└── bar5
    ├── bar6
    ├── foo6.hs
    └── foo7.hs
HINT: You can use the function doesFileExist, which takes in a FilePath and returns
True if the argument file exists and is not a directory, and False otherwise.
-}


-- printDirectory :: IO ()
-- splitList :: [a] -> ([a], a)
-- splitList xs = (init xs, last xs)


-- printDirectory = printDirectoryIndent "" "."


-- printDirectoryIndent :: String -> FilePath -> IO ()
-- printDirectoryIndent str dir =

--    do 
--       de <- doesFileExist dir
--       if not de
--          then do
--               dirs <- listDirectory dir
--               case dirs of
--                   [] -> return ()
--                   ds -> let (initds, lastd) = splitList ds
--                             lastdAction = printDirectoryIndent (str ++ "└── ") lastd
--                         in foldM_ (\_ d -> printDirectoryIndent (str ++ "├── ") d) () initds >> lastdAction
--           else putStrLn $ str ++ dir

data DirectoryStructure = File String | Folder String [IO DirectoryStructure]

-- This action prints the structure neatly using the helper functions below.
printDirectoryTree :: IO ()
printDirectoryTree = do
  putStrLn "."
  structure <- returnStructure "."
  printStructure structure 0

-- Here, we give structure to our string by parsing it into our data type.
returnStructure :: FilePath -> IO [IO DirectoryStructure]
returnStructure filePath = do
  contents <- listDirectory filePath
  return $ map go contents
 where
  go fileName = do
    let newFilePath = filePath ++ "/" ++ fileName
    isFile <- doesFileExist newFilePath
    if isFile
      then return $ File fileName
      else do
        structure <- returnStructure newFilePath
        return $ Folder fileName structure

-- It prins the whole structure to the standard ouptut
printStructure :: [IO DirectoryStructure] -> Int -> IO ()
printStructure [] _ = return ()
printStructure (x : xs) level = do
  structure <- x
  case structure of
    File name -> do
      printSpaces level
      printElement xs name
    Folder name ioList -> do
      printSpaces level
      printElement xs name
      printStructure ioList (level + 1)
  printStructure xs level

-- It prins a single element to the standard ouptut
printElement :: [IO DirectoryStructure] -> String -> IO ()
printElement xs name =
  if null xs
    then putStrLn $ "└── " ++ name
    else putStrLn $ "├── " ++ name

-- It prins spaces to the standard ouptut
printSpaces :: Int -> IO ()
printSpaces n =
  if n < 1
    then return ()
    else do
      putStr "    "
      printSpaces (n - 1)