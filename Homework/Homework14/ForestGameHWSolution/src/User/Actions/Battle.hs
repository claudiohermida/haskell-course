{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module User.Actions.Battle where

import System.Random (randomRIO)

data Golem = Golem {gAttack :: Int, gHp :: Int} deriving (Show)

data Player = Player {pAttack :: Int, pHp :: Int} deriving (Show)

data Battle = Fight | RunAway deriving (Show, Read)

randomGolem :: IO Golem
randomGolem = do
  gAttack <- randomRIO @Int (0, 10)
  gHp <- randomRIO @Int (0, 100)
  return Golem {..}

randomPlayer :: IO Player
randomPlayer = do
  pAttack <- randomRIO @Int (0, 10)
  pHp <- randomRIO @Int (0, 100)
  return Player {..}

battleLoop :: Golem -> Player -> IO Bool
battleLoop Golem {gAttack, gHp} Player {pAttack, pHp} = do
  if gHp <= 0
    then return True
    else
      ( if pHp <= 0
          then return False
          else do
            putStrLn $ "The Golem has " ++ show gHp ++ " health and " ++ show gAttack ++ " attack"
            putStrLn $ "You have " ++ show pHp ++ " health and " ++ show pAttack ++ " attack"
            putStrLn "Choose an action: Fight or RunAway?"
            selectedMove <- getLine
            luckFactor <- randomRIO @Int (1, 3)
            case read @Battle selectedMove of
              Fight ->
                if gHp <= 0
                  then return True
                  else battleLoop Golem {gAttack, gHp = gHp - luckFactor * pAttack} Player {pAttack, pHp = pHp - luckFactor * gAttack}
              RunAway ->
                if pHp <= 0
                  then return False
                  else battleLoop Golem {gAttack, gHp = gHp - luckFactor * pAttack} Player {pAttack, pHp = pHp - luckFactor * gAttack}
      )

battle :: IO Bool
{- battle = undefined -}
battle =
  do
    putStrLn "\nYou've encountered a Golem!"
    rG <- randomGolem
    rP <- randomPlayer
    resultBattle <- battleLoop rG rP
    if resultBattle
      then do
        putStrLn "\nYou've won the battle!"
        return True
      else do
        putStrLn "\nThe Golem killed you!"
        return False