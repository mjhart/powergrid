module Gbank where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import qualified Text.Read as Read

type Name = String

type Balance = Int

type Accounts = Map.Map Name Balance

data Command
    = Money 
    | Resources 
    | Turn 
    | Next 
    deriving (Show)

data Player = Player
    { name :: Name
    , balance :: Balance
    } 

data Phase
    = PhaseOne 
    | PhaseTwo 
    | PhaseThree 
    deriving (Show,Enum)

data ResourceMarket = ResourceMarket
    { coal :: Int
    , oil :: Int
    , garbage :: Int
    , uranium :: Int
    } deriving (Show)

data Game = Game
    { accounts :: Accounts
    , phase :: Phase
    , resourceMarket :: ResourceMarket
    } deriving (Show)

main = do
    accounts <- setUp
    runStateT (forever run) $ initialGame accounts

run :: StateT Game IO ()
run = do
    game <- get
    liftIO . putStrLn $ show game
    command <- liftIO getCommand
    case command of
        Money -> 
            (liftIO (adjustAccounts $ accounts game)) >>=
            (\as -> put game { accounts = as })
        Resources -> 
            (liftIO (adjustResources $ resourceMarket game)) >>=
            (\rm -> put game { resourceMarket = rm })
        Turn -> 
            put game
                { resourceMarket = addResources
                      (resourceMarket game)
                      (resourceDelta (Map.size . accounts $ game) (phase game))
                }
        Next -> modify nextPhase
        where nextPhase g@Game{phase = p} = g { phase = succ p }

adjustAccounts :: Accounts -> IO Accounts
adjustAccounts as = do
    name <- getName
    putStrLn "Delta:"
    val <- getNumber
    return $ Map.adjust (+ val) name as

adjustResources :: ResourceMarket -> IO ResourceMarket
adjustResources (rm@ResourceMarket{coal = c, oil = o, garbage = g, uranium = u}) = do
    putStrLn "Select resource: coal | garbage | oil | uranium"
    resource <- getLine
    case resource of
        "coal" ->
            fmap (\delta -> rm { coal = c - delta }) getDelta
        "garbage" ->
            fmap (\delta -> rm { garbage = g - delta }) getDelta
        "oil" ->
            fmap (\delta -> rm { oil = o - delta }) getDelta
        "uranium" ->
            fmap (\delta -> rm { uranium = u - delta }) getDelta
        otherwise -> unrecognized >> adjustResources rm
        where unrecognized = putStrLn "Unrecognized resource"
              getDelta = putStrLn "Delta:" >> getNumber

resourceDelta :: Int -> Phase -> ResourceMarket
resourceDelta 2 PhaseOne = ResourceMarket 3 2 1 1
resourceDelta 2 PhaseTwo = ResourceMarket 4 2 2 1
resourceDelta 2 PhaseThree = ResourceMarket 3 4 3 1
resourceDelta 3 PhaseOne = ResourceMarket 4 2 1 1
resourceDelta 3 PhaseTwo = ResourceMarket 5 3 2 1
resourceDelta 3 PhaseThree = ResourceMarket 3 4 3 1
resourceDelta 4 PhaseOne = ResourceMarket 5 3 2 1
resourceDelta 4 PhaseTwo = ResourceMarket 6 4 3 2
resourceDelta 4 PhaseThree = ResourceMarket 4 5 4 2
resourceDelta 5 PhaseOne = ResourceMarket 5 4 3 2
resourceDelta 5 PhaseTwo = ResourceMarket 7 5 3 3
resourceDelta 5 PhaseThree = ResourceMarket 5 6 5 2
resourceDelta 6 PhaseOne = ResourceMarket 7 5 3 2
resourceDelta 6 PhaseTwo = ResourceMarket 9 6 5 3
resourceDelta 6 PhaseThree = ResourceMarket 6 7 6 3

addResources :: ResourceMarket -> ResourceMarket -> ResourceMarket
addResources rm1 rm2 = 
    ResourceMarket
    { coal = (coal rm1) + (coal rm2)
    , oil = (oil rm1) + (oil rm2)
    , garbage = (garbage rm1) + (garbage rm2)
    , uranium = (uranium rm1) + (uranium rm2)
    }

-- Set up stuff -- 
setUp :: IO Accounts
setUp = do
    putStrLn "Hi! Welcome to gbank. How many players will be playing today?"
    n <- getNumber
    fmap buildMap (getNames n)
    where buildMap = foldr (\k -> Map.insert k 0) Map.empty

initialResourceMarket :: ResourceMarket
initialResourceMarket = 
    ResourceMarket
    { coal = 24
    , oil = 18
    , garbage = 6
    , uranium = 2
    }

initialGame :: Accounts -> Game
initialGame accounts = Game accounts PhaseOne initialResourceMarket

getNames :: Int -> IO [Name]
getNames n = 
    if n > 0
        then do
            name <- getName
            others <- getNames (n - 1)
            return $ name : others
        else return []

getName :: IO Name
getName = getPlayerInput
    "Player name:"
    "Name cannot be empty"
    (\name -> if null name then Nothing else Just name)

getCommand :: IO Command
getCommand = getPlayerInput 
    "Enter command: money | resources | next | turn"
    "Unrecognized command"
    parse
    where parse "money" = Just Money
          parse "resources" = Just Resources
          parse "next" = Just Next
          parse "turn" = Just Turn
          _ = Nothing

getNumber :: IO Int
getNumber = do
    maybeNum <- fmap Read.readMaybe getLine
    case maybeNum of
        Just n -> return n
        Nothing -> putStrLn "Not a number. Please try again" >> getNumber

getPlayerInput :: String -> String -> (String -> Maybe a) ->  IO a
getPlayerInput prompt errorMsg parse = do
    putStrLn prompt
    input <- fmap parse getLine
    case input of
        Just value -> return value
        Nothing -> putStrLn errorMsg >> getPlayerInput prompt errorMsg parse
