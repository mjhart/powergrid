{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Powergrid where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Extras
import Data.Foldable
import Data.List
import qualified Data.Map as Map
import qualified Text.Read as Read

class Printable a where
    prettyPrint :: a -> String

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
    { _coal :: Int
    , _oil :: Int
    , _garbage :: Int
    , _uranium :: Int
    } deriving (Show)

data Game = Game
    { _accounts :: Accounts
    , _phase :: Phase
    , _resourceMarket :: ResourceMarket
    } deriving (Show)

makeLenses ''Game
makeLenses ''ResourceMarket

instance Printable Phase where
    prettyPrint = show

instance Printable ResourceMarket where
    prettyPrint rm =
        let values = fmap (\l -> show . (view l) $ rm) lenses
        in unwords $ zipWith (++) labels values 
            where labels = ["Coal: ", "Garbage: ", "Oil: ", "Uranum: "]
                  lenses = [coal, garbage, oil, uranium]

instance Printable (Map.Map Name Balance) where
    prettyPrint = unwords . fmap printEntry . Map.toList
        where printEntry (name, bal) = name ++ ": " ++ (show bal)

instance Printable Game where
    prettyPrint game = unlines [printPart phase, printPart resourceMarket, printPart accounts]
        where printPart :: (Printable p) => Getting p Game p -> String
              printPart = prettyPrint . (flip view game)

main = do
    accounts <- setUp
    runStateT (forever run) $ initialGame accounts

run :: StateT Game IO ()
run = do
    game <- get
    liftIO . putStrLn $ prettyPrint game
    newGame <- liftIO $ getCommand game
    put newGame

adjustAccounts :: Accounts -> IO Accounts
adjustAccounts as = do
    name <- chooseName $ Map.keys as
    putStrLn "Delta:"
    val <- getNumber
    return $ Map.adjust (+ val) name as

chooseName :: [Name] -> IO Name
chooseName names = getPlayerInput
    "Choose Player:"
    errMsg
    (\name -> if name `elem` names then Just name else Nothing)
        where errMsg = "Player must be one of: " ++ (unwords names)


adjustResources :: ResourceMarket -> IO ResourceMarket
adjustResources rm = do
    resource <- getPlayerInput
        "Select resource: coal | garbage | oil | uranium"
        "Unrecognized resource"
        parseResource
    putStrLn "Delta:"
    delta <- getNumber
    return . (over resource) (flip (-) delta) $ rm

parseResource :: String -> Maybe (ASetter ResourceMarket ResourceMarket Int Int)
parseResource "coal" = Just coal
parseResource "garbage" = Just garbage
parseResource "oil" = Just oil
parseResource "uranium" = Just uranium
parseResource _ = Nothing

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
    { _coal = (_coal rm1) + (_coal rm2)
    , _oil = (_oil rm1) + (_oil rm2)
    , _garbage = (_garbage rm1) + (_garbage rm2)
    , _uranium = (_uranium rm1) + (_uranium rm2)
    }

-- Set up stuff --
setUp :: IO Accounts
setUp = do
    putStrLn "Hi! Welcome to gbank. How many players will be playing today?"
    n <- getNumber
    fmap buildMap (getNames n)
    where buildMap = foldr (\k -> Map.insert k 0) Map.empty

initialGame :: Accounts -> Game
initialGame accounts = Game accounts PhaseOne initialResourceMarket
    where initialResourceMarket = ResourceMarket { _coal = 24
        , _oil = 18
        , _garbage = 6
        , _uranium = 2
        }

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

getCommand :: Game -> IO Game
getCommand g = do
    command <- getPlayerInput
        "Enter command: money | resources | turn | phase"
        "Unrecognized command"
        parseCommand
    command g


parseCommand :: String -> Maybe (Game -> IO Game)
parseCommand "money" = Just (\g -> do 
    as <- adjustAccounts (view accounts g)
    return $ set accounts as g)
parseCommand "resources" = Just (\g -> do 
    rm <- adjustResources (view resourceMarket g)
    return $ set resourceMarket rm g)
parseCommand "turn" = Just $ return . nextTurn
parseCommand "phase" = Just $ return . over phase succ
parseCommand _ = Nothing

nextTurn :: Game -> Game
nextTurn g =
    let numPlayers = Map.size . view accounts $ g
        curPhase = view phase g
        delta = resourceDelta numPlayers curPhase
    in over resourceMarket (addResources delta) g

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
