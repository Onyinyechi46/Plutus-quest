module Main where

import System.IO (hFlush, stdout, hSetEncoding, utf8)
import Data.Char (toLower)

-- ============================
-- Game Types & State
-- ============================

data Level
  = Tutorial
  | UTxOForest
  | ValidatorGate
  | DatumCave
  | RedeemerArena
  | Finished
  deriving (Eq, Show)

data UTxO = UTxO
  { utxoId         :: Int
  , ownerPk        :: String
  , lockedByScript :: Bool
  , datum          :: Maybe Int
  , value          :: Integer
  } deriving (Show)

data GameState = GameState
  { playerName :: String
  , playerPk   :: String
  , tokens     :: Int
  , level      :: Level
  } deriving (Show)

initialState :: GameState
initialState = GameState
  { playerName = ""
  , playerPk   = "pkh_player_001"
  , tokens     = 0
  , level      = Tutorial
  }

-- ============================
-- Utilities
-- ============================

prompt :: String -> IO String
prompt msg = do
  putStr msg
  putStr " "
  hFlush stdout
  fmap trim getLine

trim :: String -> String
trim = f . f where f = reverse . dropWhile (`elem` [' ', '\t'])

normalize :: String -> String
normalize = map toLower . trim

pause :: IO ()
pause = do
  _ <- prompt "\n[Enter to continue]"
  pure ()

award :: Int -> GameState -> GameState
award n s = s { tokens = tokens s + n }

advance :: Level -> GameState -> GameState
advance lv s = s { level = lv }

say :: String -> IO ()
say = putStrLn

printU :: UTxO -> IO ()
printU u = say $ "  [" ++ show (utxoId u) ++ "] " ++ describe u
  where
    describe x =
      (if lockedByScript x
         then "Script-locked (datum=" ++ maybe "none" show (datum x) ++ ")"
         else "Key-locked by " ++ ownerPk x)
      ++ ", value=" ++ show (value x) ++ " ADA"

-- ============================
-- Main Loop
-- ============================

main :: IO ()
main = do
  hSetEncoding stdout utf8
  introArt
  say "Welcome to PLUTUS QUEST!"
  name <- prompt "Adventurer, what shall we call you?"
  let s0 = initialState { playerName = if null name then "Apprentice" else name }
  say ("\nHello, " ++ playerName s0 ++ "! Your training key-hash is: " ++ playerPk s0)
  gameLoop s0

gameLoop :: GameState -> IO ()
gameLoop s = case level s of
  Tutorial      -> tutorialLevel s >>= gameLoop
  UTxOForest    -> utxoForestLevel s >>= gameLoop
  ValidatorGate -> validatorGateLevel s >>= gameLoop
  DatumCave     -> datumCaveLevel s >>= gameLoop
  RedeemerArena -> redeemerArenaLevel s >>= gameLoop
  Finished      -> finale s >> pure ()  -- Ensure IO () is returned

-- ============================
-- Levels
-- ============================

tutorialLevel :: GameState -> IO GameState
tutorialLevel s = do
  say "\n=== LEVEL 0: TUTORIAL ==="
  say "Question: what is double 21?"
  ans <- prompt "> Your answer (just the number):"
  if normalize ans == "42"
    then do
      say "Correct!"
      let s' = award 1 s
      pause
      pure (advance UTxOForest s')
    else do
      say "Not quite. Hint: It's 21 multiplied by 2. Try again."
      tutorialLevel s

utxoForestLevel :: GameState -> IO GameState
utxoForestLevel s = do
  say "\n=== LEVEL 1: THE UTxO FOREST ==="
  let pkYou = playerPk s
      demo =
        [ UTxO 1 pkYou False Nothing 5
        , UTxO 2 "pkh_other_123" False Nothing 10
        , UTxO 3 "pkh_other_999" True (Just 99) 20
        ]
  mapM_ printU demo
  ans <- prompt "> Which UTxO can you spend (number only)?"
  if normalize ans == "1"
    then do
      say "Correct!"
      let s' = award 2 s
      pause
      pure (advance ValidatorGate s')
    else do
      say "Wrong, try again."
      utxoForestLevel s

validatorGateLevel :: GameState -> IO GameState
validatorGateLevel s = do
  say "\n=== LEVEL 2: VALIDATOR GATE ==="
  say "Which validator opens the gate? A) True B) False"
  pick <- prompt "> Choose A or B:"
  case normalize pick of
    "a" -> do
      say "Correct!"
      let s' = award 3 s
      pause
      pure (advance DatumCave s')
    "b" -> do
      say "Wrong, try again."
      validatorGateLevel s
    _ -> validatorGateLevel s

datumCaveLevel :: GameState -> IO GameState
datumCaveLevel s = do
  say "\n=== LEVEL 3: DATUM CAVE ==="
  say "If datum is 15 and rule is datum > 10, will cave open? (yes/no)"
  ans <- prompt "> Your answer:"
  case normalize ans of
    "yes" -> do
      say "Correct!"
      let s' = award 4 s
      pause
      pure (advance RedeemerArena s')
    "no" -> do
      say "Wrong, try again."
      datumCaveLevel s
    _ -> datumCaveLevel s

redeemerArenaLevel :: GameState -> IO GameState
redeemerArenaLevel s = do
  say "\n=== LEVEL 4: REDEEMER ARENA ==="
  say "Which redeemer opens the arena? Unlock or Lock?"
  ans <- prompt "> Your answer:"
  case normalize ans of
    "unlock" -> do
      say "Correct!"
      let s' = award 5 s
      pause
      pure (advance Finished s')
    "lock" -> do
      say "Wrong, try again."
      redeemerArenaLevel s
    _ -> redeemerArenaLevel s

-- ============================
-- Finale
-- ============================

finale :: GameState -> IO ()
finale s = do
  say "\n=== CONGRATULATIONS ==="
  say ("Player: " ++ playerName s ++ ", Tokens: " ++ show (tokens s))
  say "You have completed Plutus Quest!"

-- ============================
-- ASCII Art
-- ============================

introArt :: IO ()
introArt = putStrLn $ unlines
  [ "  ____  _       _           _           ____                  _   "
  , " |  _ \\| | ___ | |_   _  __| | ___ _   _/ ___| _   _ _ __   __ _| |__"
  , " | |_) | |/ _ \\| | | | |/ _` |/ _ \\ | |\\___ \\| | | | '_ \\ / _` | '_ \\"
  , " |  __/| | (_) | | |_| | (_| |  __/ \\ V / ___) | |_| | | | | (_| | | | |"
  , " |_|   |_|\\___/|_|\\__, |\\__,_|\\___/ \\_/ |____/ \\__,_|_| |_|\\__, |_| |_|"
  , "                  |___/                                   |___/        "
  , "\n        Welcome to Plutus Quest!"
  ]
