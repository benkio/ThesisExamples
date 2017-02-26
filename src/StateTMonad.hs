--
-- another example: a guessing game
-- (from http://scsibug.com/2006/11/28/a-simple-game-with-statet/)
--

module StateTMonad where
import System.Random
import Control.Monad.State

stateTMain :: IO ()
stateTMain = do answer <- getStdRandom (randomR (1,100)) -- think of a number
                putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
                guesses <- execStateT (guessSession answer) 0
                putStrLn $ "Success in " ++ (show guesses) ++ " tries."

guessSession :: Int -> StateT Int IO ()
guessSession answer =
    do gs <- lift getLine    -- get guess from user
       let g = read gs       -- convert to number
       modify (+1)           -- increment number of guesses
       case compare g answer of
              LT -> do lift $ putStrLn "Too low"
                       guessSession answer
              GT -> do lift $ putStrLn "Too high"
                       guessSession answer
              EQ -> lift $ putStrLn "Got it!"

------------------- 110 recognizer --------------------------

data 110State = S1 | S2 | S3 | S4

machineFunction :: 110State -> IO Unit
machineFunction S1 = putStrLn 0
machineFunction S2 = putStrLn 0
machineFunction S3 = putStrLn 0
machineFunction S4 = putStrLn 1

stateFunction :: Int -> 110State -> 110State
machineFunction x S1 = if x == 0 then S1  else S2
machineFunction x S2 = if x == 0 then S1  else S3
machineFunction x S3 = if x == 0 then S4  else S1
machineFunction x S4 = if x == 0 then S1  else S2

recognizer :: StateT 110State IO Int
recognizer = do
  input <- lift getLine -- input from the recognizer
  if (input == 'x') return $ get
  modify (stateFunction input)
  (v, s) <- get
  lift $ machineFunction s
  recognizer

recognizerMain :: IO Unit
recognizerMain = execStateT recognizer S1
