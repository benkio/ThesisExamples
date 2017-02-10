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
