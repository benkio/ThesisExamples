module ReaderMonad where

-- Idea Taken from the https://gist.github.com/egonSchiele/5752172

import Control.Monad.Reader

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

hello2 :: Reader String String
hello2 = asks $ \name -> ("hello, " ++ name ++ "!")


bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

bye2 :: Reader String String
bye2 = asks $ \name -> ("bye, " ++ name ++ "!")


convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2

convo2 = hello >>= \h -> (bye >>= \b -> return $ h ++ b) -- Using the bind
convo3 = hello >>= \h -> (\b -> h ++ b) <$> bye          -- Using the fmap
convo4 = asks (const (++)) <*> hello <*> bye             -- Using the apply

readerMain = print . runReader convo $ "adit"
