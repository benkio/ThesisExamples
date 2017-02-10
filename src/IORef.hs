module IORef where

import Data.IORef

ioRefMain :: IO ()
ioRefMain = do
    -- create a new IORef
    stringRef <- newIORef $ ""
    string1 <- readIORef stringRef
    print string1

    -- change the value inside stringRef
    writeIORef stringRef "A"
    string2 <- readIORef stringRef
    print string2
    -- now it is apparent that the value inside of stringRef has changed
