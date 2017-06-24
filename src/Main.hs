module Main
( main
) where

import Control.Monad.Except
import Control.Monad.RWS.Strict
import System.IO
import System.Random

import qualified Data.Text as T

import Machine

-- | Ditch an unhandled exception in an ExceptT. We don't have to worry about
-- safety since we're guaranteed to have no unhandled exceptions in this phase.
unsafeUnwrapExcept :: Monad m => ExceptT e m a -> m a
unsafeUnwrapExcept = fmap discard . runExceptT
    where discard = either (const $ error "[FATAL] Unhandled exception") id 

-- | Given a starting state, process a query, then repeat if still running.
process :: Stack -> Int -> StdGen -> Bool -> IO ()
process _     _    _   False = return ()
process stack size rng True  = do
    eof <- isEOF
    if eof
    then return ()
    else do
        line <- fmap T.pack getLine
        let (running, state', output) = (runRWS . unsafeUnwrapExcept)
                                        (parseAndExecuteQuery line)
                                        ()
                                        (stack, size, rng)
        let (stack', size', rng') = state'
        mapM_ (putStrLn . T.unpack) output >> process stack' size' rng' running

-- | Main function.
main :: IO ()
main = do
    putStrLn "? for help"
    rng <- getStdGen
    process [] 0 rng True
