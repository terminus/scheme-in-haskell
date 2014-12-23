module Main where
import Control.Monad
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import qualified Text.Read as R
       

main2 :: IO ()
main2 = do putStrLn "What's your name buddy? "
           args <- getLine
           putStrLn ("Hello, " ++ show (f args 0) ++ show (f args 1))
           where f a w = ((+ 0). read) a

main :: IO ()
main = do putStrLn "What's your name buddy? "
          args <- getLine
          putStrLn $ case args of
                   "a" -> "This"
                   "b" -> "That"
                   otherwise -> "XX"

 -- + (R.read $ args !! 1)))

-- main :: IO ()
-- main = do args <- getArgs
--             putStrLn ("Hello, " ++ args !! 0)

-- putStrLn $ "Hello, " ++ show value
