module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import Text.Megaparsec
import Text.Megaparsec.Error
import Lexer
import Syntax

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            myInput <- getInputLine "% "
            case myInput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> liftIO (parseTest dParser input) >> loop


{- process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO (process input) >> loop -}