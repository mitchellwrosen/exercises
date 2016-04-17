{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Protolude

import Data.OpenUnion
import Item

import qualified Item.Base.Prelude as IBasePrelude

import Data.Char (isAlpha)
import Data.List (unlines)
import Prelude (FilePath)
import System.Console.Haskeline
import System.Random.Shuffle

import qualified Data.Text    as T
import qualified Data.Text.IO as T

tempModulesDir :: FilePath
tempModulesDir = "temp"

allItems :: [Item]
allItems = IBasePrelude.items

main :: IO ()
main = shuffleM allItems >>= mapM_ f
 where
  f :: Item -> IO ()
  f item = do
    T.putStrLn ("[" <> itemPackage item <> "] [" <> itemModule item <> "]")
    T.putStrLn (prettyPrintItemName (itemName item) <> " :: " <> itemType item)

    body <-
      let go acc = do
            getInputLine "" >>= \case
              Nothing -> pure (T.pack (unlines (reverse acc)))
              Just xs -> go (xs:acc)
      in runInputT defaultSettings (go [])

    result <- runItem tempModulesDir item body
    case result of
      Union I1 x -> print x
      Union I2 x -> print x
      Union I3 x -> print x
      Union I4 x -> print x
      Union I5 x -> print x

prettyPrintItemName :: FunctionName -> Text
prettyPrintItemName name
  | isAlpha (T.head name) = name
  | otherwise = "(" <> name <> ")"
