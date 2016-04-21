{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Item

import qualified Item.Base.Prelude as IBasePrelude

import Data.Char                (isAlpha)
import Data.List                (unlines)
import Data.Monoid
import Data.Text                (Text)
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

    runItem tempModulesDir item body >>= print

prettyPrintItemName :: FunctionName -> Text
prettyPrintItemName name
  | isAlpha (T.head name) = name
  | otherwise = "(" <> name <> ")"
