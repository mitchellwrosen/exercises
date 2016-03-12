{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Item

import Data.Monoid
import Data.Proxy
import Language.Haskell.Interpreter hiding (ModuleName)
import System.Console.Haskeline
import System.FilePath.Posix
import System.IO
import System.Random.Shuffle

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = shuffleM allItems >>= mapM_ runItem


runItem :: Item -> IO ()
runItem item@(Item pkg_name mod_name func_name imports (_ :: Proxy t) check) = do
  T.putStrLn (pkg_name <> ":" <> mod_name <> "." <> func_name)

  body <-
    let go acc = do
          getInputLine "" >>= \case
            Nothing -> pure (T.pack (unlines (reverse acc)))
            Just xs -> go (xs:acc)
    in runInputT defaultSettings (go [])

  (path, h) <- openTempFile "temp" "M.hs"
  let mname = takeBaseName path

  T.hPutStrLn h $ T.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE Safe #-}"
    , "module " <> T.pack mname <> " where"
    , importsToText imports
    , "data " <> T.pack mname -- this prevents imports in the body
    , body
    ]
  hClose h

  result <- runInterpreter $ do
    set [searchPath := ["temp"]]
    loadModules [mname]
    setImportsQ [("Prelude", Nothing), (mname, Just "M")]
    interpret ("(M." ++ T.unpack func_name ++ ")") (as :: t)

  case result of
    Left err -> do
      print err
      runItem item

    Right f -> do
      correct <- check f
      if correct
         then pure ()
         else runItem item
