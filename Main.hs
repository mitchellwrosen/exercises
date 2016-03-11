{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Item

import Data.Monoid
import Data.Text     (Text)
import Data.Typeable (Typeable)
import Data.Proxy
import Language.Haskell.Interpreter
import System.FilePath.Posix
import System.IO
import System.IO.Temp

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = go appendItem

go :: Item -> IO ()
go (Item name imports (p :: Proxy t) check is_operator) = do
  T.putStrLn name
  body <-
    let go acc = do
          T.getLine >>= \case
            "EOF" -> pure (T.unlines (reverse acc))
            xs    -> go (xs:acc)
    in go []

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
    interpret ("(M." ++ T.unpack name ++ ")") (as :: t)

  case result of
    Left err -> print err
    Right f -> do
      check f
      pure ()
