{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Item

import Control.Monad
import Data.Acquire                 (mkAcquire, with)
import Data.Monoid
import Data.Text                    (Text)
import Language.Haskell.Interpreter hiding (ModuleName)
import System.Console.Haskeline
import System.Directory             (removeFile)
import System.FilePath.Posix
import System.IO
import System.Random.Shuffle

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = shuffleM allItems >>= mapM_ runItem


runItem :: Item -> IO ()
runItem item@(Item pkg_name mod_name func_name func_type func check imports) = do
  T.putStrLn ("[" <> pkg_name <> "] [" <> mod_name <> "]")
  T.putStrLn (func_name <> " :: " <> func_type)

  body <-
    let go acc = do
          getInputLine "" >>= \case
            Nothing -> pure (T.pack (unlines (reverse acc)))
            Just xs -> go (xs:acc)
    in runInputT defaultSettings (go [])

  again <-
    withTempModule "temp" "M" imports body $ \mname -> do
      result <- runInterpreter $ do
        set [searchPath := ["temp"]]
        loadModules [mname]
        setImportsQ [("Prelude", Nothing), (mname, Just "M")]
        interpret ("(M." ++ T.unpack func_name ++ ")") func

      case result of
        Left err -> do
          print err
          pure True

        Right f -> do
          correct <- check f
          if correct
             then pure False
             else pure True

  when again (runItem item)

withTempModule :: FilePath -> FilePath -> [Import] -> Text -> (FilePath -> IO a) -> IO a
withTempModule dir filename imports body =
  with
    (mkAcquire (writeModule dir (filename <> ".hs") imports body)
    (\path -> removeFile (dir </> path <.> "hs")))

writeModule :: FilePath -> FilePath -> [Import] -> Text -> IO FilePath
writeModule dir filename imports body =
  with (mkAcquire (openTempFile dir filename) (hClose . snd)) $ \(path, h) -> do
    let mname = takeBaseName path

    T.hPutStrLn h $ T.unlines
      [ "{-# LANGUAGE NoImplicitPrelude #-}"
      , "{-# LANGUAGE Safe #-}"
      , "module " <> T.pack mname <> " where"
      , itemImportsToText imports
      , "data " <> T.pack mname -- this prevents imports in the body
      , body
      ]

    pure mname
