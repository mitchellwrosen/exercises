{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module Item where

import Protolude hiding ((<.>))

import Data.Acquire                 (mkAcquire, with)
import Data.Typeable
import Language.Haskell.Interpreter hiding (ModuleName)
import System.Directory             (removeFile)
import System.FilePath.Posix
import System.IO
import Test.QuickCheck

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Test.HUnit      as HUnit


type PackageName  = Text
type ModuleName   = Text
type FunctionName = Text

data Import
  = Import PackageName [FunctionName]
  | ImportHiding PackageName [FunctionName]


-- An Item is a function paired with metadata, including a test suite for
-- comparing a user's implementation.
data Item = forall a. Typeable a => Item
  { itemPackage    :: PackageName
  , itemModule     :: ModuleName
  , itemName       :: FunctionName
  , itemType       :: Text
  , itemWitness    :: a -- always undefined
  , itemImports    :: [Import]
  , itemCheck      :: a -> IO ItemRunResult
  }

instance Eq Item where
  Item x y z _ _ _ _ == Item x' y' z' _ _ _ _ =
    x == x' && y == y' && z == z'

instance Ord Item where
  compare (Item x y z _ _ _ _) (Item x' y' z' _ _ _ _) =
    compare x x' <> compare y y' <> compare z z'


-- The result of running an item.
data ItemRunResult
  = RunInterpreterError InterpreterError
  | RunException SomeException
  | RunFailure Text
  | RunSuccess
  deriving Show


runItem :: FilePath -> Item -> Text -> IO ItemRunResult
runItem temp_dir Item{..} body = do
  withTempModule $ \mname -> do
    result <- runInterpreter $ do
      set [searchPath := [temp_dir]]
      loadModules [T.unpack mname]
      setImportsQ [(T.unpack mname, Just "M")]
      interpret ("(M." ++ T.unpack itemName ++ ")") itemWitness

    case result of
      Left err -> pure (RunInterpreterError err)
      Right f -> itemCheck f
 where
  -- Run an action given a temporary file containing Haskell code. The file is
  -- removed after the action completes.
  withTempModule :: (ModuleName -> IO a) -> IO a
  withTempModule =
    let
      acquire :: IO ModuleName
      acquire = writeModule

      release :: ModuleName -> IO ()
      release mname = removeFile (temp_dir </> T.unpack mname <.> "hs")
    in
      with (mkAcquire acquire release)

  -- Write Haskell code to disk at
  --
  --    <temp-dir> / Mxxx.hs
  --
  -- and return the module name
  --
  --    Mxxx
  --
  -- where xxx is random bytes.
  --
  writeModule :: IO ModuleName
  writeModule =
    let
      acquire :: IO (FilePath, Handle)
      acquire = openTempFile temp_dir "M.hs"

      release :: (FilePath, Handle) -> IO ()
      release = hClose . snd
    in
      with (mkAcquire acquire release) $ \(path, h) -> do
        let mname = T.pack (takeBaseName path)

        T.hPutStrLn h $ T.unlines
          [ "{-# LANGUAGE NoImplicitPrelude #-}"
          , "{-# LANGUAGE Safe #-}"
          , "module " <> mname <> " where"
          , itemImportsToText itemImports
          , "data " <> mname -- this prevents imports in the body
          , itemName <> " :: " <> itemType
          , body
          ]

        pure mname


itemImportsToText :: [Import] -> Text
itemImportsToText = T.unlines . fmap (\case
  Import       m fs -> "import " <> m <>        " (" <> T.intercalate "," fs <> ")"
  ImportHiding m fs -> "import " <> m <> " hiding (" <> T.intercalate "," fs <> ")")


qcCheck1
  :: (Show a, Arbitrary a, Eq b)
  => (a -> b)
  -> (a -> b)
  -> IO ItemRunResult
qcCheck1 f g =
  quickCheckWithResult qcArgs (\a -> f a == g a) >>= \case
    Success _ _ _ -> pure RunSuccess
    Failure _ _ _ _ _ _ _ (Just ex) _ _ -> pure (RunException ex)
    result -> pure (RunFailure (T.pack (output result)))

qcCheck2
  :: (Show a, Show b, Arbitrary a, Arbitrary b, Eq c)
  => (a -> b -> c)
  -> (a -> b -> c)
  -> IO ItemRunResult
qcCheck2 f g =
  quickCheckWithResult qcArgs (\a b -> f a b == g a b) >>= \case
    Success _ _ _ -> pure RunSuccess
    Failure _ _ _ _ _ _ _ (Just ex) _ _ -> pure (RunException ex)
    result -> pure (RunFailure (T.pack (output result)))

qcArgs :: Args
qcArgs = Args
  { replay = Nothing
  , maxSuccess = 100
  , maxDiscardRatio = 10
  , maxSize = 100
  , chatty = False
  }


hunitCheck :: [HUnit.Test] -> IO ItemRunResult
hunitCheck tests = do
  (counts, f) <- HUnit.runTestText HUnit.putTextToShowS (HUnit.TestList tests)
  if | HUnit.errors counts /= 0   -> pure (RunFailure (T.pack (f "")))
     | HUnit.failures counts /= 0 -> pure (RunFailure (T.pack (f "")))
     | otherwise                  -> pure RunSuccess
