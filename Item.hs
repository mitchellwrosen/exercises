{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Item where

import Data.Monoid
import Data.Text       (Text)
import Data.Typeable
import Test.QuickCheck

import qualified Data.Text as T


type PackageName  = Text
type ModuleName   = Text
type FunctionName = Text

type Import = (PackageName, [FunctionName])


data Item = forall a. Typeable a => Item
  { itemPackage    :: PackageName
  , itemModule     :: ModuleName
  , itemName       :: FunctionName
  , itemType       :: Text
  , itemFunc       :: a
  , itemCheck      :: a -> IO Bool
  , itemImports    :: [Import]
  }

instance Eq Item where
  Item x y z _ _ _ _ == Item x' y' z' _ _ _ _ = x == x' && y == y' && z == z'

instance Ord Item where
  compare (Item x y z _ _ _ _) (Item x' y' z' _ _ _ _) =
    compare x x' <> compare y y' <> compare z z'


itemImportsToText :: [Import] -> Text
itemImportsToText =
  T.unlines . map (\(m, fs) -> "import " <> m <> "(" <> T.intercalate "," fs <> ")")


qcCheck1
  :: (Show a, Arbitrary a, Eq b)
  => (a -> b)
  -> (a -> b)
  -> IO Bool
qcCheck1 f g = do
  result <- quickCheckResult (\a -> f a == g a)
  case result of
    Success _ _ _ -> pure True
    _ -> pure False

qcCheck2
  :: (Show a, Show b, Arbitrary a, Arbitrary b, Eq c)
  => (a -> b -> c)
  -> (a -> b -> c)
  -> IO Bool
qcCheck2 f g = do
  result <- quickCheckResult (\a b -> f a b == g a b)
  case result of
    Success _ _ _ -> pure True
    _ -> pure False


asListInt1 :: (forall a. ([a] -> [a])) -> [Int] -> [Int]
asListInt1 = id

asListInt2 :: (forall a. ([a] -> [a] -> [a])) -> [Int] -> [Int] -> [Int]
asListInt2 = id


allItems :: [Item]
allItems =
  [ Item "base" "Prelude" "&&" "Bool -> Bool -> Bool" (&&) (qcCheck2 (&&)) [("Prelude", ["Bool(..)"])]
  , Item "base" "Prelude" "||" "Bool -> Bool -> Bool" (||) (qcCheck2 (||)) [("Prelude", ["Bool(..)"])]
  , Item "base" "Prelude" "not" "Bool -> Bool" not (qcCheck1 not) [("Prelude", ["Bool(..)"])]
  , Item "base" "Prelude" "maybe" "b -> (a -> b) -> Maybe a -> b" maybe
      (\f -> pure (and [ f 5 (+1) Nothing == (5::Int)
                       , f 5 (+1) (Just 6) == 7
                       ])) [("Prelude", ["Maybe(..)"])]
  , Item "base" "Prelude" "either" "(a -> c) -> (b -> c) -> Either a b -> c" either
      (\f -> pure (and [ f (+1) (+2) (Left 5) == (6::Int)
                       , f (+1) (+2) (Right 6) == 8
                       ])) []
  , Item "base" "Prelude" "++" "[a] -> [a] -> [a]" (++) (qcCheck2 (asListInt2 (++))) []
  , Item "base" "Prelude" "reverse" "[a] -> [a]" reverse (qcCheck1 (asListInt1 reverse)) [("Prelude", ["(++)"])]
  ]
