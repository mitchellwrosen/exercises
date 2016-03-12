{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Item where

import Data.Monoid
import Data.Proxy
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
  , itemImports    :: [Import]
  , itemType       :: Proxy a -- is this necessary?
  , itemCheck      :: a -> IO Bool
  }

instance Eq Item where
  Item x y z _ _ _ == Item x' y' z' _ _ _ = x == x' && y == y' && z == z'

instance Ord Item where
  compare (Item x y z _ _ _) (Item x' y' z' _ _ _) =
    compare x x' <> compare y y' <> compare z z'


importsToText :: [Import] -> Text
importsToText =
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


allItems :: [Item]
allItems =
  [ appendItem
  , reverseItem
  ]


appendItem :: Item
appendItem =
  Item
  "base"
  "Prelude"
  "++"
  []
  (Proxy :: Proxy ([Int] -> [Int] -> [Int]))
  (qcCheck2 (++))

reverseItem :: Item
reverseItem =
  Item
  "base"
  "Prelude"
  "reverse"
  [("Prelude", ["(++)"])]
  (Proxy :: Proxy ([Int] -> [Int]))
  (qcCheck1 reverse)
