{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Item where

import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import Test.QuickCheck

import qualified Data.Text as T


data Item = forall a. Typeable a => Item
  { itemName       :: Text
  , itemImports    :: [(Text, [Text])]
  , itemType       :: Proxy a
  , itemCheck      :: a -> IO Bool
  }


importsToText :: [(Text, [Text])] -> Text
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


appendItem :: Item
appendItem =
  Item
  "++"
  []
  (Proxy :: Proxy ([Int] -> [Int] -> [Int]))
  (qcCheck2 (++))
