{-# LANGUAGE OverloadedStrings #-}

module Item.Base.Prelude where

import Protolude

import Item

import Test.HUnit ((~?=))

items :: [Item]
items =
  [ Item
      "base"
      "Prelude"
      "&&"
      "Bool -> Bool -> Bool"
      (undefined :: Bool -> Bool -> Bool)
      [ImportHiding "Prelude" ["(&&)"]]
      (qcCheck2 (&&))

  , Item
      "base"
      "Prelude"
      "||"
      "Bool -> Bool -> Bool"
      (undefined :: Bool -> Bool -> Bool)
      [ImportHiding "Prelude" ["(||)"]]
      (qcCheck2 (||))

  , Item
      "base"
      "Prelude"
      "not"
      "Bool -> Bool"
      (undefined :: Bool -> Bool)
      [ImportHiding "Prelude" ["not"]]
      (qcCheck1 not)

  , Item
      "base"
      "Prelude"
      "maybe" "b -> (a -> b) -> Maybe a -> b"
      (undefined :: Int -> (Int -> Int) -> Maybe Int -> Int)
      [ImportHiding "Prelude" ["maybe"]]
      (\f -> hunitCheck
        [ f 5 (+1) Nothing  ~?= 5
        , f 5 (+1) (Just 6) ~?= 7
        ])

  , Item
      "base"
      "Prelude"
      "either" "(a -> c) -> (b -> c) -> Either a b -> c"
      (undefined :: (Int -> Int) -> (Int -> Int) -> Either Int Int -> Int)
      [ImportHiding "Prelude" ["either"]]
      (\f -> hunitCheck
        [ f (+1) (+2) (Left 5)  ~?= 6
        , f (+1) (+2) (Right 6) ~?= 8
        ])

  , Item
      "base"
      "Prelude"
      "++"
      "[a] -> [a] -> [a]"
      (undefined :: [Int] -> [Int] -> [Int])
      [ImportHiding "Prelude" ["(++)"]]
      (qcCheck2 (++))

  , Item
      "base"
      "Prelude"
      "reverse" "[a] -> [a]"
      (undefined :: [Int] -> [Int])
      [ImportHiding "Prelude" ["reverse"]]
      (qcCheck1 reverse)
  ]
