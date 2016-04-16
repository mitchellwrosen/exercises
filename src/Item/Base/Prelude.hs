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
      "maybe"
      "b -> (a -> b) -> Maybe a -> b"
      (undefined :: Int -> (Int -> Int) -> Maybe Int -> Int)
      [ImportHiding "Prelude" ["maybe"]]
      (\f -> hunitCheck
        [ f 5 (+1) Nothing  ~?= 5
        , f 5 (+1) (Just 6) ~?= 7
        ])

  , Item
      "base"
      "Prelude"
      "either"
      "(a -> c) -> (b -> c) -> Either a b -> c"
      (undefined :: (Int -> Int) -> (Int -> Int) -> Either Int Int -> Int)
      [ImportHiding "Prelude" ["either"]]
      (\f -> forceCheck (f identity identity (Left 1))
          <> forceCheck (f identity identity (Right 1)))

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
      "reverse"
      "[a] -> [a]"
      (undefined :: [Int] -> [Int])
      [ImportHiding "Prelude" ["reverse"]]
      (qcCheck1 reverse)

  , Item
      "base"
      "Prelude"
      "fst"
      "(a, b) -> a"
      (undefined :: (Int, Int) -> Int)
      [ImportHiding "Prelude" ["fst"]]
      (\f -> forceCheck (f (1,1)))

  , Item
      "base"
      "Prelude"
      "snd"
      "(a, b) -> b"
      (undefined :: (Int, Int) -> Int)
      [ImportHiding "Prelude" ["snd"]]
      (\f -> forceCheck (f (1,1)))

  , Item
      "base"
      "Prelude"
      "curry"
      "((a, b) -> c) -> a -> b -> c"
      (undefined :: ((Int, Int) -> Int) -> Int -> Int -> Int)
      [ImportHiding "Prelude" ["curry"]]
      (\f -> forceCheck (f fst 1 2))

  , Item
      "base"
      "Prelude"
      "uncurry"
      "(a -> b -> c) -> (a, b) -> c"
      (undefined :: (Int -> Int -> Int) -> (Int, Int) -> Int)
      [ImportHiding "Prelude" ["uncurry"]]
      (\f -> forceCheck (f (+) (1,1)))

  , Item
      "base"
      "Prelude"
      "even"
      "Integral a => a -> Bool"
      (undefined :: Int -> Bool)
      [ImportHiding "Prelude" ["even", "odd"]]
      (qcCheck1 even)

  , Item
      "base"
      "Prelude"
      "gcd"
      "Integral a => a -> a -> a"
      (undefined :: Int -> Int -> Int)
      [ImportHiding "Prelude" ["gcd"]]
      (qcCheck2 gcd)

  , Item
      "base"
      "Prelude"
      "lcm"
      "Integral a => a -> a -> a"
      (undefined :: Int -> Int -> Int)
      [ImportHiding "Prelude" ["lcm"]]
      (qcCheck2 lcm)

  , Item
      "base"
      "Prelude"
      "mapM"
      "Monad m => (a -> m b) -> [a] -> m [b]"
      (undefined :: (Int -> Maybe Int) -> [Int] -> Maybe [Int])
      [ImportHiding "Prelude" ["mapM", "traverse"]]
      (\f ->
        let g = \n -> if n > 10 then Nothing else Just n
        in qcCheck1 (mapM g) (f g))
  ]
