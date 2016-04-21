{-# LANGUAGE OverloadedStrings #-}

import Item

import qualified Item.Base.Prelude

import Data.Map (Map)

import qualified Data.Map as M

items :: Map PackageName (Map ModuleName [Item])
items = M.fromList
  [ ("base", M.fromList
    [ ("Prelude", Item.Base.Prelude.items) ])
  ]

main :: IO ()
main = pure ()
