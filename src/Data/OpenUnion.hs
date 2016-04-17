{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Data.OpenUnion where

-- import Protolude

data Union :: [*] -> * where
  Union :: Index x xs -> x -> Union xs

data Index :: * -> [*] -> * where
  IZ :: Index x (x ': xs)
  IS :: Index x xs -> Index x (y ': xs)

pattern I1 = IZ
pattern I2 = IS I1
pattern I3 = IS I2
pattern I4 = IS I3
pattern I5 = IS I4

class x :< xs where
  index :: Index x xs

instance {-# OVERLAPS #-} x :< (x ': xs) where
  index = IZ

instance {-# OVERLAPS #-} (x :< xs) => x :< (y ': xs) where
  index = IS index

-- | Inject into union.
inj :: (x :< xs) => x -> Union xs
inj x = Union index x

-- | Project from union, using index to refine type of element.
prj :: Union xs -> (forall x. Index x xs -> x -> r) -> r
prj (Union i x) k = k i x
