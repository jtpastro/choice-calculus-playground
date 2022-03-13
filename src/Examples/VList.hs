{-# LANGUAGE DeriveDataTypeable #-}
module Examples.VList
  ( VList,
    List,
    Tagged,
    (<:),
    opt,
    alt,
    vempty,
    list,
    vsingle,
    single,
    vcons,
    many,
    vlist,
    len,
    vlen,
  )
where

import CC.ChoiceCalculus (Dim, Tag, V (Obj), atomic, liftV)
import Data.Data

type VList a = V (List a)

data List a
  = Cons a (List a)
  | Empty
  | VList (VList a)
   deriving (Data, Show)

type Tagged a = (Tag, V a)

infixl 2 <:

(<:) :: Tag -> V a -> Tagged a
t <: v = (t, v)

opt :: Dim -> a -> VList a
opt d x = atomic d ["yes", "no"] [vsingle x, vempty]

alt :: Dim -> [Tagged a] -> V a
alt d tvs = atomic d ts vs where (ts, vs) = unzip tvs

vempty :: VList a
vempty = list Empty

list :: List a -> VList a
list = Obj

vsingle :: a -> VList a
vsingle = list . single

single :: a -> List a
single a = Cons a Empty

vcons :: a -> VList a -> VList a
vcons x = list . Cons x . VList

many :: [a] -> List a
many = foldr Cons Empty

vlist :: [a] -> VList a
vlist = list . many

len :: List a -> V Int
len Empty = return 0
len (Cons _ xs) = fmap (+ 1) (len xs)
len (VList vl) = vl >>= len

vlen :: VList a -> V Int
vlen = liftV len
