module CC.Edit where

import CC.ChoiceCalculus (Dim, V (Chc, Dim, Obj))
import CC.Zipper (Z, match)
import Data.Generics.Zipper (Zipper, fromZipper, getHole, setHole, toZipper)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Generics.SYB (Data, Typeable)
import Generics.SYB.Basics (cast, gmapQ)

type C a = Z a

type Locator a = V a -> Maybe (C a)

type Splitter a = V a -> Maybe (C a, V a)

type Trans a = V a -> V a

type Pred a = V a -> Bool

apply :: Data a => C a -> V a -> V a
apply c h = fromZipper (setHole h c)

infixr 9 <@

(<@) :: Data a => C a -> V a -> V a
(<@) = apply

find :: (Data a, Typeable a) => (V a -> Bool) -> V a -> Maybe (C a)
find p v = match p (toZipper v)

dimDef :: Dim -> Pred a
dimDef d (Dim d' _ _) = d == d'
dimDef _ _ = False

extract :: Data a => Pred a -> Splitter a
extract p e = do
  c <- find p e
  h <- getHole c
  return (c, h)

hoist :: Data a => Dim -> V a -> V a
hoist d e = fromMaybe e $ do
  (c, Dim _ ts e') <- extract (dimDef d) e
  return (Dim d ts (c <@ e'))

safeHoist :: Data a => Dim -> V a -> V a
safeHoist d e = fromMaybe e $ do
  (c, Dim _ ts e') <- extract (dimDef d) e
  if d `Set.member` freeDims e
    then Nothing
    else return (Dim d ts (c <@ e'))

chcFor :: Dim -> Pred a
chcFor d (Chc d' _) = d == d'
chcFor _ _ = False

prioritize :: Data a => Dim -> Dim -> V a -> V a
prioritize b a e = fromMaybe e $ do
  (dA, ae) <- extract (dimDef a) e
  (cA, Chc _ [a1, a2]) <- extract (chcFor a) ae
  (cB, Chc _ [b1, b2]) <- extract (chcFor b) a2
  return $ dA <@ Chc b [cB <@ b1, cA <@ Chc a [a1, cB <@ b2]]

freeDims :: Data a => V a -> Set.Set Dim
freeDims (Dim d _ e) = d `Set.delete` freeDims e
freeDims (Chc d es) = d `Set.insert` Set.unions (map freeDims es)
freeDims e = Set.unions (ccQ freeDims e)

gccQ :: (Typeable a, Data b) => (V a -> r) -> b -> [r]
gccQ f b = case cast b of
  Nothing -> concat (gmapQ (gccQ f) b)
  Just va -> [f va]

ccQ :: Data a => (V a -> r) -> V a -> [r]
ccQ f (Obj a) = gccQ f a
ccQ f (Dim _ _ e) = [f e]
ccQ f (Chc _ es) = map f es