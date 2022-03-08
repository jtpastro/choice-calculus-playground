module ChoiceCalculus (Dim, Tag, Decision, V (Obj, Dim, Chc), liftV, atomic, semantics, choiceElimination, tagSelection) where

import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe, isNothing)

type Dim = String

type Tag = String

type Decision = [(Dim, Tag)]

data V a
  = Obj a
  | Dim Dim [Tag] (V a)
  | Chc Dim [V a]

instance Show a => Show (V a) where
  show (Obj x) = show x
  show (Dim d ts v) =
    "Dim " <> d <> "<" <> intercalate ", " ts <> "> in " <> show v
  show (Chc d vs) =
    d <> "<" <> intercalate ", " (map show vs) <> ">"

instance Functor V where
  fmap f (Obj v) = Obj (f v)
  fmap f (Dim d ts v) = Dim d ts (fmap f v)
  fmap f (Chc d vs) = Chc d (map (fmap f) vs)

instance Applicative V where
  pure = Obj
  mf <*> mx = mf `bindV` (\f -> mx `bindV` (pure . f))
    where
      Obj a `bindV` f = f a
      Dim d t v `bindV` f = Dim d t (v >>= f)
      Chc d vs `bindV` f = Chc d (map (>>= f) vs)

instance Monad V where
  return = Obj
  Obj a >>= f = f a
  Dim d t v >>= f = Dim d t (v >>= f)
  Chc d vs >>= f = Chc d (map (>>= f) vs)

liftV :: (a -> V b) -> V a -> V b
liftV = (=<<)

atomic :: Dim -> [Tag] -> [V a] -> V a
atomic d ts cs = Dim d ts $ Chc d cs

semantics :: V a -> [(Decision, V a)]
semantics (Obj v) = [([], Obj v)]
semantics (Dim dim tags v) =
  [ ((dim, fromJust $ elemAt i tags) : d, p)
    | i <- [1 .. length tags],
      (d, p) <- semantics (choiceElimination dim i v)
  ]
semantics (Chc dim cs) = map (\p -> (decisions p, Chc dim (sesPlain p))) semSes
  where
    semSes = nCP (map semantics cs)
    decisions = concatMap fst
    sesPlain = concatMap (\p -> [snd p])

choiceElimination :: Dim -> Int -> V a -> V a
choiceElimination _ _ (Obj v) = Obj v
choiceElimination dim i (Dim dim' tags v)
  | dim == dim' = Dim dim' tags v
  | otherwise = Dim dim' tags (choiceElimination dim i v)
choiceElimination dim i (Chc dim' vs)
  | dim == dim' = choiceElimination dim i (fromJust (elemAt i vs))
  | otherwise = Chc dim' (map (choiceElimination dim i) vs)

tagSelection :: Eq a => Tag -> Dim -> V a -> V a
tagSelection tag dim v = choiceElimination dim (fromJust i) v'
  where
    (tags, v') = fromJust (find dim v)
    i = position tag tags

find :: Eq a => Dim -> V a -> Maybe ([Tag], V a)
find _ (Obj v) = Nothing
find dim (Dim dim' tags v)
  | dim == dim' = Just (tags, v)
  | otherwise = find dim v
find dim (Chc dim' vs) =
  head
    ( dropWhile
        isNothing
        (map (find dim) vs)
    )

derivation :: Eq a => Decision -> V a -> V a
derivation decision v = foldl (\pv dimTag -> tagSelection (snd dimTag) (fst dimTag) pv) v decision

elemAt :: (Num t, Ord t) => t -> [p] -> Maybe p
elemAt 0 _ = Nothing
elemAt _ [] = Nothing
elemAt n (a : as)
  | n == 1 = Just a
  | n > 1 = elemAt (n -1) as
  | otherwise = Nothing

position :: (Eq t, Num p) => t -> [t] -> Maybe p
position _ [] = Nothing
position elem (a : as)
  | elem == a = Just 1
  | otherwise = (+ 1) <$> position elem as

nCP :: [[a]] -> [[a]]
nCP [] = []
nCP [e1] = [e1]
nCP [e1, e2] = [[a, b] | a <- e1, b <- e2]
nCP (e1 : e2 : e3 : es) = [a : b | a <- e1, b <- nCP (e2 : e3 : es)]