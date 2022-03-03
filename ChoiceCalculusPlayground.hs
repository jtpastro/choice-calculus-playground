import Data.Maybe

type Dim = String

type Tag = String

type Decision = [(Dim, Tag)]

data V a
  = Obj a
  | Dim Dim [Tag] (V a)
  | Chc Dim [V a]
  deriving (Show)

instance Eq (V a) where
  _ == _ = True

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

atomic :: Dim -> [Tag] -> [V a] -> V a
atomic d ts cs = Dim d ts $ Chc d cs

semantics :: V a -> [(Decision, V a)]
semantics (Obj v) = [([], Obj v)]
semantics (Dim dim tags v) =
  [ ((dim, elemAt i tags) : d, p)
    | i <- [1 .. length tags],
      (d, p) <- semantics (choiceElimination dim i v)
  ]

choiceElimination :: Dim -> Int -> V a -> V a
choiceElimination _ _ (Obj v) = Obj v
choiceElimination dim i (Dim dim' tags v)
  | dim == dim' = Dim dim' tags v
  | otherwise = Dim dim' tags (choiceElimination dim i v)
choiceElimination dim i (Chc dim' vs)
  | dim == dim' = choiceElimination dim i (elemAt i vs)
  | otherwise = Chc dim' (map (choiceElimination dim i) vs)

tagSelection :: Tag -> Dim -> V a -> V a
tagSelection tag dim v = choiceElimination dim i v'
  where
    (tags, v') = fromJust (find dim v)
    i = position tag tags

find :: Dim -> V a -> Maybe ([Tag], V a)
find _ (Obj v) = Nothing
find dim (Dim dim' tags v)
  | dim == dim' = Just (tags, v)
  | otherwise = find dim v
find dim (Chc dim' vs) =
  head
    ( dropWhile
        (== Nothing)
        (map (find dim) vs)
    )

derivation :: Decision -> V a -> V a
derivation decision v = foldl (\pv dimTag -> tagSelection (snd dimTag) (fst dimTag) pv) v decision

elemAt :: (Num t, Ord t) => t -> [p] -> p
elemAt n (a : as)
  | n == 1 = a
  | n > 1 = elemAt (n -1) as

position :: (Eq t, Num p) => t -> [t] -> p
position elem (a : as)
  | elem == a = 1
  | otherwise = 1 + position elem as

-- Listas variacionais

type VList a = V (List a)

data List a
  = Cons a (List a)
  | Empty
  | VList (VList a)

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
  where
    liftV :: (a -> V b) -> V a -> V b
    liftV = (=<<)

-- Tipos de Menu

data Food = Steak | Pasta | Fries | Cake

type Menu = VList Food

dessert :: Menu
dessert = opt "Dessert" Cake

menu :: Menu
menu = alt "Main" [meat, pasta]

meat :: Tagged (List Food)
meat = "meat" <: vlist [Steak, Fries]

pasta :: Tagged (List Food)
pasta = "pasta" <: Pasta `vcons` dessert
