module CC.Zipper where

import CC.ChoiceCalculus (V (Chc, Dim, Obj))
import Data.Generics.Zipper (Zipper, down, down', left, query, right, up)
import Data.Maybe (isNothing)
import Generics.SYB (Data, Typeable, mkQ)

type Z a = Zipper (V a)

type Move a = Z a -> Maybe (Z a)

isCC, isObj, isDim, isChc :: V a -> Bool
isCC _ = True
isObj (Obj _) = True
isObj _ = False
isDim Dim {} = True
isDim _ = False
isChc (Chc _ _) = True
isChc _ = False

-- Are we at a location that satisfies the query?
atX :: Typeable a => (V a -> Bool) -> Z a -> Bool
atX f = query (mkQ False f)

-- Are we at a node of the corresponding syntactic category.
atCC, atObj, atDim, atChc :: Typeable a => Z a -> Bool
atCC = atX isCC
atObj = atX isObj
atDim = atX isDim
atChc = atX isChc

-- Are we at the top/bottom/leftEnd/rightEnd of the expression?
atTop, atBottom, atLeftEnd, atRightEnd :: Typeable a => Z a -> Bool
atTop = isNothing . up
atBottom = isNothing . down
atLeftEnd = isNothing . left
atRightEnd = isNothing . right

match :: Typeable a => (V a -> Bool) -> Move a
match f z
  | atX f z = Just z
  | otherwise = case down' z >>= match f of
    Nothing -> tryRight
    success -> success
  where
    tryRight
      | atRightEnd z = Nothing
      | otherwise = right z >>= match f
