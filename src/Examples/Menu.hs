{-# LANGUAGE DeriveDataTypeable #-}
module Examples.Menu
  ( Food,
    Menu,
    dessert,
    menu,
    meat,
    pasta,
  )
where

import Examples.VList (List, Tagged, VList, alt, opt, vcons, vlist, (<:))
import Data.Generics (Data)

data Food = Steak | Pasta | Fries | Cake deriving (Data, Show)

type Menu = VList Food

dessert :: Menu
dessert = opt "Dessert" Cake

menu :: Menu
menu = alt "Main" [meat, pasta]

meat :: Tagged (List Food)
meat = "meat" <: vlist [Steak, Fries]

pasta :: Tagged (List Food)
pasta = "pasta" <: Pasta `vcons` dessert
