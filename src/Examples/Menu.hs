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
