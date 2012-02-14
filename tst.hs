data Name   = NameConstr String
data Color  = ColorConstr String

showInfos :: Name ->   Color ->  String
showInfos (NameConstr name) (ColorConstr color) =
      "Name: " ++ name ++ ", Color: " ++ color

main = let
  name  = NameConstr "Robin"
  color = ColorConstr "Blue... No Red!"
  in putStrLn $ showInfos color name
