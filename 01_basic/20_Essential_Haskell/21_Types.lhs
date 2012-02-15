<h3>Type construction</h3>

You can construct you own types.
First you can use aliases or type synonyms.

> type Name   = String
> type Color  = String
>
> showInfos :: Name ->  Color -> String
> showInfos name color =  "Name: " ++ name
>                         ++ ", Color: " ++ color
> name :: Name
> name = "Robin"
> color :: Color
> color = "Blue"
> main = putStrLn $ showInfos name color
