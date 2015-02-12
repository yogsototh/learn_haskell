en: <h4 id="type-construction">Type construction</h4>
fr: <h4 id="type-construction">Construction de types</h4>

en: You can construct your own types.
en: First, you can use aliases or type synonyms.
fr: Vous pouvez construire vos propres types.
fr: D'abord, vous pouvez utiliser des alias ou des synonymes de types.

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
