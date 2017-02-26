
en: But it doesn't protect you much.
en: Try to swap the two parameter of `showInfos` and run the program:
fr: Mais cela ne vous protège pas tellement.
fr: Essayez d'inverser les deux paramètres de `showInfos` et lancez le programme:

<code class="haskell">
    putStrLn $ showInfos color name
</code>

en: It will compile and execute.
en: In fact you can replace Name, Color and String everywhere.
en: The compiler will treat them as completely identical.
fr: Le code sera compilé et exécuté.
fr: En fait vous pouvez remplace Name, Color et String n'importe où.
fr: Le compilateur les traitera comme si ils était complétement identiques.

en: Another method is to create your own types using the keyword `data`.
fr: Une autre méthode est de créer vos propres type avec le mot-clé `data`.

> data Name   = NameConstr String
> data Color  = ColorConstr String
>
> showInfos :: Name ->  Color -> String
> showInfos (NameConstr name) (ColorConstr color) =
>       "Name: " ++ name ++ ", Color: " ++ color
>
> name  = NameConstr "Robin"
> color = ColorConstr "Blue"
> main = putStrLn $ showInfos name color

en: Now if you switch parameters of `showInfos`, the compiler complains!
en: So this is a potential mistake you will never make again and the only price is to be more verbose.
fr: Maintenant, si vous échangez les paramètres de `showInfos`, le compilateur se plaint!
fr: Au seul prix d'être plus verbeux, vous écartez définitivement cette erreur potentielle.

en: Also notice that constructors are functions:
fr: Remarquez aussi que les constructeurs sont des fonctions :

<code class="haskell">
NameConstr  :: String -> Name
ColorConstr :: String -> Color
</code>

en: The syntax of `data` is mainly:
fr: La syntaxe de `data` est principalement:

<code class="haskell">
data TypeName =   ConstructorName  [types]
                | ConstructorName2 [types]
                | ...
</code>

en: Generally the usage is to use the same name for the
en: DataTypeName and DataTypeConstructor.
fr: Généralement on utilise le même nom pour le DatatTypeName et le DataTypeConstructor.

en: Example:
fr: Exemple :

<code class="haskell">
data Complex a = Num a => Complex a a
</code>

en: Also you can use the record syntax:
fr: Vous pouvez également utiliser cette syntaxe :

<code class="haskell">
data DataTypeName = DataConstructor {
                      field1 :: [type of field1]
                    , field2 :: [type of field2]
                    ...
                    , fieldn :: [type of fieldn] }
</code>

en: And many accessors are made for you.
en: Furthermore you can use another order when setting values.
fr: Et de nombreux accesseurs sont définis pour vous.
fr: En outre, vous pouvez utiliser une autre ordre lorsque vous définissez des valeurs.

en: Example:
fr: Exemple :

<code class="haskell">
data Complex a = Num a => Complex { real :: a, img :: a}
c = Complex 1.0 2.0
z = Complex { real = 3, img = 4 }
real c ⇒ 1.0
img z ⇒ 4
</code>
