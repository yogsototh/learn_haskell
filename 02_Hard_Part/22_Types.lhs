
But it doesn't protect you much.
Try to swap the two parameter of `showInfos` and run the program:

<code class="haskell">
    putStrLn $ showInfos color name
</code>

It will compile and execute.
In fact you can replace Name, Color and String everywhere.
The compiler will treat them as completely identical.

Another method is to create your own types using the keyword `data`.

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

Now if you switch parameters of `showInfos`, the compiler complains!
So this is a potential mistake you will never make again and the only price is to be more verbose. 

Also notice that constructors are functions:

<code class="haskell">
NameConstr  :: String -> Name
ColorConstr :: String -> Color
</code>

The syntax of `data` is mainly:

<code class="haskell">
data TypeName =   ConstructorName  [types]
                | ConstructorName2 [types]
                | ...
</code>

Generally the usage is to use the same name for the
DataTypeName and DataTypeConstructor.

Example:

<code class="haskell">
data Complex = Num a => Complex a a
</code>

Also you can use the record syntax:

<code class="haskell">
data DataTypeName = DataConstructor {
                      field1 :: [type of field1]
                    , field2 :: [type of field2]
                    ...
                    , fieldn :: [type of fieldn] }
</code>

And many accessors are made for you.
Furthermore you can use another order when setting values.

Example:

<code class="haskell">
data Complex = Num a => Complex { real :: a, img :: a}
c = Complex 1.0 2.0
z = Complex { real = 3, img = 4 }
real c ⇒ 1.0
img z ⇒ 4
</code>
