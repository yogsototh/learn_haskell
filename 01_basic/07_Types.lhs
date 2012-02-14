## Types


Types are in the heart of Haskell.
In Haskell, types are strong and static.

Why is this important? It will help you _a lot_ not to make some mistake.

### Type inference

Static typing is generally essential to reach fast execution time.
But in common languages static typing has the price of bad generalization.
What saves Haskell is that types can be inferred.

Here are some examples on how to simulate a simple square function in Haskell
in other statically typed langauges:

> square x = x * x

This function can square any Numeral type.
You can provide square an Int, an Integer, a Float a Fractional and even Complex.

> square 2
> 4
> square 2.1
> 4.41
> :m Data.Complex
> (2 :+ 1) * (2 :+ 1) 
> 3.0 :+ 4.0

`x :+ y` is the notation for the complex (<i>x + ib</i>).

Now compare to the code necessary in C:

<code class="c">
int     int_square(int x) { return x*x; }
float   fl_square(float x) {return x*x; }
complex complex_square (complex z) {
    complex tmp; 
    tmp.real = z.real * z.real - z.img * z.img;
    tmp.img = 2 * z.img * z.real;
}
</code>

For each type, you need to write a new function.
The only way to work around this problem is to use some meta-programming trick.
For example using the pre-processor.
In C++ there is a better way, the C++ templates:

<code class="c++">
class Number<T> {
    T value;
    square() {
        value = value*value;
    }
}

Number<int> i;
i.square;

Number<float> f;
f.square;

class Complex {
    int real;
    int img;
    Complex operator<*>(Complex z) {
        Complex result;
        result.real = real*z.real - img*z.img;
        result.img  = img*z.real + real*z.img;
        return res;
    }
}

Number<Complex> z;
z.square
</code>

Even with C++ templates you are forced to write a line for each type.

To be fair, there is also a definition of the multiplication of Complex in Haskell.
But it takes only one line.
Somewhere in the source of the module `Data.Complex`:

> instance Num (Complex a) where
>   ...
>   (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
>   ...

The inference of type gives Haskell a feeling of the freedom that dynamic 
typed languages provide.

### Type construction 

You can construct you own types.
First you can use aliases or type synonyms.

> type Name   = String
> type Color  = String
>                                                                                
> showInfos :: Name ->  Color -> String
> showInfos name color =  "Name: " ++ name 
>                         ++ ", Color: " ++ color
> 
> main = let
>          name = "Robin"
>          color = "Blue"
>        in
>          putStrLn $ showInfos name color

But it doesn't protect you much.
Try to replace the last line and run the program:

>   putStrLn $ showInfos color name

In fact you can replace Name, Color and String everywhere.
The compiler will treat them as completely identical.


The second type creation it to create your own types

> data Name   = NameConstr String
> data Color  = ColorConstr String
>                                                                                
> showInfos :: Name ->  Color -> String
> showInfos (NameConstr name) (ColorConstr color) =  
>       "Name: " ++ name ++ ", Color: " ++ color
> 
> main = let
>          name = NameConstr "Robin"
>          color = ColorConstr "Blue"
>        in
>          putStrLn $ showInfos name color

Now if you switch parameters of `showInfos`, the compiler complains!
A possible mistake you could never do again.
The only price for this is to write the type constructor each time you use
such a type.

The syntax of `data` is generally:

> data DataTypeName =   DataTypeConstructor [list of types] 
>                     | AnotherDataTypeConstructor [another list of type]
>                     | ...

Generally the usage is to use the same name for the 
DataTypeName and DataTypeConstructor.

Example:

> data Complex = Num a => Complex a a 

Also you can use the record syntax:

> data DataTypeName = DataConstrctor {
>                       field1 :: [type of field1]
>                     , field2 :: [type of field2]
>                     ...
>                     , fieldn :: [type of fieldn] }

And many accessor are made for you.
Furthermore you can use another order when setting values.

Example:

> data Complex = Num a => Complex { real :: a, img :: a}
> c = Complex 1.0 2.0
> z = Complex { real = 3, img = 4 }
> real c ⇒ 1.0
> img z ⇒ 4

