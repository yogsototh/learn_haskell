### Types

In Haskell, types are strong and static.

Why is this important? It will help you _a lot_ not to make some mistake.
In fact, most bugs are catched during the compilation of your program.
And the main reason is because of the type inference during compilation.
It will be easy to detect where you used the bad parameter at the wrong place for example.

#### Type inference

Static typing is generally essential to reach fast execution time.
But in common languages static typing has the price of bad generalization.
What saves Haskell is that types can be inferred.

Here are some examples on how to simulate a simple square function in Haskell
in other statically typed languages:

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
