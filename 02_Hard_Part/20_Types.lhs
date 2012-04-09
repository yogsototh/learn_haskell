<h3 id="types">Types</h3>

<%= blogimage("salvador-dali-the-madonna-of-port-lligat.jpg","Dali, the madonna of port Lligat") %>

 > <%=tldr%>
 > 
 > - `type Name = AnotherType` is just an alias and the compiler doesn't do any difference between `Name` and `AnotherType`.
 > - `data Name = NameConstructor AnotherType` make a difference.
 > - `data` can construct structures which can be recursives.
 > - `deriving` is magic and create functions for you.

In Haskell, types are strong and static.

Why is this important? It will help you _a lot_ not to make some mistake.
In Haskell, most bugs are caught during the compilation of your program.
And the main reason is because of the type inference during compilation.
It will be easy to detect where you used the bad parameter at the wrong place for example.


<h4 id="type-inference">Type inference</h4>

Static typing is generally essential to reach fast execution time.
But most static typed language are bad to generalize concepts.
What saves Haskell is that it can _infere_ types.

Here is a simple example. 
The `square` function in Haskell:

<code class="haskell">
square x = x * x
</code>

This function can `square` any Numeral type.
You can provide `square` an `Int`, an `Integer`, a `Float` a `Fractional` and even `Complex`. Proof by example:

~~~
% ghci
GHCi, version 7.0.4:
...
Prelude> let square x = x*x
Prelude> square 2
4
Prelude> square 2.1
4.41
Prelude> -- load the Data.Complex module
Prelude> :m Data.Complex
Prelude Data.Complex> square (2 :+ 1)
3.0 :+ 4.0
~~~

`x :+ y` is the notation for the complex (<i>x + ib</i>).

Now compare with the necessary C code:

<code class="c">
int     int_square(int x) { return x*x; }

float   float_square(float x) {return x*x; }

complex complex_square (complex z) {
    complex tmp; 
    tmp.real = z.real * z.real - z.img * z.img;
    tmp.img = 2 * z.img * z.real;
}

complex x,y;
y = complex_square(x);
</code>

For each type, you need to write a new function.
The only way to work around this problem is to use some meta-programming trick.
For example using the pre-processor.
In C++ there is a better way, the C++ templates[^022001]:

[^022001]: I know there is a cleaner way, to do this in C++. This is only an example.

<code class="c++">
#include <iostream>
using namespace std;

class Complex
{
public:
    double real_; 
    double img_; 
    Complex(double real, double img) : real_(real), img_(img) {} 
    // overloaded the multiplication operator
    Complex operator*(Complex z)
    {
        return Complex(real_*z.real_ - img_*z.img_,
            img_*z.real_ + real_*z.img_);
    }
};

template<typename T>
T square(T x)
{
    return x*x;
}

ostream& operator<<(ostream& os, const Complex& z) {
    os << z.real_ << " + " << z.img_ << "i";
    return os;
}

int main() {
    cout << square<int>(5) << endl;
    cout << square<double>(5.3) << endl;
    Complex z=Complex(5,3);
    cout << square<Complex>(z) << endl;
    return 0;
}
</code>

Even with this example of C++ templates you are forced to write the type on the call of the function.

To be fair, there is also a definition of the multiplication of `Complex` in Haskell.
But it takes only one line.
Somewhere in the source of the module `Data.Complex`:

<code class="haskell">
instance Num (Complex a) where
  ...
  (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
  ...
</code>

The inference of type gives Haskell a feeling of the freedom that dynamic 
typed languages provide. 
But unlike dynamic typed languages, most error are caught before the execution.
Generally, in Haskell:

 > "if it compiles it certainly does what you intended" 
