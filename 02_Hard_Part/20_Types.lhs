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
In C++ there is a better way, the C++ templates:

<code class="c++">
#include <iostream>
#include <complex>
using namespace std;

template<typename T>
T square(T x)
{
    return x*x;
}

int main() {
    // int
    int sqr_of_five = square(5);
    cout << sqr_of_five << endl;
    // double
    cout << (double)square(5.3) << endl;
    // complex
    cout << square( complex<double>(5,3) ) 
         << endl;
    return 0;
}
</code>

C++ does a far better job than C. 
For complex problem, it become difficult to decrypt; see 
[this article](http://bartoszmilewski.com/2009/10/21/what-does-haskell-have-to-do-with-c/) for example.

In C++ you have to declare a function can work with many different types.
In Haskell this is the opposite. 
The function will be as general as possible by default.

The inference of type gives Haskell a feeling of the freedom that dynamic 
typed languages provide. 
But unlike dynamic typed languages, most error are caught before the execution.
Generally, in Haskell:

 > "if it compiles it certainly does what you intended" 
