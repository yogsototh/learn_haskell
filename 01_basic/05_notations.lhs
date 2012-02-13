Let's start with notations. Most should be all right.

Most operation are the same as usual.

~~~
3 + 2 * 6 / 3 <=> 3 + ((2*6)/3)
~~~

Logic notation:

~~~
True || False => True
True && False => False
True == False => False
True /= False => True  (/=) is the operator for different
~~~

Powers

~~~
x^n     for n an integral
x**y    for y any kind of number (Float for example)
~~~

Integer have no limit except the capacity of your machine:

~~~
4^103   
102844034832575377634685573909834406561420991602098741459288064
~~~

Yeah!
But also perfect rational numbers.

~~~
Prelude> :m Data.Ratio
Data.Ratio> (11%15) * (5%3)
11 % 9
~~~

Lists

~~~
[]  empty list
[1,2,3]                 List of integral
["foo","bar","baz"]     List of string
1:[2,3]                 => [1,2,3], (:) prepend one element
[1,2,3] ++ [4]          => [1,2,3,4], (++) concatenate two lists
[1,2,3] ++ ["foo"]      => ERROR String /= Integral
[1..10]                 => [1,2,3,4,5,6,7,8,9,10]
[1,3..10]               => [1,3,5,7,9]
[2,3,5,7,11..100]       => ERROR I am not so smart!
[10,9..1]               => [10,9,8,7,6,5,4,3,2,1]
~~~

Strings. 

A bad way to represent them is to use list of Char.

~~~
'a' :: Char
"a" :: [Char]
""  <=> []
~~~

It will be useful to learn, but in real code, don't use it.
There is better data structure for strings. 
Use Data.Text instead (mostly).

# Types

Haskell has static strong types.

Every value as a type and the type is know at compile time.
Static typing is generally essential to reach fast execution time.
But in common languages static typing has the price of bad generalization.


~~~
// in C
int     int_square(int x) { return x*x; }
float   fl_square(float x) {return x*x; }
complex complex_square (complex z) {
    complex tmp; 
    tmp.real = z.real * z.real - z.img * z.img;
    tmp.img = 2 * z.img * z.real;
}
~~~

To compensate a bit, C++ has templates:

~~~
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

~~~

In Haskell

~~~
square x = x * x

square 2
4
square 2.1
4.41

:m Data.Complex
(2 :+ 1) * (2 :+ 1) 
3.0 :+ 4.0

~~~
`x :+ y` is the notation for the complex (x + ib).
As with `C++`, the code for the multiplication is made inside the module Data.Complex.

