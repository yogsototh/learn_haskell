The solution, 
don't declare type and let Haskell find the most possible general type for us:

> f x y = x*x + y*y
>
> main = print (f 2.3 4.2)

It works! 
Great, we don't have to declare a new function for each different type.
For example, in `C`, you'll have to declare a function for int, for float, for long, for double, etc...

But, what type should we declare?
To discover the type Haskell as found for us, just launch ghci:

~~~
% ghci
GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :load very_basic_3.lhs 
[1 of 1] Compiling Main             ( very_basic_3.lhs, interpreted )
Ok, modules loaded: Main.
*Main> :type f
f :: Num a => a -> a -> a
~~~

Hey? What is this strange type?

~~~
Num a => a -> a -> a
~~~

First what is the `a` symbol? It is a type variable.
You can replace `a` by Int, or Integer of Float.
And the function will work.

Generally `a` can be any type. 
For example a String, an Int, but also more complex types, like Trees, functions, etc...
But here with have a `Num a => `. 

Num is a type class.
It contains only type which behave like numbers.
In fact, Num is class containing types who implement a specific list of functions, and in particular `(+)` and `(*)`.

Typeclass is a very efficient language construction. We can do some incredibly powerful construction with this. More on this later.

Then, `Num a => a -> a -> a` means:

Let `a` be a type belonging to the Num typeclass.
This is a function from type `a` to (`a -> a`).

Yes, strange, in Haskell no function have two argument.
Instead all function have only one argument.

In fact when evaluating: `f 3 4`. All occurs as

~~~
g :: Num a :: a -> a
g = f 3

result = g 4
~~~

where `g` is the function.

~~~
g y = 3*3 + y*y
~~~

In fact there is also another way to anotate function using the lambda notation. A lambda notation is a way to create function without giving them a name.
We could have written:

~~~
g = \y -> 3*3 + y*y
~~~

Ok, now, if you are not used to functional programming your brain should start to heat up.
It is time to make some real application.
