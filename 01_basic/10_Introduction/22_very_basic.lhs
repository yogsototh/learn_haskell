The solution, 
don't declare type and let Haskell find the most possible general type for us:

> f x y = x*x + y*y
>
> main = print (f 2.3 4.2)

It works! 
Great, we don't have to declare a new function for each different type.
For example, in `C`, you'll have to declare a function for `int`, for `float`, for `long`, for `double`, etc...

But, what type should we declare?
To discover the type Haskell as found for us, just launch ghci:

<pre>
% ghci
<span style="color: #999;">GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude&gt;</span> :load 22_very_basic.lhs<span style="color: #999;">  
[1 of 1] Compiling Main    ( 22_very_basic.lhs, interpreted )
Ok, modules loaded: Main.
*Main&gt;</span> :type f
f :: Num a => a -> a -> a
</pre>

Hey? What is this strange type?

~~~
Num a => a -> a -> a
~~~

First, `a` is a type variable. 
It means, that the first and the second argument will have the same type.
And furthermore, the result will also be of the same type.
The type variable `a` could take many different type value.
For example `Int`, `Integer`, `Float`...

So instead of having a forced type like in `C` with declaring the function for `int`, `long`, `float`, `double`, etc... 
We declare only one function like in a dynamic typed language.

Generally, without the type class constraint, `a` can be any type. 
For example a `String`, an `Int`, but also more complex types, like `Trees`, other functions, etc...
But here with have a `Num a => `. 

`Num` is a typeclass.
It contains only type which behave like numbers.
In fact, `Num` is class containing types who implement a specific list of functions, and in particular `(+)` and `(*)`.

Typeclass is a very powerful language construction.
We can do some incredibly powerful construction with this.
More on this later.

Finally, `Num a => a -> a -> a` means:

Let `a` be a type belonging to the `Num` typeclass.
This is a function from type `a` to (`a -> a`).

Yes, strange, in Haskell no function have two argument.
Instead all function have only one argument.

In fact `f 3 4` is equivalent to `(f 3) 4`. 
Note `f 3` is a function:

~~~
f :: Num a :: a -> a -> a

g :: Num a :: a -> a
g = f 3

g y ⇔ 3*3 + y*y
~~~

Another notation exists for function. 
The lambda notation permit us to create function without assigning them a name.
We call them anonymous function.
We could have written:

~~~
g = \y -> 3*3 + y*y
~~~

If you are not used to functional programming your brain should start to heat up.
It is time to make some real application.
