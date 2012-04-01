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

<pre><span class="low">
%</span> ghci<span class="low"><code>
GHCi, version 7.0.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package ffi-1.0 ... linking ... done.
Prelude></code></span> let f x y = x*x + y*y
<span class="low"><code>Prelude></code></span> :type f
<code>f :: Num a => a -> a -> a</code>
</pre>

Hey? What is this strange type?

~~~
Num a => a -> a -> a
~~~

First, let's focus on the right part `a -> a -> a`.
Here are some notation for type in Haskell:

- `Int`           → the type `Int`
- `Int -> Int`    → the type function from `Int` to `Int`.
- `Float -> Int`  → the type function from `Float` to `Int`.
- `a -> Int`      → the type function from any type to `Int`.
- `a -> a`        → the type function from any type `a` to the same type `a`.
- `a -> a -> a`   → the type function of two arguments of any type `a` to the same type `a`.

In the type `a -> a -> a`, the letter `a` is a _type variable_. 
It means `f` is a function with two argument and both argument and the result have the same type.
The type variable `a` could take many different type value.
For example `Int`, `Integer`, `Float`...

So instead of having a forced type like in `C` with declaring the function for `int`, `long`, `float`, `double`, etc... 
We declare only one function like in a dynamic typed language.

Generally `a` can be any type. 
For example a `String`, an `Int`, but also more complex types, like `Trees`, other functions, etc...
But here with have a `Num a => `. 

`Num` is a _typeclass_.
A typeclass can be understood as a set of types.
`Num` contains only type which behave like numbers.
More precisely, `Num` is class containing types who implement a specific list of functions, and in particular `(+)` and `(*)`.

Typeclass is a very powerful language construction.
We can do some incredibly powerful stuff with this.
More on this later.

Finally, `Num a => a -> a -> a` means:

Let `a` be a type belonging to the `Num` typeclass.
This is a function from type `a` to (`a -> a`).

Yes, strange. 
In fact, in Haskell no function really have two arguments.
Instead all functions have only one argument.

More precisely `f 3 4` is equivalent to `(f 3) 4`. 
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

The `\` is used because it looks like `λ` and is ASCII.

If you are not used to functional programming your brain should start to heat up.
It is time to make some real application.
