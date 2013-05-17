The solution: don't declare a type for `f` for the moment and let Haskell infer the most general type for us:

> f x y = x*x + y*y
>
> main = print (f 2.3 4.2)

It works! 
Luckily, we don't have to declare a new function for every single type.
For example, in `C`, you'll have to declare a function for `int`, for `float`, for `long`, for `double`, etc...

But, what type should we declare?
To discover the type Haskell has found for us, just launch ghci:

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

Uh? What is this strange type?

~~~
Num a => a -> a -> a
~~~

First, let's focus on the right part `a -> a -> a`.
To understand it, just look at a list of progressive examples: 

| The&nbsp;written&nbsp;type | Its meaning |
| `Int`            | the type `Int`                              |
| `Int -> Int`     | the type function from `Int` to `Int`       |
| `Float -> Int`   | the type function from `Float` to `Int`     |
| `a -> Int`       | the type function from any type to `Int`    |
| `a -> a`         | the type function from any type `a` to the same type `a`  |
| `a -> a -> a`    | the type function of two arguments of any type `a` to the same type `a`  |

In the type `a -> a -> a`, the letter `a` is a _type variable_. 
It means `f` is a function with two arguments and both arguments and the result have the same type.
The type variable `a` could take many different type values.
For example `Int`, `Integer`, `Float`...

So instead of having a forced type like in `C` and having to declare a function
for `int`, `long`, `float`, `double`, etc., we declare only one function like
in a dynamically typed language.

This is sometimes called parametric polymorphism. It's also called having your
cake and eating it too.

Generally `a` can be any type, for example a `String` or an `Int`, but also
more complex types, like `Trees`, other functions, etc. But here our type is
prefixed with `Num a => `.

`Num` is a _type class_.
A type class can be understood as a set of types.
`Num` contains only types which behave like numbers.
More precisely, `Num` is class containing types which implement a specific list of functions, and in particular `(+)` and `(*)`.

Type classes are a very powerful language construct.
We can do some incredibly powerful stuff with this.
More on this later.

Finally, `Num a => a -> a -> a` means:

Let `a` be a type belonging to the `Num` type class.
This is a function from type `a` to (`a -> a`).

Yes, strange. 
In fact, in Haskell no function really has two arguments.
Instead all functions have only one argument.
But we will note that taking two arguments is equivalent to taking one argument and returning a function taking the second argument as a parameter.

More precisely `f 3 4` is equivalent to `(f 3) 4`. 
Note `f 3` is a function:

~~~
f :: Num a => a -> a -> a

g :: Num a => a -> a
g = f 3

g y ⇔ 3*3 + y*y
~~~

Another notation exists for functions. 
The lambda notation allows us to create functions without assigning them a name.
We call them anonymous functions.
We could also have written:

~~~
g = \y -> 3*3 + y*y
~~~

The `\` is used because it looks like `λ` and is ASCII.

If you are not used to functional programming your brain should be starting to heat up.
It is time to make a real application.
