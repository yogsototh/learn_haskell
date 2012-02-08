Some very basic knowledge for Haskell:

1. Function declaration

You might be used to declare functions like this:

in C

<code class="C">
int f(int x, int y) {
    return x*x + y*y;
}
</code>

in Javascript

<code class="javascript">
function f(x,y) {
    return x*x + y*y;
}
</code>

in Python

<code class="python">
def f(x,y) =
    return x*x + y*y;
</code>

in Ruby

<code class="ruby">
def f(x,y)
    x*x + y*y
end
</code>

In scheme

<code class="scheme">
(define (f x y)
    (+ (* x x) (* y y)))
</code>

The haskell version is even lighter:

<code class="haskell">
f x y = x*x + y*y
</code>

Don't forget, Haskell is mainly built on function and types.
It is thus very easy to define functions and types.
The syntax was particularly well thought for these objects.

Generally it is a good idea to write the type of your function before its definition. But it is not mandatory. The compiler is smart enough to imagine it for you.

<code class="haskell">
f :: Int -> Int -> Int
f x y = x*x + y*y
</code>

Not bad, but what should we do if we want to add Float instead? 
Or some strange type like complex?

Don't worry, here is the solution:

<code class="haskell">
f :: Num a => a -> a -> a
f x y = x*x + y*y
</code>

First, `a` is a type variable. 
It means, that the first and the second argument will have the same type.
And furthermore, the result will also be of the same type.

So instead of having a forced type like in `C` with declaring the function for int, for long, for float, for double, etc... 
We declare only one function.
But remember our type is statically typed! Great.
It feels like dynamic typed language, and for this, this is good.

But what is this `Num a => ` part about?

`Num` is a typeclass.
Which means we are authorized to use any type which belongs to the class Num.
If a type declare himself to be a num, he must provide a definition for some functions. 
And for this example, the Num class ensure there will exists a `+` and a `*` for our type.

In very few lines I had given _a lot_ of informations.

Let's play a little.

> f :: Int -> Int -> Int
> f x y = x*x + y*y
>
> main = print (f 2 3)

