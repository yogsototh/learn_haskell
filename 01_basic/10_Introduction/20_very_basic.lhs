<h3 id="very-basic-haskell">Very basic Haskell</h3>

<%= blogimage("picasso_owl.jpg","Picasso minimal owl") %>

Before continuing you need to be warned about some essential properties of Haskell.

_Functional_

Haskell is a functional language.
If you have an imperative language background, you'll have to learn a lot of new things.
Hopefully many of these new concepts will help you to program even in imperative languages.

_Smart Static Typing_

Instead of being in your way like in `C`, `C++` or `Java`, the type system is here to help you.

_Purity_

Generally your functions won't modify anything in the outside world.
This means, it can't modify the value of a variable, can't get user input, can't write on the screen, can't launch a missile.
On the other hand, parallelism will be very easy to achieve.
Haskell makes it clear where effects occur and where you are pure.
Also, it will be far easier to reason about your program.
Most bugs will be prevented in the pure parts of your program.

Furthermore pure functions follow a fundamental law in Haskell:

 > Applying a function with the same parameters always returns the same value.

_Laziness_

Laziness by default is a very uncommon language design.
By default, Haskell evaluates something only when it is needed.
In consequence, it provides a very elegant way to manipulate infinite structures for example.

A last warning on how you should read Haskell code.
For me, it is like reading scientific papers.
Some parts are very clear, but when you see a formula, just focus and read slower.
Also, while learning Haskell, it _really_ doesn't matter much if you don't understand syntax details.
If you meet a `>>=`, `<$>`, `<-` or any other weird symbol, just ignore them and follows the flow of the code.

<h4 id="function-declaration">Function declaration</h4>

You might be used to declare functions like this:

In `C`:

<code class="c">
int f(int x, int y) {
    return x*x + y*y;
}
</code>

In Javascript:

<code class="javascript">
function f(x,y) {
    return x*x + y*y;
}
</code>

in Python:

<code class="python">
def f(x,y):
    return x*x + y*y
</code>

in Ruby:

<code class="ruby">
def f(x,y)
    x*x + y*y
end
</code>

In Scheme:

<code class="scheme">
(define (f x y)
    (+ (* x x) (* y y)))
</code>

Finally, the Haskell way is:

<code class="haskell">
f x y = x*x + y*y
</code>

Very clean. No parenthesis, no `def`.

Don't forget, Haskell uses functions and types a lot.
It is thus very easy to define them.
The syntax was particularly well thought for these objects.

<h4 id="a-type-example">A Type Example</h4>

The usual way is to declare the type of your function.
This is not mandatory.
The compiler is smart enough to discover it for you.

Let's play a little.

> -- We declare the type using ::
> f :: Int -> Int -> Int
> f x y = x*x + y*y
>
> main = print (f 2 3)

~~~
~ runhaskell 20_very_basic.lhs
13
~~~
