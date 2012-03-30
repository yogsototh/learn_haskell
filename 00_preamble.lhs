begindiv(intro)

Bend his mind to Haskell can be hard.
It was for me.
In this article I try to provide what I lacked when I started to learn Haskell.

As Boromir would say: "One does not simply learn Haskell".
To learn Haskell you'll need to learn far more than just a new language.
Haskell use a lot of concepts I've never heard about before.
But many will be useful for programming even in other languages.

The article will certainly be hard to follow.
This is done on purpose.
There is no shortcut to learn Haskell.
It is hard and challenging. 
But I believe this is a good thing.
This is like playing a very hard game.

This article can be seen as a very dense introduction to Haskell.
The conventional way to learn Haskell is to read two books. 
First ["Learn You a Haskell"](http://learnyouahaskell.com) and just after ["Real World Haskell"](http://www.realworldhaskell.org).
I also believe this is the right way to go.
But Haskell is very hard to learn by skimming these books.
You'll have to read them in detail.
This is why I believe such an article while difficult to read can be a very good introduction.
Furthermore, I believe I missed such an article while learning Haskell. 

This actual (long) article contains four parts:

- Introduction: a fast short example to show Haskell can be friendly.
- Basic Haskell: Haskell syntax, and some essential notions.
- Hard Difficulty Part:
    - Functional style; an example from imperative to functional style
    - Types; types and a standard binary tree example
    - Infinite Structure; manipulate an infinite binary tree!
- Hell Difficulty Part:
    - Deal with IO; A very minimal example
    - IO trick explained; the hidden detail I lacked to understand IO
    - Monads; incredible how we can generalize
    - More on infinite tree; a discussion on infinite tree manipulation.

 > Note: Each time you'll see a separator with a filename ending in `.lhs`, you could click the filename to get this file. If you save the file as `filename.lhs`, you can run it with 
 > <pre>
 > runhaskell filename.lhs
 > </pre>
 >
 > Some might not work, but most will.
 > You should see a link just below.

enddiv
