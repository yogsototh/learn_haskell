begindiv(intro)

This article should be named: "Blow your mind with Haskell".

As many people it was hard for me to bend my mind to the Haskell language.
In this article I give some ressource to help pass some wall of incomprehension. 

First, in order to make things work I deliever a as short as possible list of boring syntax notation used by Haskell.

Once you understand the notation, we could reach the real stuff. The one that make Haskell both unique and hard to learn.

First, what you _shouldn't_ expect from Haskell?
From my experience:

- _Terseness_. Haskell can be extremely terse, but some language are better at this game, for example codegolf and to a certain extent (understand for small programs) ruby and python .
- _Readability_. Many might say it is a question of habit, but personally I find Haskell a bit harder to read than Python and Ruby. It is not necessarily a drawback. Haskell code can be compared to a scientific paper. You have some very readable part, but sometimes you can encounter a mathematical formula. In these case, you need to focus because something hard is happening.

What to expect then?

An excelent balance between speed and high level.
Haskell is very fast even if not as fast as C but it has an incredible ability to make your code generalizable.
In fact the only limit of generalization of your code are either your intellectual capacity or some mathematical properties.
Guess which limit is generally reached first?

This actual article contains three parts.

- Introduction: a fast dig inside some real Haskell example
- Basic Haskell: Haskell syntax, and some essential notions
- Haskell New Things: Explaining the hard parts; IO and Monads
    - Functional style; an example from imperative to functional
    - Purity and IO; how to use and how it is incredible
    - Monads; incredible how we can generalize

 > Note: Each time you'll see a separator with a filename ending in `.lhs`, you could click the filename to get this file. If you save the file as `filename.lhs`, you can run it with 
 > <pre>
 > runhaskell filename.lhs
 > </pre>
 > 
 > You should see a link just below the line.

enddiv
