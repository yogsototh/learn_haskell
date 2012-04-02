<h2 id="introduction">Introduction</h2>

<h3 id="install">Install</h3>

<%= blogimage("Haskell-logo.png", "Haskell logo") %>

- [Haskell Platform](http://www.haskell.org/platform) is the standard way to install Haskell.

Tools:

- `ghc`: Compiler similar to gcc for `C`.
- `ghci`: Interactive Haskell (REPL)
- `runhaskell`: Execute a program without compiling it. Convenient but very slow compared to compiled program.

<h3 id="don-t-be-afraid">Don't be afraid</h3>

<%= blogimage("munch_TheScream.jpg","The Scream") %>

Many book/articles about Haskell start by introducing some esoteric formula (quicksort, fibonacci serie, etc...).
I will make the exact opposite.
At first I won't show you any Haskell super power.
I will start with similarities between Haskell and other programming languages.
Let's jump in the obligatory "Hello World".

> main = print "Hello World!"

To run it, you can save this code in a `hello.hs` and:

<code class="zsh">
~ runhaskell ./hello.hs
Hello World!
</code>

You could also download the literate Haskell source.
You should see a link just above the introduction title.
Download this file as `00_hello_world.lhs` and:

<code class="zsh">
~ runhaskell 00_hello_world.lhs
Hello World!
</code>
