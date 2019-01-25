<h2 id="introduction">Introduction</h2>

en: <h3 id="install">Install</h3>
fr: <h3 id="install">Installation</h3>

blogimage("Haskell-logo.png", "Haskell logo")

en: There are different way to install Haskell, I would recommend to use
fr: Aujourd'huil je considère que la manière la plus aisée d'installer Haskell est d'utiliser
[`stack`](https://haskellstack.org).

en: There are other way to install Haskell on your system you could visit,
en: you can learn more about it by visiting
fr: Il y a d'autres manières d'installer Haskell sur votre système,
fr: vous pouvez en savoir plus en visitant
[haskell.org](https://haskell.org)
en: or
fr: ou
[haskell-lang.org](https://haskell-lang.org)

en: Tools:
fr: Outils:

en: - `ghc`: Compiler similar to gcc for `C`.
en: - `ghci`: Interactive Haskell (REPL)
en: - `runhaskell`: Execute a program without compiling it. Convenient but very slow compared to compiled programs.
fr: - `ghc`: Compilateur similaire à gcc pour le langage `C`.
fr: - `ghci`: Console Haskell interactive (Read-Eval-Print Loop)
fr: - `runhaskell`: Exécuter un programme sans le compiler. Pratique mais très lent comparé aux programmes compilés.

en: <h3 id="don-t-be-afraid">Don't be afraid</h3>
fr: <h3 id="don-t-be-afraid">Ne soyez pas effrayés!</h3>

blogimage("munch_TheScream.jpg","The Scream")

en: Many books/articles about Haskell start by introducing some esoteric formula (quick sort, Fibonacci, etc...).
en: I will do the exact opposite.
en: At first I won't show you any Haskell super power.
en: I will start with similarities between Haskell and other programming languages.
en: Let's jump to the mandatory "Hello World".
fr: Beaucoup de livres/articles sur Haskell commencent par présenter des formules ésotériques (Algorithmes de tri rapide, suite de Fibonacci, etc...).
fr: Je ferai l'exact opposé
fr: En premier lieu je ne vous montrerai pas les super-pouvoirs d'Haskell.
fr: Je vais commencer par les similarités avec les autres langages de programmation.
fr: Commençons par l'indispensable "Hello World!".

> main = putStrLn "Hello World!"

en: To run it, you can save this code in a `hello.hs` and:
fr: Pour l'exécuter, vous pouvez enregistrer ce code dans un fichier `hello.hs` et:

<code class="zsh">
~ runhaskell ./hello.hs
Hello World!
</code>

en: or if you use `stack` first run `stack setup` and then:
fr: ou si vous utilisez `stack` lancez d'abord `stack setup` et ensuite :

<code class="zsh">
~ stack runhaskell ./hello.hs
Hello World!
</code>


en: You could also download the literate Haskell source.
en: You should see a link just above the introduction title.
en: Download this file as `00_hello_world.lhs` and:
fr: Vous pouvez également télécharger la source Haskell littérale.
fr: Vous devriez voir un lien juste au dessus du titre de l'introduction.
fr: Téléchargez ce fichier en tant que `00_hello_world.lhs` et:

<code class="zsh">
~ runhaskell 00_hello_world.lhs
Hello World!
</code>
