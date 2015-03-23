begindiv(intro)

en: I really believe all developers should learn Haskell.
en: I don't think everyone needs to be super Haskell ninjas,
en: but they should at least discover what Haskell has to offer.
en: Learning Haskell opens your mind.
fr: Je pense vraiment que
fr: tous les développeurs devraient apprendre Haskell.
fr: Peut-être pas devenir des ninjas d'Haskell,
fr: mais au moins savoir ce que ce langage a de particulier.
fr: Son apprentissage ouvre énormément l'esprit.

en: Mainstream languages share the same foundations:
fr: La plupart des langages partagent les mêmes fondamentaux :

en: - variables
en: - loops
en: - pointers[^0001]
en: - data structures, objects and classes (for most)
fr: - les variables
fr: - les boucles
fr: - les pointeurs[^0001]
fr: - les structures de données, les objets et les classes

en: [^0001]: Even if most recent languages try to hide them, they are present.
fr: [^0001]: Même si tous les langages récents essayent de les cacher, ils restent présents.

en: Haskell is very different.
en: The language uses a lot of concepts I had never heard about before.
en: Many of those concepts will help you become a better programmer.
fr: Haskell est très différent.
fr: Ce langage utilise des concepts dont je n'avais jamais entendu parlé avant.
fr: Beaucoup de ces concepts pourront vous aider à devenir un meilleur développeur.

en: But learning Haskell can be hard.
en: It was for me.
en: In this article I try to provide what I lacked during my learning.
fr: Plier son esprit à Haskell peut être difficile.
fr: Ce le fût pour moi.
fr: Dans cet article, j'essaye de fournir les informations qui m'ont manquées lors de mon apprentissage.

en: This article will certainly be hard to follow.
en: This is on purpose.
en: There is no shortcut to learning Haskell.
en: It is hard and challenging.
en: But I believe this is a good thing.
en: It is because it is hard that Haskell is interesting.
fr: Cet article sera certainement difficile à suivre.
fr: Mais c'est voulu.
fr: Il n'y a pas de raccourci pour apprendre Haskell.
fr: C'est difficile.
fr: Mais je pense que c'est une bonne chose.
fr: C'est parce qu'Haskell est difficile qu'il est intéressant.

en: The conventional method to learning Haskell is to read two books.
en: First ["Learn You a Haskell"](http://learnyouahaskell.com) and just after ["Real World Haskell"](http://www.realworldhaskell.org).
en: I also believe this is the right way to go.
en: But to learn what Haskell is all about, you'll have to read them in detail.
fr: La manière conventionnelle d'apprendre Haskell est de lire deux livres.
fr: En premier ["Learn You a Haskell"](http://learnyouahaskell.com)
fr: et ensuite ["Real World Haskell"](http://www.realworldhaskell.org).
fr: Je pense aussi que c'est la bonne manière de s'y prendre.
fr: Mais apprendre même un tout petit peu d'Haskell est presque impossible sans se plonger réellement dans ces livres.

en: In contrast, this article is a very brief and dense overview of all major aspects of Haskell.
en: I also added some information I lacked while I learned Haskell.
fr: Cet article fait un résumé très dense et rapide des aspect majeurs d'Haskell.
fr: J'y ai aussi rajouté des informations qui m'ont manqué pendant l'apprentissage de ce langage.

fr: Pour les francophones ; je suis désolé.
fr: Je n'ai pas eu le courage de tout retraduire en français.
fr: Sachez cependant que si vous êtes plusieurs à insister, je ferai certainement l'effort de traduire l'article en entier.
fr: Et si vous vous sentez d'avoir une bonne âme je ne suis pas contre un peu d'aide.
fr: Les sources de cet article sont sur [gihub](http://github.com/yogsototh/learn_haskell.git).

en: The article contains five parts:
fr: Cet article contient cinq parties :

en: - Introduction: a short example to show Haskell can be friendly.
en: - Basic Haskell: Haskell syntax, and some essential notions.
en: - Hard Difficulty Part:
en:     - Functional style; a progressive example, from imperative to functional style
en:     - Types; types and a standard binary tree example
en:     - Infinite Structure; manipulate an infinite binary tree!
en: - Hell Difficulty Part:
en:     - Deal with IO; A very minimal example
en:     - IO trick explained; the hidden detail I lacked to understand IO
en:     - Monads; incredible how we can generalize
en: - Appendix:
en:     - More on infinite tree; a more math oriented discussion about infinite trees

fr: - Introduction : un exemple rapide pour montrer qu'Haskell peut être facile.
fr: - Les bases d'Haskell : La syntaxe et des notions essentielles
fr: - Partie difficile :
fr:     - Style fonctionnel : un exemple progressif, du style impératif au style fonctionnel ;
fr:     - Types : la syntaxe et un exemple d'arbre binaire ;
fr:     - Structure infinie : manipulons un arbre infini !
fr: - Partie de difficulté infernale :
fr:     - Utiliser les IO : un exemple très minimal ;
fr:     - Le truc des IO révélé : les détails cachés d'IO qui m'ont manqués
fr:     - Les monades : incroyable à quel point on peut généraliser
fr: - Appendice :
fr:     - Revenons sur les arbres infinis : une discussion plus mathématique sur la manipulation d'arbres infinis.

en:  > Note: Each time you see a separator with a filename ending in `.lhs`
en:  > you can click the filename to get this file.
en:  > If you save the file as `filename.lhs`, you can run it with
en:  > <pre>
en:  > runhaskell filename.lhs
en:  > </pre>
en:  >
en:  > Some might not work, but most will.
en:  > You should see a link just below.

fr:  > Note: Chaque fois que vous voyez un séparateur avec un nom de fichier se terminant par `lhs`, vous pouvez cliquer sur le nom de fichier et télécharger le fichier.
fr:  > Si vous sauvegardez le fichier sour le nom `filename.lhs`, vous pouvez l'exécuter avec :
fr:  > <pre>
fr:  > runhaskell filename.lhs
fr:  > </pre>
fr:  >
fr:  > Certains ne marcheront pas, mais la majorité vous donneront un résultat.
fr:  > Vous devriez voir un lien juste en dessous.

enddiv
