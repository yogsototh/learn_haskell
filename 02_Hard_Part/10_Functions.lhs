en: <h2 id="hard-part">Hard Part</h2>
fr: <h2 id="hard-part">La Partie Difficile</h2>

en: The hard part can now begin.
fr: La partie difficile peut maintenant commencer.

en: <h3 id="functional-style">Functional style</h3>
fr: <h3 id="functional-style">Le style fonctionnel</h3>

blogimage("hr_giger_biomechanicallandscape_500.jpg","Biomechanical Landscape by H.R. Giger")

en: In this section, I will give a short example of the impressive refactoring ability provided by Haskell.
en: We will select a problem and solve it in a standard imperative way.
en: Then I will make the code evolve.
en: The end result will be both more elegant and easier to adapt.
fr: Dans cette section, je vais vous donner un court exemple de l'impressionante capacité de remaniement de Haskell.
fr: Nous allons sélectionner un problème et le résoudre à la manière d'un langage impératif standard.
fr: Ensuite, je ferais évoluer le code.
fr: Le résultat final sera plus élégant et plus facile à adapter.

en: Let's solve the following problem:
fr: résolvons les problèmes suivants:

en:  > Given a list of integers, return the sum of the even numbers in the list.
fr:  > Soit une liste d'entiers, retourner la somme des nombres pairs de cette liste.
 >
en:  > example:
fr:  > exemple:
 > `[1,2,3,4,5] ⇒  2 + 4 ⇒  6`

en: To show differences between functional and imperative approaches,
en: I'll start by providing an imperative solution (in JavaScript):
fr: Pour montrer les différences entre les approches fonctionnelle et impérative,
fr: je vais commencer par donner la solution impérative (en JavaScript):

<code class="javascript">
function evenSum(list) {
    var result = 0;
    for (var i=0; i< list.length ; i++) {
        if (list[i] % 2 ==0) {
            result += list[i];
        }
    }
    return result;
}
</code>

en: In Haskell, by contrast, we don't have variables or a for loop.
en: One solution to achieve the same result without loops is to use recursion.
fr: En Haskell, en revanche, nous n'avons pas de variables ou un boucle `for`.
fr: Une des solutions pour parvenir au même résultat sans boucles est d'utiliser la récursion.

en:  > _Remark_:
en:  > Recursion is generally perceived as slow in imperative languages.
en:  > But this is generally not the case in functional programming.
en:  > Most of the time Haskell will handle recursive functions efficiently.
fr:  > _Remarque_:
fr:  > La récursion est souvent perçue comme lente dans les langages impératifs.
fr:  > Mais ce n'est généralement pas le cas en programmation fonctionnelle.
fr:  > La plupart du temps Haskell gérera les fonctions récursives efficacement.

en: Here is a `C` version of the recursive function.
en: Note that for simplicity I assume the int list ends with the first `0` value.
fr: Voici la version `C` de la fonction récursive.
fr: Remarquez que je suppose que la liste d'int fini avec la première valeur `0`.


<code class="c">
int evenSum(int *list) {
    return accumSum(0,list);
}

int accumSum(int n, int *list) {
    int x;
    int *xs;
    if (*list == 0) { // if the list is empty
        return n;
    } else {
        x = list[0]; // let x be the first element of the list
        xs = list+1; // let xs be the list without x
        if ( 0 == (x%2) ) { // if x is even
            return accumSum(n+x, xs);
        } else {
            return accumSum(n, xs);
        }
    }
}
</code>

en: Keep this code in mind. We will translate it into Haskell.
en: First, however, I need to introduce three simple but useful functions we will use:
fr: Gardez ce code à l'esprit. Nous allons le traduire en Haskell.
fr: Premièrement,

<code class="haskell">
even :: Integral a => a -> Bool
head :: [a] -> a
tail :: [a] -> [a]
</code>

en: `even` verifies if a number is even.
fr: `even` vérifie si un nombre est pair.

<code class="haskell">
even :: Integral a => a -> Bool
even 3  ⇒ False
even 2  ⇒ True
</code>

en: `head` returns the first element of a list:
fr: `head` retourne le premier élément d'une liste:

<code class="haskell">
head :: [a] -> a
head [1,2,3] ⇒ 1
head []      ⇒ ERROR
</code>

en: `tail` returns all elements of a list, except the first:
fr: `tail` retourne tous les éléments d'une liste, sauf le premier:

<code class="haskell">
tail :: [a] -> [a]
tail [1,2,3] ⇒ [2,3]
tail [3]     ⇒ []
en: tail []      ⇒ ERROR
fr: tail []      ⇒ ERREUR
</code>

en: Note that for any non empty list `l`,
en:  `l ⇔ (head l):(tail l)`
fr: Remarquez que pour toute liste non-vide `l`,
fr:  `l ⇔ (head l):(tail l)`
