<h2 id="hard-part">Hard Part</h2>

The hard part can now begin.

<h3 id="functional-style">Functional style</h3>

<%= blogimage("hr_giger_biomechanicallandscape_500.jpg","Biomechanical Landscape by H.R. Giger") %>

In this section, I will give a short example of the impressive refactoring ability provided by Haskell.
We will select a problem and solve it using a standard imperative way.
Then I will make the code evolve.
The end result will be both more elegant and easier to adapt. 

Let's solve the following problem:

 > Given a list of integers, return the sum of the even numbers in the list.
 > 
 > example:
 > `[1,2,3,4,5] ⇒  2 + 4 ⇒  6`

To show differences between the functional and imperative approach,
I'll start by providing an imperative solution (in Javascript):

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

But, in Haskell we don't have variables, nor for loop.
One solution to achieve the same result without loops is to use recursion.

 > _Remark_:  
 > Recursion is generally perceived as slow in imperative languages.
 > But it is generally not the case in functional programming.
 > Most of the time Haskell will handle recursive functions efficiently.

Here is a `C` version of the recursive function.
Note that for simplicity, I assume the int list ends with the first `0` value.


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

Keep this code in mind. We will translate it into Haskell.
But before, I need to introduce three simple but useful functions we will use:

<code class="haskell">
even :: Integral a => a -> Bool
head :: [a] -> a
tail :: [a] -> [a]
</code>

`even` verifies if a number is even.

<code class="haskell">
even :: Integral a => a -> Bool
even 3  ⇒ False
even 2  ⇒ True
</code>

`head` returns the first element of a list:

<code class="haskell">
head :: [a] -> a
head [1,2,3] ⇒ 1
head []      ⇒ ERROR
</code>

`tail` returns all elements of a list, except the first:

<code class="haskell">
tail :: [a] -> [a]
tail [1,2,3] ⇒ [2,3]
tail [3]     ⇒ []
tail []      ⇒ ERROR
</code>

Note that for any non empty list `l`,
`l ⇔ (head l):(tail l)`
