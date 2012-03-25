#include <stdio.h>
#include <stdlib.h>

int evenSum(int *list) {
    return accumSum(0,list);
}

// In C I should have declared this 
// function before evenSum, but
// I find it easier this way
int accumSum(int n, int *list) {
    int x;
    int *xs;
    if (list[0] == 0) { // if the list is empty
        return n;
    } else {
        x = list[0]; // let x be the first element of the list
        xs = list+1; // let xs be the list without its head
        if ( 0 == (x%2) ) { // if x is even
            return accumSum(n+x, xs);
        } else {
            return accumSum(n, xs);
        }
    }
}

int *createList(int n) {
    int *list = malloc((n+1) * sizeof(int));
    int i;
    for (i=0; i<n ; i++) {
        list[i]=i+1;
    }
    list[n]=0;
    return list;
}


int main(int argc, char **argv) {
    int *list = createList(5);
    printf("result: %d\n", evenSum(list));
    return 0;
}
