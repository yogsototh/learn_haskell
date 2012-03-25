#include <stdio.h>
#include <stdlib.h>

int evenSum(int *list) {
    return accumSum(0,list);
}

int accumSum(int n, int *list) {
    int x;
    int *xs;
    if (*list == NULL) { // if the list is empty
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

// Create the list [1..n]
int *createList(int n) {
    int *list = malloc((n+1) * sizeof(int));
    int i;
    for (i=0; i<n ; i++) list[i]=i+1;
    list[n]=0;
    return list;
}


// the length of the string to be displayed to
// show the list of integer
int strLengthForList(int *list) {
    int res=0;
    int p;
    while (*list) {
        p=1;
        res+=2; // for , and space
        while (*list >= p) {
            res++;
            p*=10;
        }
        list++;
    }
    // I don't remove the two more char value
    // because I must take into account [ and ]
    return res;
}

// show the list
char *showList(int *list) {
    int len=strLengthForList(list);
    char *result = (char *)malloc( len  );
    *result='\0';

    while (*list) {
        if (*result) 
            sprintf(result, "%s, %d", result, *list);
        else
            sprintf(result, "[%d", *list);
        list++;
    }
    result[len-1]=']';
    result[len]='\0';
    return result;
}

int main(int argc, char **argv) {
    int *list = createList(5);
    printf("evenSum of %s is %d\n", showList(list), evenSum(list));
    return 0;
}
