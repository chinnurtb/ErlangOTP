#include <stdio.h>
#include <stdlib.h>

char strarr[5][10] =
{
    "one", "two", "three", "four", "five"
};

int intarr[5] =
{
    5, 4, 3, 2, 1
};

int strcmp(const void *, const void *);

int intcmp(const void *a, const void *b)
{
    if (* (int *) a > * (int *) b)
        return 1;
    else if (* (int *) a < * (int *) b)
        return -1;
    else
        return 0;
}

int main(void)
{
    int i;
    for (i=0; i < 5; i++) 
        printf("%s ", strarr[i]);
    printf("\n");
    qsort(strarr, 5, 10, &strcmp);
    for (i=0; i < 5; i++) 
        printf("%s ", strarr[i]);
    printf("\n");
    for (i=0; i < 5; i++) 
        printf("%d ", intarr[i]);
    printf("\n");
    qsort(intarr, 5, sizeof(int), &intcmp);
    for (i=0; i < 5; i++) 
        printf("%d ", intarr[i]);
    printf("\n");
    return 0;
}
