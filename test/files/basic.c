#include <stdlib.h>

int f(int x, int y) {
    int a = 1;
    int b = 2;
    int c = a + b;
    
    int *arr = (int *)malloc(3 * sizeof(int));
    arr[0] = a;
    arr[1] = b;
    arr[2] = x;
    free(arr);
    
    while (c < 10) {
        c = c + 1;
    }
    return 0;
}