#include <stdlib.h>

void f(int count) {
    int* curr = malloc (2 * sizeof(int));
    int* next;
    while (count > 0) {
        count --;
        next = malloc (2 * sizeof(int));
        free(curr);
        curr = next;
    }
    curr[0] = 1;
}