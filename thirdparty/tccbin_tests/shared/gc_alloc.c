/* Allocates through GC_malloc under real load, and reads the result
   back - exercises the GC's stack-scanning/collection machinery, not
   just its startup path. */
#include <stdio.h>
#include "gc.h"

struct node {
    int val;
    struct node *next;
};

int main(void) {
    GC_INIT();
    struct node *head = NULL;
    for (int i = 0; i < 100000; i++) {
        struct node *n = (struct node *)GC_malloc(sizeof(struct node));
        n->val = i;
        n->next = head;
        head = n;
    }
    long long sum = 0;
    for (struct node *n = head; n; n = n->next)
        sum += n->val;
    printf("sum=%lld\n", sum);
    return 0;
}
