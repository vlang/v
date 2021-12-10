#include "header.h"
#include <stdlib.h>
#include <stdio.h>


void* new_atype(int n) {
	struct Atype *x = malloc(sizeof(struct Atype));
	x->val = n;
	return x;
}

// pointer to first element of array
void handle_array(void *p, int n) {
	struct Atype *ptr = (struct Atype *)p;
	for (int i=0; i<n; i++) {
		printf("%d - %d\n", i, ptr[i].val);
	}
}

// pointer to array
void handle_array2(void *p, int n) {
	struct Atype (*ptr)[];
	ptr = p;
	for (int i=0; i<n; i++) {
		printf("%d - %d\n", i, (*ptr+i)->val);
	}
}

void destroy_atype(void *p) {
	free(p);
}

