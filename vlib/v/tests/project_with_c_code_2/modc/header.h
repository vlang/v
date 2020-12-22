#ifndef CMOD_H
#define CMOD_H

struct Atype {
	int val;
};


void* new_atype(int n);

void handle_array(void *p, int n);

void handle_array2(void *p, int n);

void destroy_atype(void *p);


#endif

