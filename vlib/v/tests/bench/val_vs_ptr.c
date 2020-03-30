#include <stdio.h>

int increment_val(int n) {
	return n + 2;
}

// ~26% faster
void increment_ptr(int* n) {
	*n += 2;
}

int main() {
	int n = 0;
	for (int i = 0; i < 1000000000; i++) {
		n = increment_val(n);
		//increment_ptr(&n);
	}
	printf("%d\n", n);
	return 0;
}

