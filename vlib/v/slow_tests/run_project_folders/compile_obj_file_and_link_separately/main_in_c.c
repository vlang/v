#include <stdio.h>
extern int abc_addition(int x, int y);
int main() {
	int res = abc_addition(10,20);
	printf("Result of addition: %d\n", res);
	return 0;
}
