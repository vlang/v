#include <stdio.h>
extern int abc__addition(int x, int y);
int main() {
	int res = abc__addition(10,20);
	printf("Result of addition: %d\n", res);
	return 0;
}
