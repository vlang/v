#include <stdio.h>

extern int square(int i);

extern double sqrt_of_sum_of_squares(double x, double y);

int main()
{
	int i = 10;
	printf("square(%d) = %d\n", i, square(i));
	double x=0.9;
	double y=1.2;
	printf("sqrt_of_sum_of_squares(%f, %f) = %f\n", x, y, sqrt_of_sum_of_squares(x, y));
}
