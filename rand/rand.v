module rand

#include <time.h>
// #include <stdlib.h>
fn seed() {
	# time_t t;
	# srand((unsigned) time(&t));
}

fn next(max int) int {
	# return  rand() % max;
	return 0
}

