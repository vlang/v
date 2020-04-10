module mod1

#flag -I vlib/v/tests/project_with_c_code/mod1/c
#flag vlib/v/tests/project_with_c_code/mod1/c/implementation.o

#include "header.h"

fn C.cadd(int,int) int

pub fn vadd(a int, b int) int {
	return 1000 + C.cadd(a,b)
}
