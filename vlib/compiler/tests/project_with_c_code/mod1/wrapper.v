module mod1

#flag -I @VROOT/c
#flag @VROOT/c/implementation.o

#include "header.h"

fn C.cadd(int,int) int

pub fn vadd(a int, b int) int {
	return 1000 + C.cadd(a,b)
}
