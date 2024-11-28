module mod1

#flag -I @VMODROOT/mod1/c
#flag @VMODROOT/mod1/c/implementation.o

#include "header.h"

pub struct C.MyStruct {
	UppercaseField int
}

fn C.cadd(int, int) int

pub fn vadd(a int, b int) int {
	x := C.MyStruct{100}
	return 900 + x.UppercaseField + C.cadd(a, b)
}
