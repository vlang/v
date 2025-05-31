module multiple_c_cources

#flag @VMODROOT/file1.c
#flag @VMODROOT/file2.c

#include "@VMODROOT/common.h"

fn C.f1() int
fn C.f2() int

pub fn call_c_functions() {
	println('start')
	println(C.f1())
	println(C.f2())
	println('done')
}
