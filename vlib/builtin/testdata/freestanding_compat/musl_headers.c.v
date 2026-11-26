module main

#include "@DIR/musl_headers.h"

fn C.v_test_musl_headers() int

fn main() {
	exit(C.v_test_musl_headers())
}
