module main

#include "@VEXEROOT/vlib/builtin/testdata/freestanding_tls/probe.h"

fn C.tls_probe_pristine() i32
fn C.tls_probe_store(u64)
fn C.tls_probe_matches(u64) i32

struct LibraryTlsPair {
mut:
	first  i32
	second i32
}

__global test__anon_fn_library_scalar = i32(17)
__global test__anon_fn_library_fixed = [i32(19), 23, 29]!

@[cinit]
__global test__anon_fn_library_pair = LibraryTlsPair{ first: 31, second: 37 }

@[export: 'v_tls_library_probe']
fn library_probe(value u64, initialized i32) i32 {
	if initialized != 0 {
		if test__anon_fn_library_scalar != i32(value) || test__anon_fn_library_fixed[0] != i32(value)
			|| test__anon_fn_library_pair.first != i32(value) {
			return 0
		}
		return C.tls_probe_matches(value)
	}
	if test__anon_fn_library_scalar != 17 || (test__anon_fn_library_fixed[0] != 19 || test__anon_fn_library_fixed[1] != 23 || test__anon_fn_library_fixed[2] != 29)
		|| test__anon_fn_library_pair.first != 31 || test__anon_fn_library_pair.second != 37 {
		return 0
	}
	if C.tls_probe_pristine() != 1 { return 0 }
	test__anon_fn_library_scalar = i32(value)
	test__anon_fn_library_fixed[0] = i32(value)
	test__anon_fn_library_pair.first = i32(value)
	C.tls_probe_store(value)
	return 1
}
