module main

#include "@VEXEROOT/vlib/builtin/testdata/freestanding_tls/probe.h"

fn C.tls_probe_pristine() i32
fn C.tls_probe_store(u64)
fn C.tls_probe_matches(u64) i32

__global tls_workers_ready u32
__global tls_workers_go u32

struct TlsPair {
mut:
	first  i32
	second i32
}

// Capture-style compiler-private names select V's native per-thread storage.
__global test__anon_fn_tls_zero = i32(0)
__global test__anon_fn_tls_scalar = i32(17)
__global test__anon_fn_tls_fixed = [i32(19), 23, 29]!

@[cinit]
__global test__anon_fn_tls_pair = TlsPair{ first: 31, second: 37 }

fn initialize_v_tls(value u64) {
	require_tls(test__anon_fn_tls_zero == 0)
	require_tls(test__anon_fn_tls_scalar == 17)
	require_tls(test__anon_fn_tls_fixed[0] == 19 && test__anon_fn_tls_fixed[1] == 23 && test__anon_fn_tls_fixed[2] == 29)
	require_tls(test__anon_fn_tls_pair.first == 31 && test__anon_fn_tls_pair.second == 37)
	test__anon_fn_tls_zero = i32(value)
	test__anon_fn_tls_scalar = i32(value + 100)
	test__anon_fn_tls_fixed[0] = i32(value + 200)
	test__anon_fn_tls_pair.first = i32(value + 300)
}

fn check_v_tls(value u64) {
	require_tls(test__anon_fn_tls_zero == i32(value))
	require_tls(test__anon_fn_tls_scalar == i32(value + 100))
	require_tls(test__anon_fn_tls_fixed == [i32(value + 200), 23, 29]!)
	require_tls(test__anon_fn_tls_pair.first == i32(value + 300) && test__anon_fn_tls_pair.second == 37)
}

// Keep verification active in optimized standalone executables.
fn require_tls(condition bool) {
	if !condition { exit(91) }
}

fn worker(value u64) {
	initialize_v_tls(value)
	require_tls(C.tls_probe_pristine() == 1)
	C.tls_probe_store(value)
	for {
		old := vlibc_load_u32(&tls_workers_ready)
		if vlibc_cas_u32(&tls_workers_ready, old, old + 1) { break }
	}
	for vlibc_load_u32(&tls_workers_go) == 0 {
	}
	for _ in 0 .. 1000 {
		require_tls(C.tls_probe_matches(value) == 1)
		check_v_tls(value)
	}
}

fn main() {
	initialize_v_tls(17)
	require_tls(C.tls_probe_pristine() == 1)
	C.tls_probe_store(17)
	mut first, first_error := vlibc_thread_spawn(worker, 1)
	require_tls(first_error == .enoerror)
	mut second, second_error := vlibc_thread_spawn(worker, 2)
	require_tls(second_error == .enoerror)
	for vlibc_load_u32(&tls_workers_ready) != 2 {
	}
	require_tls(C.tls_probe_matches(17) == 1)
	check_v_tls(17)
	vlibc_store_u32(&tls_workers_go, 1)
	first_join := vlibc_thread_join(mut first)
	second_join := vlibc_thread_join(mut second)
	require_tls(first_join == .enoerror)
	require_tls(second_join == .enoerror)
	$if linux {
		require_tls(first.tls_mapping == unsafe { nil })
		require_tls(second.tls_mapping == unsafe { nil })
	}
	require_tls(C.tls_probe_matches(17) == 1)
	check_v_tls(17)
	// Reusing a thread after join must start from the ELF template again.
	mut reused, reused_error := vlibc_thread_spawn(worker, 3)
	require_tls(reused_error == .enoerror)
	reused_join := vlibc_thread_join(mut reused)
	require_tls(reused_join == .enoerror)
	require_tls(C.tls_probe_matches(17) == 1)
	check_v_tls(17)
	println('freestanding TLS passed')
}
