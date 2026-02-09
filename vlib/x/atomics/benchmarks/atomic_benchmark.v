module main

import x.atomics
import time

$if windows {
	#include "@VEXEROOT/thirdparty/stdatomic/win/atomic.h"
} $else {
	#include "@VEXEROOT/thirdparty/stdatomic/nix/atomic.h"
}

fn C.atomic_store_u32(voidptr, u32)
fn C.atomic_load_u32(voidptr) u32
fn C.atomic_fetch_add_u32(voidptr, u32) u32
fn C.atomic_compare_exchange_strong_u32(voidptr, voidptr, u32) bool
fn C.atomic_exchange_u32(voidptr, u32) u32

fn C.atomic_store_u64(voidptr, u64)
fn C.atomic_load_u64(voidptr) u64
fn C.atomic_fetch_add_u64(voidptr, u64) u64
fn C.atomic_compare_exchange_strong_u64(voidptr, voidptr, u64) bool
fn C.atomic_exchange_u64(voidptr, u64) u64

const iterations = 100_000_000

fn keepalive_u64(x u64) {
	asm volatile amd64 {
		nop
		; ; r (x)
	}
}

fn keepalive_u32(x u32) {
	asm volatile amd64 {
		nop
		; ; r (x)
	}
}

fn keepalive_i64(x i64) {
	asm volatile amd64 {
		nop
		; ; r (x)
	}
}

fn keepalive_i32(x i32) {
	asm volatile amd64 {
		nop
		; ; r (x)
	}
}

fn bench_u64(name string, f fn (&u64, u64), iters int) {
	mut v := u64(0)

	for i in 0 .. 100_000 {
		f(&v, u64(i))
	}

	mut sw := time.new_stopwatch()
	for i in 0 .. iters {
		f(&v, u64(i))
	}

	elapsed := sw.elapsed()
	ns_per_op := f64(elapsed.nanoseconds()) / f64(iters)

	keepalive_u64(v)
	println('${name}: ${ns_per_op:.3} ns/op (total: ${elapsed}, iters: ${iters})')
}

fn bench_u32(name string, f fn (&u32, u32), iters int) {
	mut v := u32(0)

	for i in 0 .. 100_000 {
		f(&v, u32(i))
	}

	mut sw := time.new_stopwatch()
	for i in 0 .. iters {
		f(&v, u32(i))
	}

	elapsed := sw.elapsed()
	ns_per_op := f64(elapsed.nanoseconds()) / f64(iters)

	keepalive_u32(v)
	println('${name}: ${ns_per_op:.3} ns/op (total: ${elapsed}, iters: ${iters})')
}

fn bench_i64(name string, f fn (&i64, i64), iters int) {
	mut v := i64(0)

	for i in 0 .. 100_000 {
		f(&v, i64(i))
	}

	mut sw := time.new_stopwatch()
	for i in 0 .. iters {
		f(&v, i64(i))
	}

	elapsed := sw.elapsed()
	ns_per_op := f64(elapsed.nanoseconds()) / f64(iters)

	keepalive_i64(v)
	println('${name}: ${ns_per_op:.3} ns/op (total: ${elapsed}, iters: ${iters})')
}

fn bench_i32(name string, f fn (&i32, i32), iters int) {
	mut v := i32(0)

	for i in 0 .. 100_000 {
		f(&v, i32(i))
	}

	mut sw := time.new_stopwatch()
	for i in 0 .. iters {
		f(&v, i32(i))
	}

	elapsed := sw.elapsed()
	ns_per_op := f64(elapsed.nanoseconds()) / f64(iters)

	keepalive_i32(v)
	println('${name}: ${ns_per_op:.3} ns/op (total: ${elapsed}, iters: ${iters})')
}

fn std_store_u64(addr &u64, val u64) {
	C.atomic_store_u64(voidptr(addr), val)
}

fn custom_store_u64(addr &u64, val u64) {
	atomics.store_u64(addr, val)
}

fn std_load_u64(addr &u64, _ u64) {
	_ = C.atomic_load_u64(voidptr(addr))
}

fn custom_load_u64(addr &u64, _ u64) {
	_ = atomics.load_u64(addr)
}

fn std_add_u64(addr &u64, delta u64) {
	_ = C.atomic_fetch_add_u64(voidptr(addr), delta)
}

fn custom_add_u64(addr &u64, delta u64) {
	_ = atomics.add_u64(addr, delta)
}

fn std_swap_u64(addr &u64, val u64) {
	_ = C.atomic_exchange_u64(voidptr(addr), val)
}

fn custom_swap_u64(addr &u64, val u64) {
	_ = atomics.swap_u64(addr, val)
}

fn std_cas_u64(addr &u64, val u64) {
	mut expected := u64(0)
	_ = C.atomic_compare_exchange_strong_u64(voidptr(addr), voidptr(&expected), val)
}

fn custom_cas_u64(addr &u64, val u64) {
	_ = atomics.cas_u64(addr, 0, val)
}

fn std_store_u32(addr &u32, val u32) {
	C.atomic_store_u32(voidptr(addr), val)
}

fn custom_store_u32(addr &u32, val u32) {
	atomics.store_u32(addr, val)
}

fn std_load_u32(addr &u32, _ u32) {
	_ = C.atomic_load_u32(voidptr(addr))
}

fn custom_load_u32(addr &u32, _ u32) {
	_ = atomics.load_u32(addr)
}

fn std_add_u32(addr &u32, delta u32) {
	_ = C.atomic_fetch_add_u32(voidptr(addr), delta)
}

fn custom_add_u32(addr &u32, delta u32) {
	_ = atomics.add_u32(addr, delta)
}

fn std_swap_u32(addr &u32, val u32) {
	_ = C.atomic_exchange_u32(voidptr(addr), val)
}

fn custom_swap_u32(addr &u32, val u32) {
	_ = atomics.swap_u32(addr, val)
}

fn std_cas_u32(addr &u32, val u32) {
	mut expected := u32(0)
	_ = C.atomic_compare_exchange_strong_u32(voidptr(addr), voidptr(&expected), val)
}

fn custom_cas_u32(addr &u32, val u32) {
	_ = atomics.cas_u32(addr, 0, val)
}

fn std_store_i64(addr &i64, val i64) {
	unsafe { C.atomic_store_u64(voidptr(addr), u64(val)) }
}

fn custom_store_i64(addr &i64, val i64) {
	atomics.store_i64(addr, val)
}

fn std_load_i64(addr &i64, _ i64) {
	unsafe {
		_ = C.atomic_load_u64(voidptr(addr))
	}
}

fn custom_load_i64(addr &i64, _ i64) {
	_ = atomics.load_i64(addr)
}

fn std_add_i64(addr &i64, delta i64) {
	unsafe {
		_ = C.atomic_fetch_add_u64(voidptr(addr), u64(delta))
	}
}

fn custom_add_i64(addr &i64, delta i64) {
	_ = atomics.add_i64(addr, delta)
}

fn std_swap_i64(addr &i64, val i64) {
	unsafe {
		_ = C.atomic_exchange_u64(voidptr(addr), u64(val))
	}
}

fn custom_swap_i64(addr &i64, val i64) {
	_ = atomics.swap_i64(addr, val)
}

fn std_cas_i64(addr &i64, val i64) {
	unsafe {
		mut expected := u64(0)
		_ = C.atomic_compare_exchange_strong_u64(voidptr(addr), voidptr(&expected), u64(val))
	}
}

fn custom_cas_i64(addr &i64, val i64) {
	_ = atomics.cas_i64(addr, 0, val)
}

fn std_store_i32(addr &i32, val i32) {
	unsafe { C.atomic_store_u32(voidptr(addr), u32(val)) }
}

fn custom_store_i32(addr &i32, val i32) {
	atomics.store_i32(addr, val)
}

fn std_load_i32(addr &i32, _ i32) {
	unsafe {
		_ = C.atomic_load_u32(voidptr(addr))
	}
}

fn custom_load_i32(addr &i32, _ i32) {
	_ = atomics.load_i32(addr)
}

fn std_add_i32(addr &i32, delta i32) {
	unsafe {
		_ = C.atomic_fetch_add_u32(voidptr(addr), u32(delta))
	}
}

fn custom_add_i32(addr &i32, delta i32) {
	_ = atomics.add_i32(addr, delta)
}

fn std_swap_i32(addr &i32, val i32) {
	unsafe {
		_ = C.atomic_exchange_u32(voidptr(addr), u32(val))
	}
}

fn custom_swap_i32(addr &i32, val i32) {
	_ = atomics.swap_i32(addr, val)
}

fn std_cas_i32(addr &i32, val i32) {
	unsafe {
		mut expected := u32(0)
		_ = C.atomic_compare_exchange_strong_u32(voidptr(addr), voidptr(&expected), u32(val))
	}
}

fn custom_cas_i32(addr &i32, val i32) {
	_ = atomics.cas_i32(addr, 0, val)
}

fn main() {
	bench_u64('u64 store std', std_store_u64, iterations)
	bench_u64('u64 store custom', custom_store_u64, iterations)

	bench_u64('u64 load std', std_load_u64, iterations)
	bench_u64('u64 load custom', custom_load_u64, iterations)

	bench_u64('u64 add std', std_add_u64, iterations)
	bench_u64('u64 add custom', custom_add_u64, iterations)

	bench_u64('u64 swap std', std_swap_u64, iterations)
	bench_u64('u64 swap custom', custom_swap_u64, iterations)

	bench_u64('u64 cas std', std_cas_u64, iterations)
	bench_u64('u64 cas custom', custom_cas_u64, iterations)

	println('')

	bench_u32('u32 store std', std_store_u32, iterations)
	bench_u32('u32 store custom', custom_store_u32, iterations)

	bench_u32('u32 load std', std_load_u32, iterations)
	bench_u32('u32 load custom', custom_load_u32, iterations)

	bench_u32('u32 add std', std_add_u32, iterations)
	bench_u32('u32 add custom', custom_add_u32, iterations)

	bench_u32('u32 swap std', std_swap_u32, iterations)
	bench_u32('u32 swap custom', custom_swap_u32, iterations)

	bench_u32('u32 cas std', std_cas_u32, iterations)
	bench_u32('u32 cas custom', custom_cas_u32, iterations)

	println('')

	bench_i64('i64 store std', std_store_i64, iterations)
	bench_i64('i64 store custom', custom_store_i64, iterations)

	bench_i64('i64 load std', std_load_i64, iterations)
	bench_i64('i64 load custom', custom_load_i64, iterations)

	bench_i64('i64 add std', std_add_i64, iterations)
	bench_i64('i64 add custom', custom_add_i64, iterations)

	bench_i64('i64 swap std', std_swap_i64, iterations)
	bench_i64('i64 swap custom', custom_swap_i64, iterations)

	bench_i64('i64 cas std', std_cas_i64, iterations)
	bench_i64('i64 cas custom', custom_cas_i64, iterations)

	println('')

	bench_i32('i32 store std', std_store_i32, iterations)
	bench_i32('i32 store custom', custom_store_i32, iterations)

	bench_i32('i32 load std', std_load_i32, iterations)
	bench_i32('i32 load custom', custom_load_i32, iterations)

	bench_i32('i32 add std', std_add_i32, iterations)
	bench_i32('i32 add custom', custom_add_i32, iterations)

	bench_i32('i32 swap std', std_swap_i32, iterations)
	bench_i32('i32 swap custom', custom_swap_i32, iterations)

	bench_i32('i32 cas std', std_cas_i32, iterations)
	bench_i32('i32 cas custom', custom_cas_i32, iterations)
}
