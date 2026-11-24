module main

import builtin.closure
import context
import sync.stdatomic
import time

#include "@VEXEROOT/vlib/builtin/testdata/freestanding_closures/permissions.h"

fn C.v_closure_test_permissions(voidptr) i32

__global closure_startup_callback fn () int = captured(123)
__global closure_execute_stop u64
__global closure_execute_calls u64

fn require_closure(condition bool) {
	if !condition { exit(91) }
}

fn captured(value int) fn () int {
	return fn [value] () int { return value * 3 + 1 }
}

fn execute_while_allocating(callback fn () int) {
	for stdatomic.load_u64(&closure_execute_stop) == 0 {
		require_closure(callback() == 22)
		stdatomic.add_u64(&closure_execute_calls, 1)
	}
}

fn exercise_pool(seed int) int {
	mut callbacks := []fn () int{cap: 4096}
	for i in 0 .. 4096 { callbacks << captured(seed + i) }
	mut sum := 0
	for i, callback in callbacks {
		value := callback()
		require_closure(value == (seed + i) * 3 + 1)
		sum += value
	}
	return sum
}

fn exercise_lifetime() {
	mut lifetime := closure.new_lifetime()
	for i in 0 .. 256 {
		lifetime.frame(fn [i] () {
			callback := captured(i)
			require_closure(callback() == i * 3 + 1)
		}) or { panic(err) }
		lifetime.reclaim_all() or { panic(err) }
	}
	lifetime.dispose() or { panic(err) }
}

fn exercise_reclaimed_slots() {
	mut lifetime := closure.new_lifetime()
	mut addresses := []voidptr{cap: 2048}
	first := fn [mut addresses] () {
		for i in 0 .. 2048 {
			callback := captured(i)
			require_closure(callback() == i * 3 + 1)
			addresses << unsafe { voidptr(callback) }
		}
	}
	second := fn [mut addresses] () {
		for i in 0 .. addresses.len {
			callback := captured(i + 100)
			require_closure(callback() == (i + 100) * 3 + 1)
			require_closure(unsafe { voidptr(callback) } == addresses[addresses.len - i - 1])
		}
	}
	// Stage the two borrowed work callbacks before either tracked frame, so the
	// second work callback does not itself consume a reclaimed slot.
	lifetime.frame(first) or { panic(err) }
	lifetime.reclaim_all() or { panic(err) }
	// Reclamation must return every managed slot, including page boundaries.
	// Slots are a LIFO pool. No reclaimed callback is executed here.
	lifetime.frame(second) or { panic(err) }
	lifetime.reclaim_all() or { panic(err) }
	lifetime.dispose() or { panic(err) }
}

fn wrong_thread_lifetime(mut lifetime closure.Lifetime) bool {
	lifetime.reclaim_all() or {
		return err.msg() == 'closure lifetime used from a different thread'
	}
	return false
}

fn exercise_thread_identity() {
	mut lifetime := &closure.Lifetime{}
	lifetime.reclaim_all() or { panic(err) }
	task := spawn wrong_thread_lifetime(mut lifetime)
	require_closure(task.wait())
	lifetime.dispose() or { panic(err) }
}

fn exercise_context() {
	mut background := context.background()
	mut ctx, cancel := context.with_timeout(mut background, 10 * time.millisecond)
	done := ctx.done()
	select {
		_ := <-done {
		}
		2 * time.second {
			exit(92)
		}
	}
	require_closure(ctx.err().msg() == 'context deadline exceeded')
	cancel()
}

fn main() {
	// Calls must work before any explicit thread runtime activity, including
	// callbacks created while module globals were being initialized.
	require_closure(closure_startup_callback() == 370)
	callback := captured(7)
	require_closure(callback() == 22)
	$if linux {
		check_linux_permissions(callback)
	}
	$if macos {
		check_macos_permissions(callback)
		for i, byte in closure.closure_thunk {
			require_closure(unsafe { &u8(voidptr(callback))[i] } == byte)
		}
	}
	exercise_lifetime()
	exercise_reclaimed_slots()
	exercise_thread_identity()
	executor := spawn execute_while_allocating(callback)
	for stdatomic.load_u64(&closure_execute_calls) == 0 {
	}
	mut workers := []thread int{}
	for seed in 0 .. 4 { workers << spawn exercise_pool(seed * 10000) }
	values := workers.wait()
	require_closure(values.len == 4)
	stdatomic.store_u64(&closure_execute_stop, 1)
	executor.wait()
	$if !closure_primitive_only ? {
		for _ in 0 .. 4 { exercise_context() }
	}
	println('freestanding captured closures passed')
}
