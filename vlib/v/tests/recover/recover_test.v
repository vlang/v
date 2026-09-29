// Tests for `recover()`, which follows Go: a panic runs the deferred blocks up
// the stack, and `recover()` directly in one of them stops the panic.

struct Point {
	x int
	y int
}

fn div(a int, b int) int {
	defer {
		recover()
	}
	if b == 0 {
		panic('division by zero')
	}
	return a / b
}

fn test_recover_returns_zero_value() {
	assert div(10, 2) == 5
	assert div(10, 0) == 0
}

struct Message {
mut:
	text string = 'no panic'
}

fn recovered_message(f fn ()) string {
	mut msg := Message{}
	run_and_recover(f, mut msg)
	return msg.text
}

fn run_and_recover(f fn (), mut msg Message) {
	defer {
		if r := recover() {
			msg.text = r
		}
	}
	f()
}

fn test_recover_returns_the_panic_message() {
	assert recovered_message(fn () {
		panic('boom')
	}) == 'boom'
	assert recovered_message(fn () {}) == 'no panic'
}

fn test_recover_without_panic_is_none() {
	mut got := 'unset'
	{
		defer {
			got = recover() or { 'none' }
		}
	}
	assert got == 'none'
	if _ := recover() {
		assert false
	}
}

fn test_recover_catches_runtime_errors() {
	msg := recovered_message(fn () {
		a := [1, 2, 3]
		i := 5
		println(a[i])
	})
	assert msg.contains('index out of range')
}

fn test_recover_catches_error_panics() {
	msg := recovered_message(fn () {
		panic(error('an error'))
	})
	assert msg == 'an error'
}

fn record(mut log []string, s string) {
	log << s
}

fn inner(mut log []string) {
	defer {
		record(mut log, 'inner defer 1')
	}
	defer {
		record(mut log, 'inner defer 2')
	}
	record(mut log, 'inner body')
	panic('inner panic')
}

fn middle(mut log []string) {
	defer {
		record(mut log, 'middle defer')
	}
	inner(mut log)
	record(mut log, 'unreachable in middle')
}

fn outer(mut log []string) int {
	defer {
		record(mut log, 'outer first defer')
	}
	defer {
		if r := recover() {
			record(mut log, 'recovered: ${r}')
		}
	}
	defer {
		record(mut log, 'outer last defer')
	}
	middle(mut log)
	record(mut log, 'unreachable in outer')
	return 42
}

fn test_panic_runs_deferred_blocks_up_the_stack_in_lifo_order() {
	mut log := []string{}
	res := outer(mut log)
	assert res == 0
	assert log == ['inner body', 'inner defer 2', 'inner defer 1', 'middle defer', 'outer last defer',
		'recovered: inner panic', 'outer first defer']
}

fn test_execution_continues_after_the_recovered_call() {
	mut log := []string{}
	outer(mut log)
	log << 'after'
	assert log.last() == 'after'
}

fn helper_that_recovers(mut log []string) {
	if r := recover() {
		log << 'helper recovered ${r}'
	} else {
		log << 'helper got none'
	}
}

fn recover_only_directly_in_defer(mut log []string) {
	defer {
		helper_that_recovers(mut log)
	}
	panic('not recovered by helper')
}

fn call_recover_only_directly_in_defer(mut log []string) {
	defer {
		if r := recover() {
			log << 'caller recovered ${r}'
		}
	}
	recover_only_directly_in_defer(mut log)
}

fn test_recover_in_a_called_function_has_no_effect() {
	mut log := []string{}
	call_recover_only_directly_in_defer(mut log)
	assert log == ['helper got none', 'caller recovered not recovered by helper']
}

fn repanic() {
	defer {
		if r := recover() {
			panic('again: ${r}')
		}
	}
	panic('first')
}

fn test_panic_in_a_deferred_block_replaces_the_recovered_one() {
	assert recovered_message(repanic) == 'again: first'
}

fn panic_in_defer_without_recover() {
	defer {
		panic('second')
	}
	panic('first')
}

fn test_newer_panic_wins_when_nothing_recovered() {
	assert recovered_message(panic_in_defer_without_recover) == 'second'
}

fn recovers_its_own_panic() string {
	defer {
		recover()
	}
	panic('nested')
	return 'unreachable'
}

fn outer_panic_survives_nested_recovery(mut log []string) {
	defer {
		// A deferred block that runs for a panic can itself call functions that
		// panic and recover; the outer panic stays in flight and can be recovered.
		log << 'nested returned "${recovers_its_own_panic()}"'
		if r := recover() {
			log << 'outer recovered ${r}'
		}
	}
	panic('outer')
}

fn test_nested_panic_and_recovery_inside_a_deferred_block() {
	mut log := []string{}
	outer_panic_survives_nested_recovery(mut log)
	assert log == ['nested returned ""', 'outer recovered outer']
}

fn recover_twice(mut log []string) {
	defer {
		first := recover() or { 'none' }
		second := recover() or { 'none' }
		log << '${first} ${second}'
	}
	panic('once')
}

fn test_second_recover_is_none() {
	mut log := []string{}
	recover_twice(mut log)
	assert log == ['once none']
}

fn later_defers_see_the_recovered_panic_as_gone(mut log []string) {
	defer {
		log << 'earlier defer: ' + (recover() or { 'none' })
	}
	defer {
		log << 'recovering defer: ' + (recover() or { 'none' })
	}
	panic('p')
}

fn test_remaining_deferred_blocks_run_after_recovery() {
	mut log := []string{}
	later_defers_see_the_recovered_panic_as_gone(mut log)
	assert log == ['recovering defer: p', 'earlier defer: none']
}

fn deferred_block_reads_updated_locals(mut log []string) {
	mut counter := 0
	mut name := 'start'
	mut pt := Point{}
	defer {
		recover()
		log << '${counter} ${name} ${pt.x},${pt.y}'
	}
	for i in 0 .. 10 {
		counter += i
	}
	name = 'end'
	pt = Point{3, 4}
	panic('with locals')
}

fn test_deferred_block_sees_locals_changed_after_the_defer() {
	mut log := []string{}
	deferred_block_reads_updated_locals(mut log)
	assert log == ['45 end 3,4']
}

fn scoped_defers(mut log []string) {
	defer {
		if r := recover() {
			log << 'recovered ${r}'
		}
	}
	for i in 0 .. 3 {
		defer {
			log << 'end of iteration ${i}'
		}
		if i == 2 {
			panic('in loop')
		}
	}
}

fn test_block_scoped_defers_in_loops() {
	mut log := []string{}
	scoped_defers(mut log)
	assert log == ['end of iteration 0', 'end of iteration 1', 'end of iteration 2', 'recovered in loop']
}

fn fn_defers(mut log []string) {
	defer {
		if r := recover() {
			log << 'recovered ${r}'
		}
	}
	for i in 0 .. 3 {
		defer(fn) {
			log << 'fn defer'
		}
		defer {
			log << 'iteration ${i}'
		}
	}
	panic('after loop')
}

fn test_function_defers_run_on_panic() {
	mut log := []string{}
	fn_defers(mut log)
	assert log == ['iteration 0', 'iteration 1', 'iteration 2', 'fn defer', 'fn defer', 'fn defer',
		'recovered after loop']
}

fn recover_in_fn_defer(mut log []string, register bool) int {
	if register {
		defer(fn) {
			if r := recover() {
				log << 'recovered ${r}'
			}
		}
	}
	panic('fn defer recovers')
	return 1
}

fn test_recover_in_function_defer() {
	mut log := []string{}
	assert recover_in_fn_defer(mut log, true) == 0
	assert log == ['recovered fn defer recovers']
}

fn zero_string() string {
	defer {
		recover()
	}
	panic('')
	return 'x'
}

fn zero_struct() Point {
	defer {
		recover()
	}
	panic('')
	return Point{1, 2}
}

fn zero_array() []int {
	defer {
		recover()
	}
	panic('')
	return [1]
}

fn zero_option() ?int {
	defer {
		recover()
	}
	panic('')
	return 1
}

fn zero_result() !int {
	defer {
		recover()
	}
	panic('')
	return 1
}

fn zero_multi() (int, string) {
	defer {
		recover()
	}
	panic('')
	return 1, 'x'
}

fn test_recovered_functions_return_zero_values() {
	assert zero_string() == ''
	assert zero_struct() == Point{}
	assert zero_array() == []
	assert (zero_option() or { -1 }) == 0
	assert (zero_result() or { -1 }) == 0
	a, b := zero_multi()
	assert a == 0
	assert b == ''
}

struct Counter {
mut:
	n int
}

fn (mut c Counter) bump_and_panic() {
	defer {
		if _ := recover() {
			c.n += 100
		}
	}
	c.n++
	panic('method')
}

fn test_recover_in_method() {
	mut c := Counter{}
	c.bump_and_panic()
	assert c.n == 101
}

fn generic_recover[T](v T) T {
	defer {
		recover()
	}
	panic('generic')
	return v
}

fn test_recover_in_generic_function() {
	assert generic_recover(5) == 0
	assert generic_recover('s') == ''
}

fn recursive(n int, mut log []string) {
	defer {
		log << 'defer ${n}'
		if n == 0 {
			if r := recover() {
				log << 'recovered at ${n}: ${r}'
			}
		}
	}
	if n == 3 {
		panic('deep')
	}
	recursive(n + 1, mut log)
	log << 'returned to ${n}'
}

fn test_recover_in_recursion_stops_at_the_recovering_call() {
	mut log := []string{}
	recursive(0, mut log)
	assert log == ['defer 3', 'defer 2', 'defer 1', 'defer 0', 'recovered at 0: deep']
}

fn recursive_middle(n int, mut log []string) {
	defer {
		log << 'defer ${n}'
		if n == 1 {
			recover()
		}
	}
	if n == 3 {
		panic('deep')
	}
	recursive_middle(n + 1, mut log)
	log << 'returned to ${n}'
}

fn test_caller_of_the_recovering_call_continues() {
	mut log := []string{}
	recursive_middle(0, mut log)
	assert log == ['defer 3', 'defer 2', 'defer 1', 'returned to 0', 'defer 0']
}

fn closure_in_defer(mut log []string) {
	defer {
		// A closure is a function of its own, so its recover() has no effect.
		f := fn () ?string {
			return recover()
		}
		log << 'closure: ' + (f() or { 'none' })
		log << 'direct: ' + (recover() or { 'none' })
	}
	panic('p')
}

fn test_recover_in_a_closure_inside_defer_has_no_effect() {
	mut log := []string{}
	closure_in_defer(mut log)
	assert log == ['closure: none', 'direct: p']
}

fn worker(id int) string {
	defer {
		recover()
	}
	if id % 2 == 1 {
		panic('odd ${id}')
	}
	return 'ok ${id}'
}

fn test_recover_in_threads() {
	mut threads := []thread string{}
	for i in 0 .. 8 {
		threads << spawn worker(i)
	}
	res := threads.wait()
	assert res == ['ok 0', '', 'ok 2', '', 'ok 4', '', 'ok 6', '']
}

fn test_many_recoveries() {
	mut sum := 0
	for i in 0 .. 10000 {
		sum += div(i, i % 3)
	}
	assert sum > 0
}

fn allocate_then_recover(mut log []string) {
	defer {
		for i in 0 .. 2000 {
			tmp := []int{len: 100, init: index + i}
			assert tmp.len == 100
		}
		gc_collect()
		if r := recover() {
			log << r
		}
	}
	n := 42
	panic('heap message ${n}')
}

fn test_panic_message_survives_allocations_in_deferred_blocks() {
	mut log := []string{}
	allocate_then_recover(mut log)
	assert log == ['heap message 42']
}

fn repeated_fn_defer_panics_on_return(mut log []int) {
	defer {
		recover()
	}
	for _ in 0 .. 3 {
		defer(fn) {
			log << 1
			if log.len == 1 {
				panic('cleanup')
			}
		}
	}
}

fn test_panic_in_a_repeated_function_defer_on_return_still_runs_the_others() {
	mut log := []int{}
	repeated_fn_defer_panics_on_return(mut log)
	assert log.len == 3
}

fn repeated_fn_defer_panics_while_unwinding(mut log []string) {
	defer {
		if r := recover() {
			log << 'recovered ${r}'
		}
	}
	for _ in 0 .. 3 {
		defer(fn) {
			log << 'cleanup'
			if log.len == 1 {
				panic('in cleanup')
			}
		}
	}
	panic('body')
}

fn test_panic_in_a_repeated_function_defer_while_unwinding_still_runs_the_others() {
	mut log := []string{}
	repeated_fn_defer_panics_while_unwinding(mut log)
	assert log == ['cleanup', 'cleanup', 'cleanup', 'recovered in cleanup']
}

fn repeated_fn_defer_recovers(mut log []string) int {
	for _ in 0 .. 3 {
		defer(fn) {
			log << 'run: ' + (recover() or { 'none' })
		}
	}
	panic('p')
	return 1
}

fn test_recover_in_a_repeated_function_defer() {
	mut log := []string{}
	assert repeated_fn_defer_recovers(mut log) == 0
	assert log == ['run: p', 'run: none', 'run: none']
}

fn test_many_threads_that_recover() {
	mut threads := []thread string{}
	for _ in 0 .. 200 {
		threads << spawn worker(1)
	}
	assert threads.wait().all(it == '')
}
