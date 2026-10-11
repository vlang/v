import arena
import strings

struct Point {
	x int
	y int
}

fn test_scope_allocations_come_from_the_arena() {
	mut a := arena.new()
	defer {
		a.free()
	}
	assert !arena.active()
	a.push()
	assert arena.active()
	assert a.is_current()
	s := 'number: ${42}'
	mut arr := []int{}
	for i in 0 .. 1000 {
		arr << i
	}
	mut m := map[string]int{}
	for i in 0 .. 200 {
		m['key${i}'] = i
	}
	mut sb := strings.new_builder(8)
	for i in 0 .. 100 {
		sb.write_string('${i},')
	}
	joined := sb.str()
	p := &Point{1, 2}
	a.pop()
	assert !arena.active()
	assert !a.is_current()
	assert a.owns(s.str)
	assert a.owns(arr.data)
	assert a.owns(joined.str)
	assert a.owns(p)
	assert a.used() > 0
	assert a.used() <= a.capacity()
	// Arena memory stays valid after pop, until the arena is reset or freed.
	assert s == 'number: 42'
	assert arr.len == 1000
	assert arr[999] == 999
	assert m.len == 200
	assert m['key199'] == 199
	assert joined.starts_with('0,1,2,3,')
	assert p.y == 2
	// After pop, allocations use the default allocator again: clone() copies
	// values out of the arena.
	s2 := s.clone()
	arr2 := arr.clone()
	m2 := m.clone()
	joined2 := joined.clone()
	assert !a.owns(s2.str)
	assert !a.owns(arr2.data)
	assert !a.owns(joined2.str)
	a.reset()
	assert a.used() == 0
	assert s2 == 'number: 42'
	assert arr2.len == 1000
	assert arr2[500] == 500
	assert m2.len == 200
	assert m2['key7'] == 7
	assert joined2.starts_with('0,1,2,3,')
}

fn test_nested_scopes() {
	mut outer := arena.new()
	mut inner := arena.new()
	defer {
		inner.free()
		outer.free()
	}
	outer.push()
	s1 := 'outer ${1}'
	inner.push()
	assert inner.is_current()
	assert !outer.is_current()
	s2 := 'inner ${2}'
	inner.pop()
	assert outer.is_current()
	s3 := 'outer ${3}'
	outer.pop()
	assert !arena.active()
	assert outer.owns(s1.str)
	assert !inner.owns(s1.str)
	assert inner.owns(s2.str)
	assert !outer.owns(s2.str)
	assert outer.owns(s3.str)
	inner.reset()
	assert inner.used() == 0
	assert s1 == 'outer 1'
	assert s3 == 'outer 3'
}

fn test_free_is_a_noop_for_arena_memory() {
	heap_ptr := unsafe { malloc(64) }
	mut a := arena.new()
	defer {
		a.free()
	}
	a.push()
	// Memory from the default allocator can still be freed in a scope.
	unsafe { free(heap_ptr) }
	p := unsafe { malloc(64) }
	unsafe { vmemset(p, 0x5a, 64) }
	q := unsafe { malloc(64) }
	unsafe { vmemset(q, 0x33, 64) }
	unsafe { free(p) }
	mut words := ['a', 'b', 'c']
	unsafe { words.free() }
	a.pop()
	// free() after pop is still a no-op for arena memory.
	unsafe { free(q) }
	assert a.owns(p)
	assert a.owns(q)
	unsafe {
		assert p[0] == 0x5a
		assert p[63] == 0x5a
		assert q[0] == 0x33
		assert q[63] == 0x33
	}
}

fn test_realloc_copies_arena_memory_out_after_pop() {
	mut a := arena.new()
	defer {
		a.free()
	}
	a.push()
	p := unsafe { malloc(64) }
	unsafe { vmemset(p, 7, 64) }
	// The newest allocation of the current arena grows in place.
	grown := unsafe { realloc_data(p, 64, 128) }
	assert grown == p
	r := unsafe { malloc(32) }
	unsafe { vmemset(r, 9, 32) }
	mut m := map[int]int{}
	m[-1] = -1
	a.pop()
	q := unsafe { realloc_data(p, 128, 4096) }
	assert q != p
	assert !a.owns(q)
	assert a.owns(p)
	unsafe {
		assert q[0] == 7
		assert q[63] == 7
		free(q)
	}
	// v_realloc does not know the old size; the data is copied all the same.
	r2 := unsafe { v_realloc(r, 100) }
	assert !a.owns(r2)
	unsafe {
		assert r2[0] == 9
		assert r2[31] == 9
		free(r2)
	}
	// A map that was created in the arena keeps working when it grows after pop.
	for i in 0 .. 5000 {
		m[i] = i * 2
	}
	assert m.len == 5001
	assert m[-1] == -1
	assert m[4999] == 9998
}

fn test_v_realloc_of_an_older_block_in_the_current_arena() {
	mut a := arena.new()
	defer {
		a.free()
	}
	a.push()
	p := unsafe { malloc(16) }
	unsafe { vmemset(p, 1, 16) }
	q := unsafe { malloc(16) }
	unsafe { vmemset(q, 2, 16) }
	// `p` is not the newest allocation, so it is copied to a new block of the
	// arena right after `q`; without the old size, the copy must not overlap it.
	r := unsafe { v_realloc(p, 100) }
	a.pop()
	assert r != p
	assert a.owns(r)
	unsafe {
		for i in 0 .. 16 {
			assert r[i] == 1
			assert q[i] == 2
		}
	}
}

fn worker(id int) int {
	if arena.active() {
		// Spawned threads must start with the default allocator.
		return -1
	}
	mut a := arena.new(chunk_size: 4096)
	defer {
		a.free()
	}
	mut total := 0
	for round in 0 .. 50 {
		a.push()
		mut parts := []string{}
		for i in 0 .. 100 {
			parts << 'w${id}-r${round}-i${i}'
		}
		joined := parts.join(',')
		in_arena := a.owns(joined.str) && a.owns(parts.data)
		a.pop()
		if !in_arena {
			return -2
		}
		total += joined.len
		a.reset()
	}
	return total
}

fn expected_worker_total(id int) int {
	mut total := 0
	for round in 0 .. 50 {
		mut parts := []string{}
		for i in 0 .. 100 {
			parts << 'w${id}-r${round}-i${i}'
		}
		total += parts.join(',').len
	}
	return total
}

fn release_elsewhere(p &u8) bool {
	if arena.active() {
		return false
	}
	// Memory of another thread's arena: free() is a no-op, realloc copies it.
	unsafe { free(p) }
	q := unsafe { realloc_data(p, 16, 64) }
	ok := q != p && unsafe { q[0] == 42 && q[15] == 42 }
	unsafe { free(q) }
	return ok
}

fn test_spawned_threads_have_their_own_arenas() {
	mut main_arena := arena.new()
	defer {
		main_arena.free()
	}
	main_arena.push()
	p := unsafe { malloc(16) }
	unsafe { vmemset(p, 42, 16) }
	mut threads := []thread int{}
	for id in 0 .. 4 {
		threads << spawn worker(id)
	}
	other := spawn release_elsewhere(p)
	main_arena.pop()
	results := threads.wait()
	assert other.wait()
	assert results.len == 4
	for id, total in results {
		assert total == expected_worker_total(id)
	}
	assert main_arena.owns(p)
	assert unsafe { p[15] } == 42
}

fn churn_arenas(rounds int) int {
	mut total := 0
	for round in 0 .. rounds {
		mut a := arena.new(chunk_size: 1024)
		a.push()
		mut parts := []string{}
		for i in 0 .. 32 {
			parts << 'r${round}-i${i}-' + 'x'.repeat(i)
		}
		total += parts.join(',').len
		a.pop()
		a.free()
	}
	return total
}

fn release_repeatedly(p &u8, rounds int) bool {
	for _ in 0 .. rounds {
		unsafe { free(p) }
		q := unsafe { realloc_data(p, 16, 64) }
		if q == p || unsafe { q[0] != 42 || q[15] != 42 } {
			return false
		}
		unsafe { free(q) }
	}
	return true
}

fn test_other_threads_recognize_arena_memory_while_arenas_change() {
	mut main_arena := arena.new()
	defer {
		main_arena.free()
	}
	main_arena.push()
	p := unsafe { malloc(16) }
	unsafe { vmemset(p, 42, 16) }
	main_arena.pop()
	// The registry of chunks changes on some threads, while others look up
	// arena memory in it.
	mut churners := []thread int{}
	for _ in 0 .. 3 {
		churners << spawn churn_arenas(100)
	}
	mut releasers := []thread bool{}
	for _ in 0 .. 3 {
		releasers << spawn release_repeatedly(p, 2000)
	}
	totals := churners.wait()
	assert releasers.wait().all(it)
	assert totals.all(it == churn_arenas(100))
	assert main_arena.owns(p)
	assert unsafe { p[15] } == 42
}

fn test_arena_memory_is_reused_in_loops() {
	mut a := arena.new(chunk_size: 4096)
	defer {
		a.free()
	}
	mut warm_capacity := isize(0)
	for i in 0 .. 2000 {
		a.push()
		mut sb := strings.new_builder(64)
		for j in 0 .. 100 {
			sb.write_string('line ${i} ${j}\n')
		}
		s := sb.str()
		in_arena := a.owns(s.str)
		a.pop()
		assert in_arena
		assert s.ends_with('line ${i} 99\n')
		a.reset()
		if i == 20 {
			warm_capacity = a.capacity()
		} else if i > 20 {
			// The kept chunk is big enough: no new memory is needed.
			assert a.capacity() == warm_capacity
		}
	}
	assert warm_capacity > 0
	assert warm_capacity <= 256 * 1024
}

fn test_gc_objects_referenced_from_arena_memory_survive() {
	mut a := arena.new()
	defer {
		a.free()
	}
	a.push()
	mut holder := []&Point{len: 200, init: unsafe { nil }}
	a.pop()
	assert a.owns(holder.data)
	for i in 0 .. holder.len {
		holder[i] = &Point{i, i * 2}
	}
	gc_collect()
	mut garbage := 0
	for i in 0 .. 20000 {
		g := &Point{i, i}
		garbage += g.x
	}
	gc_collect()
	assert garbage > 0
	for i in 0 .. holder.len {
		assert holder[i].x == i
		assert holder[i].y == i * 2
	}
}

struct PanicMessage {
mut:
	text string
}

fn capture_panic(f fn (), mut msg PanicMessage) {
	defer {
		if r := recover() {
			msg.text = r
		}
	}
	f()
}

fn panic_message(f fn ()) string {
	mut msg := PanicMessage{}
	capture_panic(f, mut msg)
	return msg.text
}

fn test_misuse_panics_with_a_clear_message() {
	assert panic_message(fn () {
		mut a := arena.new()
		defer {
			a.free()
		}
		a.pop()
	}).contains('pop() without an active arena')
	assert panic_message(fn () {
		mut a := arena.new()
		mut b := arena.new()
		a.push()
		b.push()
		defer {
			b.pop()
			a.pop()
			b.free()
			a.free()
		}
		a.pop()
	}).contains('not the innermost active arena')
	assert panic_message(fn () {
		mut a := arena.new()
		a.push()
		defer {
			a.pop()
			a.free()
		}
		a.push()
	}).contains('already active')
	assert panic_message(fn () {
		mut a := arena.new()
		a.push()
		defer {
			a.pop()
			a.free()
		}
		a.reset()
	}).contains('reset() of an active arena')
	assert panic_message(fn () {
		mut a := arena.new()
		a.push()
		defer {
			a.pop()
			a.free()
		}
		a.free()
	}).contains('free() of an active arena')
	assert panic_message(fn () {
		mut a := arena.new()
		a.free()
		a.free() // freeing twice is fine
		a.push()
	}).contains('freed')
	assert !arena.active()
}
