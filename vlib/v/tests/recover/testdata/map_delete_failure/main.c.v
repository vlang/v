module main

import os

#include "@DIR/deny.h"

fn C.map_delete_arm_on(int)
fn C.map_delete_disarm() int

@[aligned: 64]
struct Cell {
	value int
}

struct Attempt {
mut:
	message string
}

fn remove(mut items map[int]Cell, key int, mut attempt Attempt, fail_at int) {
	defer {
		if msg := recover() { attempt.message = msg }
	}
	C.map_delete_arm_on(fail_at)
	items.delete(key)
}

// Check both lookup directions and iteration order. All allocations used by
// the assertions run after disarming the allocator.
fn check(items map[int]Cell, order []int) {
	assert items.len == order.len
	mut index := 0
	for key, item in items {
		assert key == order[index]
		assert item.value == key * 3 + 1
		stored := unsafe { voidptr(&items[key]) }
		assert uint(stored) % 64 == 0
		index++
	}
	assert index == order.len
	for key in 0 .. 68 {
		assert (key in items) == (key in order)
		if key in items { assert items[key].value == key * 3 + 1 }
	}
}

fn main() {
	mut items := map[int]Cell{}
	mut order := []int{}
	for key in 0 .. 67 {
		items[key] = Cell{ value: key * 3 + 1 }
		order << key
	}
	mut attempt := Attempt{}
	if os.args[1] == 'first-hole' {
		remove(mut items, 10, mut attempt, 1)
		denied := C.map_delete_disarm()
		assert denied == 1
		assert attempt.message == 'memory allocation failure'
		check(items, order)
		items.delete(10)
		order.delete(10)
		check(items, order)
	} else {
		// The 33rd non-tail deletion is the first compaction for 67 slots.
		for key in 0 .. 32 { items.delete(key * 2) }
		before := order.filter(it % 2 == 1 || it >= 64)
		kept := before.filter(it != 64)
		mut succeeded := false
		for position in 1 .. 16 {
			attempt.message = ''
			remove(mut items, 64, mut attempt, position)
			denied := C.map_delete_disarm()
			if denied == 1 {
				assert attempt.message == 'memory allocation failure'
				check(items, before)
			} else {
				assert attempt.message == ''
				check(items, kept)
				succeeded = true
				break
			}
		}
		assert succeeded
		items.delete(64)
		check(items, kept)
	}
	items[67] = Cell{ value: 202 }
	assert items[67].value == 202
}
