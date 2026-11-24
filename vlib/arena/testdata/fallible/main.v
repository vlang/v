module main

import arena
import os

fn allocate(allocator arena.Allocator, size int) ?&u8 {
	return unsafe { allocator.try_calloc(size) }
}

fn require(ok bool, message string) {
	if !ok { panic(message) }
}

fn main() {
	mut parent := arena.new(chunk_size: 128)
	selected := parent.allocator()
	if pointer := allocate(selected, 0) {
		require(isnil(pointer), 'zero result was not nil')
	} else {
		panic('zero allocation failed')
	}
	require(parent.capacity() == 0 && parent.used() == 0, 'zero allocated storage')
	if _ := allocate(selected, -1) { panic('negative size succeeded') }
	pointer := allocate(selected, 64) or { panic('inactive allocation failed') }
	unsafe {
		for i in 0 .. 64 {
			require(pointer[i] == 0, 'allocation not zeroed')
			pointer[i] = u8(i + 1)
		}
	}
	used, capacity := parent.used(), parent.capacity()
	if _ := allocate(selected, max_int) { panic('impossible size succeeded') }
	require(parent.used() == used && parent.capacity() == capacity, 'failure changed arena storage')
	unsafe {
		for i in 0 .. 64 { require(pointer[i] == u8(i + 1), 'failure changed existing data') }
	}
	parent.push()
	mut child := arena.new(chunk_size: 128)
	child.push()
	before := child.used()
	other := allocate(selected, 16) or { panic('ancestor allocation failed') }
	require(parent.owns(other) && !child.owns(other), 'wrong allocation owner')
	heap := allocate(arena.heap_allocator(), 16) or { panic('heap allocation failed') }
	require(!parent.owns(heap) && !child.owns(heap), 'heap allocation used an arena')
	unsafe { free(heap) }
	require(child.is_current() && child.used() == before, 'explicit allocation changed current allocator')
	child.pop()
	parent.pop()
	child.free()
	parent.reset()
	again := allocate(selected, 64) or { panic('reset arena not reusable') }
	unsafe {
		for i in 0 .. 64 { require(again[i] == 0, 'reset storage not zeroed') }
	}
	parent.free()
	if os.args.len > 1 && os.args[1] == 'oom' {
		$if linux && (amd64 || arm64) {
			// A real request beyond Linux userspace, with no integer overflow.
			mut huge := arena.new(chunk_size: max_int / 4)
			if _ := allocate(huge.allocator(), 8) { panic('impossible mapping succeeded') }
			require(huge.used() == 0 && huge.capacity() == 0, 'OOM changed arena storage')
			huge.push()
			huge.pop()
			huge.free()
			mut next := arena.new(chunk_size: 128)
			_ = allocate(next.allocator(), 8) or { panic('allocation after OOM failed') }
			next.free()
		} $else {
			panic('real mapping failure case requires 64-bit Linux')
		}
	}
	println('fallible allocations passed')
}
