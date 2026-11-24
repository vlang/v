// Verifies that the `arena` module works under -prealloc via prealloc scopes.
// Run with: v -prealloc -gc none test vlib/arena/arena_prealloc_test.v
import arena
import os

fn runtime_len(base int) int {
	return base + os.args.len - os.args.len
}

fn test_arena_over_prealloc_scope_lifecycle() {
	$if prealloc {
		mut a := arena.new()
		a.push()
		assert a.is_current()
		value := 'x'.repeat(runtime_len(100))
		assert value.len == 100
		assert a.owns(value.str)
		used := a.used()
		assert used > 0
		capacity := a.capacity()
		assert capacity >= used
		a.pop()
		assert !a.is_current()
		// Popped memory stays valid until reset.
		assert value[0] == `x`

		a.reset()
		assert a.used() == 0
		assert a.capacity() > 0

		a.push()
		again := 'y'.repeat(runtime_len(100))
		assert a.owns(again.str)
		assert again[0] == `y`
		a.pop()
		a.free()
	}
}

fn test_nested_arenas_over_prealloc_scopes() {
	$if prealloc {
		mut outer := arena.new()
		mut inner := arena.new()
		outer.push()
		first := 'a'.repeat(runtime_len(32))
		assert outer.owns(first.str)
		inner.push()
		assert inner.is_current()
		second := 'b'.repeat(runtime_len(32))
		assert inner.owns(second.str)
		inner.pop()
		assert outer.is_current()
		assert second[0] == `b`
		outer.pop()
		inner.free()
		outer.free()
	}
}
