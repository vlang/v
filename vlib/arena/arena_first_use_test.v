// vtest vflags: -gc none
import arena
import os
import v.cmdexec

const arena_publication_probe = '--vtest-arena-publication'

fn create_first_arena(allocating chan bool, published chan bool) {
	_ = <-allocating
	mut a := arena.new()
	published.close()
	a.push()
	text := 'testing arena'.repeat(10)
	if text.len != 130 {
		panic('unexpected repeated string length')
	}
	// Use a runtime allocation to check that the published hooks dispatch to the arena.
	block := unsafe { malloc(32) }
	if !a.owns(block) {
		panic('allocation after publication did not use the arena')
	}
	a.pop()
	a.free()
}

fn exercise_heap_allocator() {
	for _ in 0 .. 256 {
		// Call the allocator entry points that dispatch through the hooks.
		unsafe {
			p := malloc(32)
			free(p)
		}
	}
}

fn testsuite_begin() {
	if os.args.len != 2 || os.args[1] != arena_publication_probe {
		return
	}
	allocating := chan bool{cap: 1}
	published := chan bool{}
	worker := spawn create_first_arena(allocating, published)
	// Exercise the heap before publication, while it is pending, and after it.
	exercise_heap_allocator()
	allocating <- true
	for !published.closed {
		exercise_heap_allocator()
	}
	exercise_heap_allocator()
	worker.wait()
	exit(0)
}

fn test_first_arena_publication_during_heap_allocations() {
	// The hooks can only be published once per process.
	for _ in 0 .. 3 {
		result := cmdexec.run_with_timeout(os.executable(), [arena_publication_probe], 15_000)
		assert result.exit_code == 0, result.output
	}
}
