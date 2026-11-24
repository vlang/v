import arena

struct AllocatorScopeJob {
mut:
	expected arena.Allocator
	nested   arena.Allocator
	bytes    []u8
	calls    int
	message  string
	failure  ?arena.AllocationFailure
}

fn allocator_scope_write(mut job AllocatorScopeJob) {
	assert job.expected.is_current()
	job.bytes = []u8{len: 128, init: u8(index)}
	job.calls++
}

fn allocator_scope_count(mut job AllocatorScopeJob) {
	assert job.expected.is_current()
	job.calls++
}

fn allocator_scope_panic(mut job AllocatorScopeJob) {
	assert job.expected.is_current()
	job.calls++
	panic('allocator scope callback panic')
}

fn allocator_scope_recover(allocator arena.Allocator, mut job AllocatorScopeJob,
	work fn (mut AllocatorScopeJob)) {
	defer {
		if message := recover() {
			job.message = message
			job.failure = arena.allocation_failure()
		}
	}
	unsafe { allocator.run(mut job, work) }
}

fn test_allocator_run_selects_inactive_storage_and_restores_unrelated_arena() {
	mut target := arena.new()
	mut caller := arena.new()
	caller.push()
	mut job := AllocatorScopeJob{ expected: target.allocator() }
	unsafe { target.allocator().run(mut job, allocator_scope_write) }
	assert caller.is_current()
	assert target.owns(job.bytes.data) && !caller.owns(job.bytes.data)
	assert job.bytes[127] == 127 && job.calls == 1
	// An inactive selection must release its active state on return.
	target.reset()
	caller.pop()
	caller.free()
	target.free()
}

fn test_allocator_run_reuses_current_arena_without_popping_it() {
	mut target := arena.new()
	target.push()
	mut job := AllocatorScopeJob{ expected: target.allocator() }
	unsafe { target.allocator().run(mut job, allocator_scope_write) }
	assert target.is_current() && target.owns(job.bytes.data)
	assert job.bytes[17] == 17
	target.pop()
	target.free()
}

fn test_allocator_run_selects_ancestor_and_restores_child() {
	mut parent := arena.new()
	parent.push()
	mut child := arena.new()
	child.push()
	mut job := AllocatorScopeJob{ expected: parent.allocator() }
	unsafe { parent.allocator().run(mut job, allocator_scope_write) }
	assert child.is_current()
	assert parent.owns(job.bytes.data) && !child.owns(job.bytes.data)
	child.pop()
	child.free()
	assert parent.is_current() && job.bytes[42] == 42
	parent.pop()
	parent.free()
}

fn test_allocator_run_heap_output_survives_caller_arena_release() {
	mut caller := arena.new()
	caller.push()
	mut job := AllocatorScopeJob{ expected: arena.heap_allocator() }
	unsafe { arena.heap_allocator().run(mut job, allocator_scope_write) }
	assert caller.is_current() && !caller.owns(job.bytes.data)
	caller.pop()
	caller.free()
	assert job.bytes[127] == 127
	unsafe { job.bytes.free() }
}

fn allocator_scope_nested(mut job AllocatorScopeJob) {
	assert job.expected.is_current()
	outer := job.expected
	nested := job.nested
	job.expected = nested
	unsafe { nested.run(mut job, allocator_scope_write) }
	job.expected = outer
	assert outer.is_current()
	job.calls++
}

fn test_allocator_run_nested_selections_restore_each_caller() {
	mut caller := arena.new()
	mut outer := arena.new()
	mut inner := arena.new()
	caller.push()
	mut job := AllocatorScopeJob{ expected: outer.allocator(), nested: inner.allocator() }
	unsafe { outer.allocator().run(mut job, allocator_scope_nested) }
	assert caller.is_current() && job.calls == 2
	assert inner.owns(job.bytes.data) && !outer.owns(job.bytes.data)
	inner.reset()
	outer.reset()
	caller.pop()
	caller.free()
	outer.free()
	inner.free()
}

fn test_allocator_run_callback_panic_restores_inactive_and_ancestor_scopes() {
	mut target := arena.new()
	mut caller := arena.new()
	caller.push()
	mut first := AllocatorScopeJob{ expected: target.allocator() }
	allocator_scope_recover(target.allocator(), mut first, allocator_scope_panic)
	assert first.message == 'allocator scope callback panic' && caller.is_current()
	target.reset()
	caller.pop()
	target.push()
	caller.push()
	mut second := AllocatorScopeJob{ expected: target.allocator() }
	allocator_scope_recover(target.allocator(), mut second, allocator_scope_panic)
	assert second.message == 'allocator scope callback panic' && caller.is_current()
	caller.pop()
	assert target.is_current()
	target.pop()
	caller.free()
	target.free()
}

fn allocator_scope_denied_descendant(mut job AllocatorScopeJob) {
	assert job.expected.is_current()
	mut child := arena.new(chunk_size: 128)
	defer { child.free() }
	child.push()
	defer { child.pop() }
	job.bytes = []u8{len: 4096}
}

fn test_allocator_run_allocation_failure_restores_scope_and_inherited_budget() {
	mut target := arena.new(limits: arena.ChunkLimits{ max_live_bytes: 0 })
	mut caller := arena.new()
	caller.push()
	mut job := AllocatorScopeJob{ expected: target.allocator() }
	allocator_scope_recover(target.allocator(), mut job, allocator_scope_denied_descendant)
	assert job.message.len > 0 && caller.is_current()
	assert (job.failure or { panic('lost allocation failure') }) == .chunk_limit
	usage := target.chunk_usage() or { panic('lost budget') }
	assert usage.live_bytes == 0 && usage.total_bytes == 0
	// No allocation is needed to reselect the exhausted owner after unwinding.
	unsafe { target.allocator().run(mut job, allocator_scope_count) }
	assert caller.is_current() && job.calls == 1
	target.reset()
	caller.pop()
	caller.free()
	target.free()
}

fn allocator_scope_hold(target &arena.Arena, ready chan bool, release chan bool) {
	target.push()
	ready <- true
	_ := <-release
	target.pop()
}

fn test_allocator_run_rejects_other_thread_without_changing_caller_or_target() {
	mut target := arena.new()
	ready := chan bool{}
	release := chan bool{}
	worker := spawn allocator_scope_hold(&target, ready, release)
	_ := <-ready
	mut caller := arena.new()
	caller.push()
	mut job := AllocatorScopeJob{ expected: target.allocator() }
	allocator_scope_recover(target.allocator(), mut job, allocator_scope_write)
	assert job.message.contains('active outside the current allocator stack')
	assert caller.is_current() && job.calls == 0
	release <- true
	worker.wait()
	unsafe { target.allocator().run(mut job, allocator_scope_write) }
	assert caller.is_current() && target.owns(job.bytes.data) && job.calls == 1
	caller.pop()
	caller.free()
	target.free()
}
