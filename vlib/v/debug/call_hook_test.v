module debug

struct HookRecorder {
pub mut:
	before []string
	after  []string
}

// A hook is registered by its own fn value, so `remove_*` can take the same
// function object back out without an index that could shift under it.
fn test_add_and_remove_call_hooks_track_the_registered_functions() {
	mut recorder := &HookRecorder{}
	before := add_before_call(fn [mut recorder] (name string) {
		unsafe {
			recorder.before << name
		}
	})
	after := add_after_call(fn [mut recorder] (name string) {
		unsafe {
			recorder.after << name
		}
	})
	defer {
		remove_before_call(before)
		remove_after_call(after)
	}
	assert g_trace.trace_before_call.len == 1
	assert g_trace.trace_after_call.len == 1
	assert g_trace.trace_before_call[0] == before
	assert g_trace.trace_after_call[0] == after
	// Registering the same function twice records it twice.
	add_before_call(before)
	assert g_trace.trace_before_call.len == 2
	remove_before_call(before)
	assert g_trace.trace_before_call.len == 1
	before_call_hook('outer')
	after_call_hook('outer')
	assert recorder.before == ['outer']
	assert recorder.after == ['outer']
	// A hook that is not registered is absent from the list entirely.
	remove_before_call(before)
	remove_after_call(after)
	assert g_trace.trace_before_call.len == 0
	assert g_trace.trace_after_call.len == 0
	// Removing one that is not there is a no-op.
	remove_before_call(before)
	remove_after_call(after)
	assert g_trace.trace_before_call.len == 0
	assert g_trace.trace_after_call.len == 0
}

// Two hooks of the same shape are different values, so removing one leaves the
// other registered.
fn test_remove_call_hook_only_removes_the_one_that_was_registered() {
	mut first := &HookRecorder{}
	mut second := &HookRecorder{}
	one := add_before_call(fn [mut first] (name string) {
		unsafe {
			first.before << name
		}
	})
	two := add_before_call(fn [mut second] (name string) {
		unsafe {
			second.before << name
		}
	})
	defer {
		remove_before_call(one)
		remove_before_call(two)
	}
	assert one != two
	remove_before_call(one)
	assert g_trace.trace_before_call.len == 1
	assert g_trace.trace_before_call[0] == two
	before_call_hook('only_the_second')
	assert first.before == []
	assert second.before == ['only_the_second']
}

// `callstack` is the frame list the `bt` command reads, and a depth past the end
// has to be none rather than a frame from beyond the stack.
fn test_callstack_returns_none_past_the_recorded_frames() {
	g_callstack << FnTrace{
		name: 'main.main'
		file: 'main.v'
		line: 12
	}
	g_callstack << FnTrace{
		name: 'main.inner'
		file: 'main.v'
		line: 20
	}
	defer {
		g_callstack = []FnTrace{}
	}
	frame := callstack(0) or { panic('the first frame should be readable') }
	assert frame.name == 'main.main'
	assert frame.file == 'main.v'
	assert frame.line == 12
	inner := callstack(1) or { panic('the second frame should be readable') }
	assert inner.name == 'main.inner'
	assert inner.line == 20
	assert callstack(2) == none
	assert callstack(99) == none
	// NOTE: `callstack` only guards the upper bound, so a negative depth reads
	// `g_callstack[-1]` and panics. Left as found.
}
