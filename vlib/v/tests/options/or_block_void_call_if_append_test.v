// The `or {}` block of a call that returns no value only runs statements, so an
// array append in a final `if` without `else` is a statement.
interface Registry {
	has_empty_cells() bool
mut:
	cleanup(fail bool) !
}

struct CellRegistry {
	cells int
}

fn (r CellRegistry) has_empty_cells() bool {
	return r.cells > 0
}

fn (mut r CellRegistry) cleanup(fail bool) ! {
	if fail {
		return error('cleanup failed')
	}
}

struct Port {
mut:
	closed   bool
	messages []string
}

fn post(fail bool) ! {
	if fail {
		return error('post failed')
	}
}

fn maybe_post(fail bool) ? {
	if fail {
		return none
	}
}

fn post_handling_error(mut port Port, fail bool) {
	post(fail) or {
		if !port.closed {
			port.messages << 'error: ${err.msg()}'
		}
	}
	maybe_post(fail) or {
		if !port.closed {
			port.messages << 'none'
		}
	}
}

fn test_append_in_if_of_void_result_or_block() {
	mut port := Port{}
	post_handling_error(mut port, true)
	post_handling_error(mut port, false)
	port.closed = true
	post_handling_error(mut port, true)
	assert port.messages == ['error: post failed', 'none']
}

fn test_append_in_if_of_interface_void_result_or_block() {
	mut jobs := [Registry(CellRegistry{
		cells: 1
	}), Registry(CellRegistry{})]
	mut requeued := []Registry{}
	for jobs.len > 0 {
		mut registry := jobs.pop()
		registry.cleanup(true) or {
			if registry.has_empty_cells() {
				requeued << registry
			}
		}
	}
	assert requeued.len == 1
	assert requeued[0].has_empty_cells()
}
