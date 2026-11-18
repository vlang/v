import sync
import time

fn test_taskgroup_normal_completion() {
	mut group := sync.new_task_group[int]()
	for i in 0 .. 5 {
		id := i
		group.spawn(fn [id] () !int {
			return id * 2
		}) or { panic('spawn failed: ${err}') }
	}
	results := group.wait() or { panic('wait failed: ${err}') }
	assert results == [0, 2, 4, 6, 8]
}

fn test_taskgroup_result_order() {
	n := 5
	mut group := sync.new_task_group[int]()
	for i in 0 .. n {
		id := i
		// Later tasks finish first; results must still come back in order.
		group.spawn(fn [id, n] () !int {
			time.sleep((n - id) * 20 * time.millisecond)
			return id * 10
		}) or { panic('spawn failed: ${err}') }
	}
	results := group.wait() or { panic('wait failed: ${err}') }
	assert results == [0, 10, 20, 30, 40]
}

fn test_taskgroup_early_failure() {
	mut group := sync.new_task_group[int]()
	shared_ctx := group.context()
	for i in 0 .. 4 {
		id := i
		group.spawn(fn [id, shared_ctx] () !int {
			if id == 0 {
				return error('boom')
			}
			time.sleep(150 * time.millisecond)
			shared_ctx.check_cancelled()!
			return id
		}) or { panic('spawn failed: ${err}') }
	}
	group.wait() or {
		assert err.msg() == 'boom'
		return
	}
	assert false, 'wait must return the first task error'
}

fn test_taskgroup_cancellation() {
	mut group := sync.new_task_group[int]()
	ctx := group.context()
	for i in 0 .. 3 {
		id := i
		group.spawn(fn [id, ctx] () !int {
			time.sleep(300 * time.millisecond)
			ctx.check_cancelled()!
			return id
		}) or { panic('spawn failed: ${err}') }
	}
	time.sleep(50 * time.millisecond)
	group.cancel()
	assert ctx.is_cancelled()
	group.wait() or {
		assert err.msg() == 'sync: task cancelled'
		return
	}
	assert false, 'wait must fail after cancel'
}

fn test_taskgroup_repeated_use() {
	for round in 0 .. 3 {
		base := round * 10
		mut group := sync.new_task_group[int]()
		for i in 0 .. 2 {
			id := base + i
			group.spawn(fn [id] () !int {
				return id
			}) or { panic('spawn failed: ${err}') }
		}
		results := group.wait() or { panic('wait failed: ${err}') }
		assert results == [base, base + 1]
	}
}

fn test_taskgroup_spawn_after_wait() {
	mut group := sync.new_task_group[int]()
	results := group.wait() or { panic('empty wait failed: ${err}') }
	assert results.len == 0
	group.spawn(fn () !int {
		return 1
	}) or {
		assert err.msg() == 'sync: TaskGroup spawn after wait'
		return
	}
	assert false, 'spawn after wait must fail'
}

fn test_taskgroup_double_wait() {
	mut group := sync.new_task_group[int]()
	group.spawn(fn () !int {
		return 1
	}) or { panic('spawn failed: ${err}') }
	first := group.wait() or { panic('first wait failed: ${err}') }
	assert first == [1]
	group.wait() or {
		assert err.msg() == 'sync: TaskGroup wait called twice'
		return
	}
	assert false, 'second wait must fail'
}

fn test_taskgroup_nil_job() {
	mut group := sync.new_task_group[int]()
	group.spawn(unsafe { nil }) or {
		assert err.msg() == 'sync: TaskGroup nil job'
		return
	}
	assert false, 'spawning a nil job must fail'
}
