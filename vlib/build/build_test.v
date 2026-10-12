module build

struct RunCounter {
mut:
	runs []string
}

fn (mut c RunCounter) record(t Task) ! {
	c.runs << t.name
}

// test_task_repeatable_runs_once_per_dependent checks that a task declared
// with `repeatable: true` runs again for every task that depends on it,
// instead of being skipped after its first run.
// See https://github.com/vlang/v/issues/30140
fn test_task_repeatable_runs_once_per_dependent() {
	mut counter := &RunCounter{}
	mut ctx := context()
	ctx.task(name: 'a', depends: ['dep'], run: counter.record)
	ctx.task(name: 'b', depends: ['dep'], run: counter.record)
	ctx.task(name: 'dep', repeatable: true, run: counter.record)
	ctx.exec('a')
	ctx.exec('b')
	assert counter.runs == ['dep', 'a', 'dep', 'b']
}

// test_task_non_repeatable_runs_once checks the default: a task without
// `repeatable: true` runs at most once per build cycle, even when several
// tasks depend on it.
fn test_task_non_repeatable_runs_once() {
	mut counter := &RunCounter{}
	mut ctx := context()
	ctx.task(name: 'a', depends: ['dep'], run: counter.record)
	ctx.task(name: 'b', depends: ['dep'], run: counter.record)
	ctx.task(name: 'dep', run: counter.record)
	ctx.exec('a')
	ctx.exec('b')
	assert counter.runs == ['dep', 'a', 'b']
}

fn test_repeatable_task_runs_when_requested_directly() {
	mut counter := &RunCounter{}
	mut ctx := context()
	ctx.task(name: 'repeat', repeatable: true, run: counter.record)
	ctx.exec('repeat')
	ctx.exec('repeat')
	assert counter.runs == ['repeat', 'repeat']
}

fn test_repeatable_task_still_honors_should_run() {
	mut counter := &RunCounter{}
	mut ctx := context()
	ctx.task(name: 'skip', repeatable: true, should_run: |self| false, run: counter.record)
	ctx.exec('skip')
	ctx.exec('skip')
	assert counter.runs.len == 0
}
