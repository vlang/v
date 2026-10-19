module build

import os

// A counter that task bodies and should_run predicates can bump, so a test can
// assert how many times each ran rather than what was printed.
struct Counter {
mut:
	n int
}

fn (mut c Counter) bump() {
	c.n += 1
}

fn body(mut c Counter) fn (Task) ! {
	return fn [mut c] (t Task) ! {
		c.bump()
	}
}

fn truthy(mut c Counter, answer bool) fn (Task) !bool {
	return fn [mut c, answer] (t Task) !bool {
		c.bump()
		return answer
	}
}

const artifact_name = 'v_testcov_build_artifact.txt'

fn test_context_remembers_its_default_task() {
	ctx := context(default: 'only')
	assert ctx.default? == 'only'
	empty := context()
	assert empty.default == none
}

fn test_registered_tasks_are_found_by_name() {
	mut ctx := context()
	ctx.task(
		name:    'build'
		help:    'compile it'
		depends: ['fetch']
		run:     fn (t Task) ! {}
	)
	if t := ctx.get_task('build') {
		assert t.name == 'build'
		assert t.help == 'compile it'
		assert t.depends == ['fetch']
	} else {
		assert false, 'get_task must find a registered task'
	}
	if _ := ctx.get_task('missing') {
		assert false, 'get_task must return none for an unknown name'
	}
}

// A recorder that accumulates the order tasks ran in.
struct Order {
mut:
	names []string
}

fn (mut o Order) add(name string) {
	o.names << name
}

fn test_dependencies_run_before_their_dependents() {
	mut order := &Order{}
	mut ctx := context()
	ctx.task(
		name:    'parent'
		depends: ['first', 'second']
		run:     fn [mut order] (t Task) ! {
			order.add(t.name)
		}
	)
	ctx.task(
		name: 'first'
		run:  fn [mut order] (t Task) ! {
			order.add(t.name)
		}
	)
	ctx.task(
		name: 'second'
		run:  fn [mut order] (t Task) ! {
			order.add(t.name)
		}
	)
	ctx.exec('parent')
	assert order.names == ['first', 'second', 'parent']
}

fn test_should_run_false_skips_the_body_but_walks_the_graph() {
	mut skipped := &Counter{}
	mut ran := &Counter{}
	mut ctx := context()
	ctx.task(
		name:       'gate'
		should_run: truthy(mut skipped, false)
		run:        body(mut ran)
	)
	ctx.task(
		name:    'after'
		depends: ['gate']
		run:     body(mut ran)
	)
	ctx.exec('after')
	// the gate's predicate ran once and its body never did; `after` still ran
	assert skipped.n == 1
	assert ran.n == 1
}

fn test_should_run_is_evaluated_once_per_task_name() {
	mut calls := &Counter{}
	mut ctx := context()
	ctx.task(
		name:       'gate'
		should_run: truthy(mut calls, true)
		run:        fn (t Task) ! {}
	)
	ctx.task(
		name:    'one'
		depends: ['gate']
		run:     fn (t Task) ! {}
	)
	ctx.task(
		name:    'two'
		depends: ['gate']
		run:     fn (t Task) ! {}
	)
	ctx.exec('one')
	ctx.exec('two')
	assert calls.n == 1
}

fn test_a_task_that_already_ran_is_not_run_again() {
	mut ran := &Counter{}
	mut ctx := context()
	ctx.task(
		name: 'once'
		run:  body(mut ran)
	)
	ctx.exec('once')
	ctx.exec('once')
	assert ran.n == 1
}

fn test_repeatable_is_forwarded_by_tasks_and_artifacts() {
	mut task_runs := &Counter{}
	mut ctx := context()
	ctx.task(
		name:       't'
		repeatable: true
		run:        body(mut task_runs)
	)
	ctx.exec('t')
	ctx.exec('t')
	assert task_runs.n == 2

	mut art_runs := &Counter{}
	mut actx := context()
	actx.artifact(
		name:       'a'
		repeatable: true
		run:        body(mut art_runs)
	)
	actx.exec('a')
	actx.exec('a')
	assert art_runs.n == 2
}

fn test_artifact_default_should_run_is_file_absence() {
	abs_path := os.join_path(os.vtmp_dir(), '${artifact_name}_${os.getpid()}')
	os.rm(abs_path) or {}
	defer {
		os.rm(abs_path) or {}
	}
	mut ran := &Counter{}
	mut ctx := context()
	ctx.artifact(
		name: abs_path
		run:  body(mut ran)
	)
	// the file does not exist yet, so the artifact is produced
	ctx.exec(abs_path)
	assert ran.n == 1
	os.write_file(abs_path, 'already here') or { panic(err) }
	// now that it exists, should_run says no
	ctx.exec(abs_path)
	assert ran.n == 1
}

fn test_run_falls_back_to_the_default_task() {
	mut ran := &Counter{}
	mut ctx := context(default: 'main')
	ctx.task(
		name: 'main'
		run:  body(mut ran)
	)
	// run() takes its task names from os.args, and a test binary's args hold
	// neither a task name nor an option, so the default is the one that runs
	ctx.run()
	assert ran.n == 1
}
