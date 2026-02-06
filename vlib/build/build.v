module build

import os

@[heap; noinit]
pub struct BuildContext {
mut:
	// should_run caches the result of should_run from tasks.
	should_run map[string]bool
	tasks      []Task
pub mut:
	// default is the default task to run when no others are provided.
	default ?string
}

@[heap; noinit]
pub struct Task {
	run        fn (Task) !     @[required]
	should_run fn (Task) !bool @[required]
	// repeatable controls whether or not this task can run multiple times per build cycle
	repeatable bool
pub:
	name    string
	help    string
	depends []string
mut:
	did_run bool
}

@[params]
pub struct BuildContextParams {
pub:
	default ?string
}

@[params]
pub struct TaskParams {
pub:
	name       string @[required]
	help       string
	depends    []string
	should_run fn (Task) !bool = |self| true
	run        fn (Task) ! @[required]
	// repeatable controls whether or not this task can run multiple times per build cycle
	repeatable bool
}

@[params]
pub struct ArtifactParams {
pub:
	name       string @[required]
	help       string
	depends    []string
	should_run fn (Task) !bool = |self| !os.exists(self.name)
	run        fn (Task) ! @[required]
	// repeatable controls whether or not this task can run multiple times per build cycle
	repeatable bool
}

// context creates an empty BuildContext.
pub fn context(params BuildContextParams) BuildContext {
	return BuildContext{
		default: params.default
	}
}

// task creates a task for the given context.
pub fn (mut context BuildContext) task(config TaskParams) {
	if context.get_task(config.name) != none {
		eprintln('error: task already exists with name `${config.name}`')
		exit(1)
	}
	context.tasks << Task{
		should_run: config.should_run
		run:        config.run
		name:       config.name
		help:       config.help
		depends:    config.depends
	}
}

// artifact creates an artifact task for the given context.
pub fn (mut context BuildContext) artifact(config ArtifactParams) {
	if context.get_task(config.name) != none {
		eprintln('error: task already exists with name `${config.name}`')
		exit(1)
	}
	context.tasks << Task{
		should_run: config.should_run
		run:        config.run
		name:       config.name
		help:       config.help
		depends:    config.depends
		repeatable: config.repeatable
	}
}

// get_task gets the task with the given name.
pub fn (mut context BuildContext) get_task(name string) ?&Task {
	for mut task in context.tasks {
		if task.name == name {
			return mut task
		}
	}
	return none
}

// exec executes the task with the given name in the context.
pub fn (mut context BuildContext) exec(name string) {
	if mut task := context.get_task(name) {
		task.exec(mut context)
	} else {
		eprintln('error: no such task: ${name}')
		exit(1)
	}
}

// exec runs the given task and its dependencies.
pub fn (mut task Task) exec(mut context BuildContext) {
	if task.did_run && !task.repeatable {
		println(': ${task.name} (skipped)')
		return
	}

	if task.name !in context.should_run {
		context.should_run[task.name] = task.should_run(task) or {
			eprintln('error: failed to call should_run for task `${task.name}`: ${err}')
			exit(1)
		}
	}

	if !context.should_run[task.name] {
		println(': ${task.name} (skipped)')
		return
	}

	for dep in task.depends {
		if dep == task.name {
			eprintln('error: cyclic task dependency detected, `${task.name}` depends on itself')
			exit(1)
		}

		context.exec(dep)
	}
	println(': ${task.name}')
	task.did_run = true
	task.run(task) or {
		eprintln('error: failed to run task `${task.name}`: ${err}')
		exit(1)
	}
}

// run executes all tasks provided through os.args.
pub fn (mut context BuildContext) run() {
	// filter out options
	mut tasks := os.args[1..].filter(|it| !it.starts_with('-'))

	// check options
	if '--tasks' in os.args || '-tasks' in os.args {
		println('Tasks:')
		for _, task in context.tasks {
			println('- ${task.name}: ${task.help}')
		}
		return
	}

	if tasks.len == 0 {
		if context.default != none {
			tasks << context.default
		} else {
			eprintln('error: no task provided, run with `--tasks` for a list')
			exit(1)
		}
	}

	// execute tasks
	for arg in tasks {
		context.exec(arg)
	}
}
