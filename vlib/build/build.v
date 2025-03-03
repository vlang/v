module build

import os

@[noinit]
pub struct BuildContext {
pub mut:
	tasks []Task
}

@[noinit]
pub struct Task {
	run fn () ! @[required]
pub:
	name    string
	help    string
	depends []string
}

@[params]
pub struct TaskConfig {
pub:
	name    string @[required]
	help    string
	depends []string
	run     fn () ! @[required]
}

// context creates an empty BuildContext.
pub fn context() BuildContext {
	return BuildContext{}
}

// task creates a task for the given context.
pub fn (mut context BuildContext) task(config TaskConfig) {
	if context.get_task(config.name) != none {
		eprintln('error: task already exists with name `${config.name}`')
		exit(1)
	}
	context.tasks << Task{
		run:     config.run
		name:    config.name
		help:    config.help
		depends: config.depends
	}
}

pub fn (context &BuildContext) get_task(name string) ?Task {
	for task in context.tasks {
		if task.name == name {
			return task
		}
	}
	return none
}

// exec executes the task with the given name in the context.
pub fn (context &BuildContext) exec(name string) {
	if task := context.get_task(name) {
		task.exec(context)
	} else {
		eprintln('error: no such task: ${name}')
		exit(1)
	}
}

// exec runs the given task and its dependencies
pub fn (task &Task) exec(context &BuildContext) {
	for dep in task.depends {
		if dep == task.name {
			eprintln('error: cyclic task dependency detected, `${task.name}` depends on itself')
			exit(1)
		}

		context.exec(dep)
	}
	println(': ${task.name}')
	task.run() or {
		eprintln('error: failed to run task `${task.name}`: ${err}')
		exit(1)
	}
}

// run executes all tasks provided through os.args.
pub fn (context &BuildContext) run() {
	if os.args.len == 1 {
		eprintln('error: no task provided, run with `--tasks` for a list of each')
		exit(1)
	}

	if '--tasks' in os.args || '-tasks' in os.args {
		println('Tasks:')
		for _, task in context.tasks {
			println('- ${task.name}: ${task.help}')
		}
		return
	}

	for arg in os.args[1..] {
		// Skip flags
		if arg.starts_with('-') {
			continue
		}
		context.exec(arg)
	}
}
