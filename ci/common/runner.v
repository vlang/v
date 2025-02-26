module common

import os
import log
import term
import time

// exec is a helper function, to execute commands and exit early, if they fail.
pub fn exec(command string) {
	log.info('cmd: ${command}')
	result := os.system(command)
	if result != 0 {
		exit(result)
	}
}

const self_command = 'v ' +
	os.real_path(os.executable()).replace_once(os.real_path(@VROOT), '').trim_left('/\\') + '.vsh'

pub const is_github_job = os.getenv('GITHUB_JOB') != ''

pub type Fn = fn ()

pub struct Task {
pub mut:
	f     Fn = unsafe { nil }
	label string
}

pub fn (t Task) run(tname string) {
	cmd := '${self_command} ${tname}'
	log.info('Start ${term.colorize(term.yellow, t.label)}, cmd: `${cmd}`')
	start := time.now()
	t.f()
	dt := time.now() - start
	log.info('Finished ${term.colorize(term.yellow, t.label)} in ${dt.milliseconds()} ms, cmd: `${cmd}`')
	println('')
}

pub fn run(all_tasks map[string]Task) {
	unbuffer_stdout()
	log.use_stdout()
	if os.args.len < 2 {
		println('Usage: v run macos_ci.vsh <task_name>')
		println('Available tasks are: ${all_tasks.keys()}')
		exit(0)
	}
	task_name := os.args[1]
	if task_name == 'all' {
		log.info(term.colorize(term.green, 'Run everything...'))
		for tname, t in all_tasks {
			t.run(tname)
		}
		exit(0)
	}
	t := all_tasks[task_name] or {
		eprintln('Unknown task with name: `${task_name}`')
		exit(1)
	}
	t.run(task_name)
}
