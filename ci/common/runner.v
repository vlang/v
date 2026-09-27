module common

import os
import crypto.sha256
import log
import term
import time

// exec is a helper function, to execute commands and exit early, if they fail.
pub fn exec(command string) {
	cmd := resolve_v_command(command)
	progress_dir := ci_task_progress_dir()
	previous_resume_dir := os.getenv_opt('VTEST_RESUME_DIR')
	if progress_dir != '' {
		// Keep the same file's results separate across tasks and command variants.
		os.setenv('VTEST_RESUME_DIR', os.join_path(progress_dir, 'tests', sha256.hexhash(cmd)), true)
	}
	defer {
		if progress_dir != '' {
			if previous := previous_resume_dir {
				os.setenv('VTEST_RESUME_DIR', previous, true)
			} else {
				os.unsetenv('VTEST_RESUME_DIR')
			}
		}
	}
	log.info('cmd: ${cmd}')
	result := os.system(cmd)
	if result != 0 {
		exit(result)
	}
}

fn ci_task_progress_dir() string {
	return os.getenv_opt('V_CI_TASK_PROGRESS') or { os.getenv('V_MACOS_CI_TASK_PROGRESS') }
}

// resolve_v_command ensures that commands starting with `v ` use the V from @VEXEROOT,
// not a potentially different V found via PATH.
fn resolve_v_command(command string) string {
	if command.starts_with('v ') {
		vexe := os.getenv_opt('V_CI_VEXE') or { os.join_path_single(@VEXEROOT, 'v') }
		return os.quoted_path(vexe) + command[1..]
	}
	return command
}

// unset is a helper function to unset a specific env variable.
pub fn unset(evar string) {
	log.info('unsetting env variable: ${evar}')
	os.unsetenv(evar)
}

// file_size_greater_than asserts that the given file exists, and is at least min_fsize bytes long.
pub fn file_size_greater_than(fpath string, min_fsize u64) {
	log.info('path should exist `${fpath}` ...')
	if !os.exists(fpath) {
		exit(1)
	}
	log.info('path exists, and should be a file: `${fpath}` ...')
	if !os.is_file(fpath) {
		exit(2)
	}
	real_size := os.file_size(fpath)
	log.info('actual file size of `${fpath}` is ${real_size}, wanted: ${min_fsize}, diff: ${real_size - min_fsize}.')
	if real_size < min_fsize {
		exit(3)
	}
}

const self_command = os.quoted_path(os.getenv_opt('V_CI_VEXE') or {
	os.join_path_single(@VEXEROOT, 'v')
}) + ' ' +
	os.real_path(os.executable()).replace_once(os.real_path(@VEXEROOT), '').trim_left('/\\') +
	'.vsh'

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
		mut failed_tasks := []string{}
		for tname, t in all_tasks {
			cmd := '${self_command} ${tname}'
			log.info('Start ${term.colorize(term.yellow, t.label)}, cmd: `${cmd}`')
			start := time.now()
			result := os.system(cmd)
			dt := time.now() - start
			if result != 0 {
				log.error('FAILED ${term.colorize(term.red, t.label)} in ${dt.milliseconds()} ms, cmd: `${cmd}`')
				failed_tasks << tname
			} else {
				log.info('Finished ${term.colorize(term.yellow, t.label)} in ${dt.milliseconds()} ms, cmd: `${cmd}`')
			}
		}
		if failed_tasks.len > 0 {
			log.error('${failed_tasks.len} task(s) failed: ${failed_tasks}')
			exit(1)
		}
		exit(0)
	}
	t := all_tasks[task_name] or {
		eprintln('Unknown task with name: `${task_name}`')
		exit(1)
	}
	t.run(task_name)
}
