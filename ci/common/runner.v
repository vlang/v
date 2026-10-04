module common

import os
import crypto.sha256
import log
import term
import time

// exec is a helper function, to execute commands and exit early, if they fail.
@[deprecated: 'use exec_args with an argument array to avoid shell injection']
pub fn exec(command string) {
	exec_args(os.split_args(command) or { panic(err) })
}

// exec_args runs literal arguments with the V compiler from this checkout.
pub fn exec_args(arguments []string) {
	argv := ci_argv(arguments, os.getenv_opt('V_CI_VEXE') or {
		os.join_path_single(@VEXEROOT, 'v')
	})
	command := argv.map(os.quoted_path(it)).join(' ')
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
	result := os.system_args(argv)
	if result != 0 {
		exit(result)
	}
}

// ci_argv replaces the program `v` with `vexe`, also after leading `NAME=value`
// assignments, which are then run through `env`. An explicit leading `env`
// program is kept, so `['env', 'VJOBS=1', 'v', ...]` does not depend on PATH.
fn ci_argv(arguments []string, vexe string) []string {
	mut argv := arguments.clone()
	has_env_program := argv.len > 0 && argv[0] == 'env'
	mut program_index := if has_env_program { 1 } else { 0 }
	for program_index < argv.len && is_env_assignment(argv[program_index]) {
		program_index++
	}
	if program_index < argv.len && argv[program_index] == 'v' {
		argv[program_index] = vexe
	}
	if program_index > 0 && !has_env_program {
		argv.prepend('env')
	}
	return argv
}

fn ci_task_progress_dir() string {
	return os.getenv_opt('V_CI_TASK_PROGRESS') or { os.getenv('V_MACOS_CI_TASK_PROGRESS') }
}

// resolve_v_command ensures that commands starting with `v `, optionally after leading
// `NAME=value` environment assignments, use the V from @VEXEROOT, not a potentially
// different V found via PATH.
fn resolve_v_command(command string) string {
	mut prefix_len := 0
	for {
		rest := command[prefix_len..]
		if rest.starts_with('v ') {
			vexe := os.getenv_opt('V_CI_VEXE') or { os.join_path_single(@VEXEROOT, 'v') }
			return command[..prefix_len] + os.quoted_path(vexe) + rest[1..]
		}
		word_end := rest.index(' ') or { return command }
		if !is_env_assignment(rest[..word_end]) {
			return command
		}
		prefix_len += word_end + 1
	}
	return command
}

fn is_env_assignment(word string) bool {
	eq := word.index('=') or { return false }
	if eq == 0 {
		return false
	}
	for i, c in word[..eq] {
		if !(c == `_` || c.is_letter() || (i > 0 && c.is_digit())) {
			return false
		}
	}
	return true
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
			result := os.system_args([
				...(os.split_args(self_command) or { panic(err) }),
				tname,
			])
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
