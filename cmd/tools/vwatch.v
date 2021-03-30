module main

import os
import time

//
// Implements `v -watch file.v` , `v -watch run file.v` etc.
// With this command, V will collect all .v files that are needed for the compilation,
// then it will enter an infinite loop, monitoring them for changes.
//
// When a change is detected, it will stop the current process, if it is still running,
// then rerun/recompile/etc.
//
// In effect, this makes it easy to have an editor session and a separate terminal,
// running just `v -watch run file.v`, and you will see your changes right after you save your
// .v file in your editor.
//

struct VFileStat {
	path  string
	mtime int
}

[unsafe]
fn (mut vfs VFileStat) free() {
	unsafe { vfs.path.free() }
}

enum RerunCommand {
	restart
	quit
}

struct Context {
mut:
	check_period_ms int = 250
	is_debug        bool
	vexe            string
	affected_paths  []string
	vfiles          []VFileStat
	opts            []string
	rerun_channel   chan RerunCommand
	child_process   &os.Process
	is_exiting      bool // set by SIGINT/Ctrl-C
}

fn (context &Context) str() string {
	return 'Context{ check_period_ms: $context.check_period_ms, is_debug: $context.is_debug, vexe: $context.vexe, opts: $context.opts, vfiles: $context.vfiles'
}

fn (mut context Context) elog_debug(msg string) {
	if context.is_debug {
		eprintln('> vredo: $msg')
	}
}

fn (mut context Context) get_stats_for_affected_vfiles() []VFileStat {
	if context.affected_paths.len == 0 {
		mut apaths := map[string]bool{}
		// The next command will make V parse the program, and print all .v files,
		// needed for its compilation, without actually compiling it.
		copts := context.opts.join(' ')
		cmd := '"$context.vexe" -silent -print-v-files $copts'
		// context.elog_debug('> cmd: $cmd')
		mut vfiles := os.execute(cmd)
		if vfiles.exit_code == 0 {
			paths_trimmed := vfiles.output.trim_space()
			mut paths := paths_trimmed.split('\n')
			for vf in paths {
				apaths[os.real_path(os.dir(vf))] = true
			}
		}
		context.affected_paths = apaths.keys()
		// context.elog_debug('vfiles paths to be scanned: $context.affected_paths')
	}
	// scan all files in the found folders
	mut newstats := []VFileStat{}
	for path in context.affected_paths {
		mut files := os.ls(path) or { []string{} }
		for pf in files {
			pf_ext := os.file_ext(pf).to_lower()
			if pf_ext in ['', 'bak', 'exe', 'dll', 'so'] {
				continue
			}
			if pf.starts_with('.#') {
				continue
			}
			if pf.ends_with('~') {
				continue
			}
			f := os.join_path(path, pf)
			fullpath := os.real_path(f)
			mtime := os.file_last_mod_unix(fullpath)
			newstats << VFileStat{fullpath, mtime}
		}
	}
	return newstats
}

fn (mut context Context) get_changed_vfiles() int {
	mut changed := 0
	newfiles := context.get_stats_for_affected_vfiles()
	for vfs in newfiles {
		mut found := false
		for existing_vfs in context.vfiles {
			if existing_vfs.path == vfs.path {
				found = true
				if existing_vfs.mtime != vfs.mtime {
					changed++
				}
				break
			}
		}
		if !found {
			changed++
			continue
		}
	}
	context.vfiles = newfiles
	context.elog_debug('> get_changed_vfiles: $changed')
	return changed
}

fn change_detection_loop(ocontext &Context) {
	mut context := ocontext
	for {
		if context.is_exiting {
			return
		}
		changes := context.get_changed_vfiles()
		if changes > 0 {
			context.rerun_channel <- RerunCommand.restart
		}
		time.sleep(context.check_period_ms * time.millisecond)
	}
}

fn (mut context Context) compilation_runner_loop() {
	cmd := '"$context.vexe" ${context.opts.join(' ')}'
	mut runner_cycles := 0
	_ := <-context.rerun_channel
	// context.elog_debug('>>>>>>>>>>>>>>>>>>>>>>>>>>>>> compilation_runner_loop FOR <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<')
	for {
		for {
			timestamp := time.now().format_ss_micro()
			context.child_process = os.new_process(context.vexe)
			context.child_process.use_pgroup = true
			context.child_process.set_args(context.opts)
			context.child_process.run()
			eprintln('$timestamp: $cmd | pid: $context.child_process.pid')
			// context.elog_debug('> compilation_runner_loop vexe pid: $context.child_process.pid | status: $context.child_process.status | cycles: $runner_cycles')
			for {
				mut cmds := []RerunCommand{}
				for {
					if context.is_exiting {
						return
					}
					if !context.child_process.is_alive() {
						// context.elog_debug('> process pid: $context.child_process.pid is no longer alive')
						context.child_process.wait()
					}
					select {
						action := <-context.rerun_channel {
							// context.elog_debug('received action: $action')
							cmds << action
							if action == .quit {
								context.child_process.signal_pgkill()
								context.child_process.wait()
								return
							}
						}
						> 100 * time.millisecond {
							should_restart := RerunCommand.restart in cmds
							cmds = []
							if should_restart {
								// context.elog_debug('>>>>>>>> KILLING $context.child_process.pid')
								context.child_process.signal_pgkill()
								context.child_process.wait()
								break
							}
						}
					}
				}
				if !context.child_process.is_alive() {
					context.child_process.wait()
					break
				}
			}
		}
		runner_cycles++
	}
}

const ccontext = Context{
	child_process: 0
}

fn main() {
	mut context := &ccontext
	context.rerun_channel = chan RerunCommand{cap: 10}
	context.vexe = os.getenv('VEXE')
	context.opts = os.args[1..]
	os.signal(C.SIGINT, fn () {
		mut context := &ccontext
		context.is_exiting = true
		if context.child_process == 0 {
			return
		}
		context.child_process.signal_pgkill()
		context.child_process.wait()
	})
	//
	context.is_debug = '-debug' in os.getenv('VWATCH').split(' ')
	go context.compilation_runner_loop()
	change_detection_loop(context)
}
