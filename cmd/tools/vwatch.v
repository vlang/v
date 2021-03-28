module main

// Implements `v redo file.v` , `v redo run file.v` etc.
// With this command, V will collect all .v files that are needed for the compilation,
// then it will enter an infinite loop, monitoring them for changes.
//
// When a change is detected, it will stop the current process, if it is still running,
// then rerun/recompile/etc.
//
// In effect, this makes it easy to have an editor session and a separate terminal,
// running just `v redo file.v`, and you will see your changes right after you save your
// .v file in your editor.
// TODO: stop/kill the current long running process if it is still live, when a change is
// detected.
import os
import time
import v.pref

struct VFileStat {
	path  string
	mtime int
}

struct Context {
mut:
	check_period_ms int = 200
	is_debug        bool
	vexe            string
	affected_paths  map[string]bool
	vfiles          map[string]VFileStat
	opts            []string
	rerun_channel   chan string
	child_process   &os.Process
	is_exiting      bool // set by SIGINT/Ctrl-C
}

fn (context Context) str() string {
	return 'Context{ check_period_ms: $context.check_period_ms, is_debug: $context.is_debug, vexe: $context.vexe, opts: $context.opts, vfiles: $context.vfiles'
}

fn (mut context Context) elog_debug(msg string) {
	if context.is_debug {
		eprintln('> vredo: $msg')
	}
}

fn (mut context Context) update_changed_vfiles(changes map[string]VFileStat) {
	for f, x in changes {
		context.vfiles[f] = x
	}
}

fn (mut context Context) get_stats_for_affected_vfiles() map[string]VFileStat {
	mut newstats := map[string]VFileStat{}
	if context.affected_paths.len == 0 {
		cmd := '"$context.vexe" -silent -print_v_files ${context.opts.join(' ')}'
		context.elog_debug('> cmd: $cmd')
		vfiles := os.execute(cmd)
		if vfiles.exit_code == 0 {
			paths := vfiles.output.trim_space().split('\n')
			for vf in paths {
				context.affected_paths[os.dir(vf)] = true
			}
		}
	}
	for path, _ in context.affected_paths {
		files := os.ls(path) or { []string{} }
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
			newstats[fullpath] = VFileStat{fullpath, mtime}
		}
	}
	return newstats
}

fn (mut context Context) get_changed_vfiles() map[string]VFileStat {
	mut changed := map[string]VFileStat{}
	newstats := context.get_stats_for_affected_vfiles()
	for f, newstat in newstats {
		if f !in context.vfiles {
			changed[f] = newstat
			continue
		}
		oldstat := context.vfiles[f]
		if oldstat.mtime != newstat.mtime {
			changed[f] = newstat
			continue
		}
	}
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
		if changes.len > 0 {
			for f, _ in changes {
				context.elog_debug('Checking found $changes.len changed files... First changed file: $f .')
				break
			}
			context.update_changed_vfiles(changes)
			context.rerun_channel <- 'restart'
		}
		time.sleep(time.millisecond * context.check_period_ms)
	}
}

fn (mut context Context) compilation_runner_loop() {
	cmd := '"$context.vexe" ${context.opts.join(' ')}'
	mut runner_cycles := 0
	mut cmds := []string{}
	_ := <-context.rerun_channel
	context.elog_debug('>>>>>>>>>>>>>>>>>>>>>>>>>>>>> compilation_runner_loop FOR <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<')
	for {
		for {
			timestamp := time.now().format_ss_micro()
			context.child_process = os.new_process(context.vexe)
			context.child_process.use_pgroup = true
			context.child_process.set_args(context.opts)
			context.child_process.run()
			eprintln('$timestamp: $cmd | pid: $context.child_process.pid')
			context.elog_debug('> compilation_runner_loop vexe pid: $context.child_process.pid | status: $context.child_process.status | cycles: $runner_cycles')
			for {
				for {
					if context.is_exiting {
						return
					}
					if !context.child_process.is_alive() {
						context.elog_debug('> process pid: $context.child_process.pid is no longer alive')
						context.child_process.wait()
					}
					select {
						action := <-context.rerun_channel {
							context.elog_debug('received action: $action')
							cmds << action
							if action == 'quit' {
								context.child_process.signal_pgkill()
								context.child_process.wait()
								return
							}
						}
						> 100 * time.millisecond {
							should_restart := 'restart' in cmds
							cmds = []
							if should_restart {
								context.elog_debug('>>>>>>>> KILLING $context.child_process.pid')
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
	context.rerun_channel = chan string{cap: 10}
	context.vexe = pref.vexe_path()
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
	go change_detection_loop(context)
	context.compilation_runner_loop()
}
