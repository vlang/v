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

[unsafe]
fn (mut vfs VFileStat) free() {
	unsafe { vfs.path.free() }
}

struct Context {
mut:
	check_period_ms int = 300
	is_debug        bool
	vexe            string
	affected_paths  []string
	vfiles          map[string]VFileStat
	opts            []string
	rerun_channel   chan string
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

fn (mut context Context) update_changed_vfiles(changes map[string]VFileStat) {
	for f, x in changes {
		context.vfiles[f] = x
	}
}

fn (mut context Context) get_stats_for_affected_vfiles() map[string]VFileStat {
	if context.affected_paths.len == 0 {
		mut apaths := map[string]bool{}
		// The next command will make V parse the program, and print all .v files,
		// needed for its compilation, without actually compiling it.
		copts := context.opts.join(' ')
		cmd := '"$context.vexe" -silent -print-v-files ${copts}'
		// context.elog_debug('> cmd: $cmd')
		vfiles := os.execute(cmd)
		if vfiles.exit_code == 0 {
			paths_trimmed := vfiles.output.trim_space()
			paths := paths_trimmed.split('\n')
			for vf in paths {
				apaths[os.real_path(os.dir(vf))] = true
			}
		}
		context.affected_paths = apaths.keys()
		// context.elog_debug('vfiles paths to be scanned: $context.affected_paths')
	}
	// scan all files in the found folders
	mut newstats := map[string]VFileStat{}
	for path in context.affected_paths {
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
		if context.vfiles[f].mtime != newstat.mtime {
			changed[f] = newstat
			continue
		}
	}
	// context.elog_debug('> get_changed_vfiles: $changed')
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
			context.update_changed_vfiles(changes)
            eprintln('<- pushing "restart" changes.len: $changes.len')
			context.rerun_channel <- 'restart'
            eprintln('<- pushed "restart" changes.len: $changes.len')
		}
		time.sleep(context.check_period_ms * time.millisecond)
	}
}

fn (mut context Context) compilation_runner_loop() {
	cmd := '"$context.vexe" ${context.opts.join(' ')}'
	mut runner_cycles := 0
	mut cmds := []string{}
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
	go context.compilation_runner_loop()
	change_detection_loop(context)
}
