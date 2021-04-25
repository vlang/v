module main

import os
import time

const scan_timeout_s = 5 * 60

const max_v_cycles = 1000

const scan_frequency_hz = 4

const scan_period_ms = 1000 / scan_frequency_hz

const max_scan_cycles = scan_timeout_s * scan_frequency_hz

//
// Implements `v -watch file.v` , `v -watch run file.v` etc.
// With this command, V will collect all .v files that are needed for the
// compilation, then it will enter an infinite loop, monitoring them for
// changes.
//
// When a change is detected, it will stop the current process, if it is
// still running, then rerun/recompile/etc.
//
// In effect, this makes it easy to have an editor session and a separate
// terminal, running just `v -watch run file.v`, and you will see your
// changes right after you save your .v file in your editor.
//
//
//    Since -gc boehm is not available on all platforms yet,
// and this program leaks ~8MB/minute without it, the implementation here
// is done similarly to vfmt in 2 modes, in the same executable:
//
//   a) A parent/manager process that only manages a single worker
//   process. The parent process does mostly nothing except restarting
//   workers, thus it does not leak much.
//
//   b) A worker process, doing the actual monitoring/polling.
//    NB: *workers are started with the -vwatchworker option*
//
//    Worker processes will run for a limited number of iterations, then
// they will do exit(255), and then the parent will start a new worker.
// Exiting by any other code will cause the parent to also exit with the
// same error code. This limits the potential leak that a worker process
// can do, even without using the garbage collection mode.
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
	pid             int  // the pid of the current process; useful while debugging manager/worker interactions
	is_worker       bool // true in the workers, false in the manager process
	check_period_ms int = scan_period_ms
	vexe            string
	affected_paths  []string
	vfiles          []VFileStat
	opts            []string
	rerun_channel   chan RerunCommand
	child_process   &os.Process
	is_exiting      bool // set by SIGINT/Ctrl-C
	v_cycles        int  // how many times the worker has restarted the V compiler
	scan_cycles     int  // how many times the worker has scanned for source file changes
}

[if debug_vwatch]
fn (mut context Context) elog(msg string) {
	eprintln('> vredo $context.pid, $msg')
}

fn (context &Context) str() string {
	return 'Context{ pid: $context.pid, is_worker: $context.is_worker, check_period_ms: $context.check_period_ms, vexe: $context.vexe, opts: $context.opts, is_exiting: $context.is_exiting, vfiles: $context.vfiles'
}

fn (mut context Context) get_stats_for_affected_vfiles() []VFileStat {
	if context.affected_paths.len == 0 {
		mut apaths := map[string]bool{}
		// The next command will make V parse the program, and print all .v files,
		// needed for its compilation, without actually compiling it.
		copts := context.opts.join(' ')
		cmd := '"$context.vexe" -silent -print-v-files $copts'
		// context.elog('> cmd: $cmd')
		mut vfiles := os.execute(cmd)
		if vfiles.exit_code == 0 {
			paths_trimmed := vfiles.output.trim_space()
			mut paths := paths_trimmed.split('\n')
			for vf in paths {
				apaths[os.real_path(os.dir(vf))] = true
			}
		}
		context.affected_paths = apaths.keys()
		// context.elog('vfiles paths to be scanned: $context.affected_paths')
	}
	// scan all files in the found folders
	mut newstats := []VFileStat{}
	for path in context.affected_paths {
		mut files := os.ls(path) or { []string{} }
		for pf in files {
			pf_ext := os.file_ext(pf).to_lower()
			if pf_ext in ['', '.bak', '.exe', '.dll', '.so', '.def'] {
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
	// always add the v compiler itself, so that if it is recompiled with `v self`
	// the watcher will rerun the compilation too
	newstats << VFileStat{context.vexe, os.file_last_mod_unix(context.vexe)}
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
					context.elog('> new updates for file: $vfs')
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
	if changed > 0 {
		context.elog('> get_changed_vfiles: $changed')
	}
	return changed
}

fn change_detection_loop(ocontext &Context) {
	mut context := ocontext
	for {
		if context.v_cycles >= max_v_cycles || context.scan_cycles >= max_scan_cycles {
			context.is_exiting = true
			context.kill_pgroup()
			time.sleep(50 * time.millisecond)
			exit(255)
		}
		if context.is_exiting {
			return
		}
		changes := context.get_changed_vfiles()
		if changes > 0 {
			context.rerun_channel <- RerunCommand.restart
		}
		time.sleep(context.check_period_ms * time.millisecond)
		context.scan_cycles++
	}
}

fn (mut context Context) kill_pgroup() {
	if context.child_process == 0 {
		return
	}
	if context.child_process.is_alive() {
		context.child_process.signal_pgkill()
	}
	context.child_process.wait()
}

fn (mut context Context) compilation_runner_loop() {
	cmd := '"$context.vexe" ${context.opts.join(' ')}'
	_ := <-context.rerun_channel
	for {
		context.elog('>> loop: v_cycles: $context.v_cycles')
		timestamp := time.now().format_ss_milli()
		context.child_process = os.new_process(context.vexe)
		context.child_process.use_pgroup = true
		context.child_process.set_args(context.opts)
		context.child_process.run()
		eprintln('$timestamp: $cmd | pid: ${context.child_process.pid:7d} | reload cycle: ${context.v_cycles:5d}')
		for {
			mut cmds := []RerunCommand{}
			for {
				if context.is_exiting {
					return
				}
				if !context.child_process.is_alive() {
					context.child_process.wait()
				}
				select {
					action := <-context.rerun_channel {
						cmds << action
						if action == .quit {
							context.kill_pgroup()
							return
						}
					}
					> 100 * time.millisecond {
						should_restart := RerunCommand.restart in cmds
						cmds = []
						if should_restart {
							// context.elog('>>>>>>>> KILLING $context.child_process.pid')
							context.kill_pgroup()
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
		context.v_cycles++
	}
}

const ccontext = Context{
	child_process: 0
}

fn main() {
	mut context := unsafe { &Context(voidptr(&ccontext)) }
	context.pid = os.getpid()
	context.vexe = os.getenv('VEXE')
	context.is_worker = os.args.contains('-vwatchworker')
	context.opts = os.args[1..].filter(it != '-vwatchworker')
	context.elog('>>> context.pid: $context.pid')
	context.elog('>>> context.vexe: $context.vexe')
	context.elog('>>> context.opts: $context.opts')
	context.elog('>>> context.is_worker: $context.is_worker')
	if context.is_worker {
		context.worker_main()
	} else {
		context.manager_main()
	}
}

fn (mut context Context) manager_main() {
	myexecutable := os.executable()
	mut worker_opts := ['-vwatchworker']
	worker_opts << context.opts
	for {
		mut worker_process := os.new_process(myexecutable)
		worker_process.set_args(worker_opts)
		worker_process.run()
		for {
			if !worker_process.is_alive() {
				worker_process.wait()
				break
			}
			time.sleep(200 * time.millisecond)
		}
		if !(worker_process.code == 255 && worker_process.status == .exited) {
			break
		}
	}
}

fn (mut context Context) worker_main() {
	context.rerun_channel = chan RerunCommand{cap: 10}
	os.signal(C.SIGINT, fn () {
		mut context := unsafe { &Context(voidptr(&ccontext)) }
		context.is_exiting = true
		context.kill_pgroup()
	})
	go context.compilation_runner_loop()
	change_detection_loop(context)
}
