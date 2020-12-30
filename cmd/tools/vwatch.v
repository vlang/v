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
import sync

struct VFileStat {
	path  string
	mtime int
}

struct Context {
mut:
	check_period_ms int = 200
	is_debug        bool
	vexe            string
	vfiles          map[string]VFileStat
	opts            []string
	rerun_channel   chan string
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
	cmd := '"$context.vexe" -silent -print_v_files ${context.opts.join(' ')}'
	context.elog_debug('> cmd: $cmd')
	vfiles := os.exec(cmd) or { panic(err) }
	if vfiles.exit_code == 0 {
		paths := vfiles.output.trim_space().split('\n')
		for f in paths {
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
	return changed
}

fn change_detection_loop(ocontext &Context) {
	mut context := ocontext
	for {
		changes := context.get_changed_vfiles()
		if changes.len > 0 {
			for f, _ in changes {
				context.elog_debug('Checking found $changes.len changed files... First changed file: $f .')
				break
			}
			context.update_changed_vfiles(changes)
			context.rerun_channel <- 'restart'
		}
		time.sleep_ms(context.check_period_ms)
	}
}

fn (mut context Context) compilation_runner_loop() {
	cmd := '"$context.vexe" ${context.opts.join(' ')}'
	mut runner_cycles := 0
	mut cmds := []string{}
	x := <- context.rerun_channel
	eprintln('>>>>>>>>>>>>>>>>>>>>>>>>>>>>> compilation_runner_loop FOR <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<')
	for {
		for {
			timestamp := time.now().format_ss_micro()
			println("$timestamp: $cmd")
			mut p := os.new_process(context.vexe)
			p.set_args(context.opts)
			p.run()
			eprintln('> compilation_runner_loop vexe pid: $p.pid | status: $p.status')
			eprintln('> compilation_runner_loop vexe pid: $p.pid | cycle $runner_cycles')
			for {
				for {
					select {
						action := <- context.rerun_channel {
							eprintln('received action: $action')
							cmds << action
							if action == 'quit' {
								p.signal_kill()
								p.wait()
								return
							}
						}
						> 2000 * time.millisecond {					
							should_restart := 'restart' in cmds
							eprintln('> compilation_runner_loop vexe pid: $p.pid | > 2000 ms passed with no other command, cmds: $cmds | should_restart: $should_restart')
							cmds = []
							if should_restart {
								eprintln('>>>>>>>> KILLING $p.pid')
								p.signal_kill()
								p.wait()
								break
							}
						}
					}
				}
				if !p.is_alive() {
					p.wait()
					break
				}				
			}
		}
		runner_cycles++
	}
}

fn main() {
	mut context := Context{}
	context.rerun_channel = chan string{cap: 10}
	context.vexe = pref.vexe_path()
	context.opts = os.args[1..]
	//
	context.is_debug = '-debug' in os.getenv('VWATCH').split(' ')
	go change_detection_loop(&context)
	context.compilation_runner_loop()
	//
	// TODO: Add input handling, to implement forced redo on a keypress.
	// For now, just sleep forever in the main thread.
	time.sleep_ms(1000_000_000)
}
