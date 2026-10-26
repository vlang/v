module main

import os
import time
import term
import v.help
import v.util.vwatchtty

const scan_timeout_s = get_scan_timeout_seconds()

const max_v_cycles = 1000

const scan_frequency_hz = 4

const scan_period_ms = 1000 / scan_frequency_hz

const max_scan_cycles = scan_timeout_s * scan_frequency_hz

const default_veb_suffixes = '*.v,*.html,*.css,*.js,*.md,*.tr'

// build_debounce_ms is how long a build waits after the last change, so that
// an editor writing a file several times starts one build, not several.
const build_debounce_ms = 150

// veb_status_env names the environment variable that tells a veb app, built
// with `-d veb_livereload`, where `v watch` writes the build status.
const veb_status_env = 'VEB_LIVERELOAD_STATUS'

fn get_scan_timeout_seconds() int {
	env_vw_timeout := os.getenv('VWATCH_TIMEOUT').int()
	if env_vw_timeout == 0 {
		$if gcboehm ? {
			return 35000000 // over 1 year
		} $else {
			return 5 * 60
		}
	}
	return env_vw_timeout
}

//
// Implements `v watch file.v` , `v watch run file.v` etc.
// With this command, V will collect all .v files that are needed for the
// compilation, then it will enter an infinite loop, monitoring them for
// changes.
//
// When a change is detected, it will stop the current process, if it is
// still running, then rerun/recompile/etc.
//
// In effect, this makes it easy to have an editor session and a separate
// terminal, running just `v watch run file.v`, and you will see your
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
//    Note: *workers are started with the --vwatchworker option*
//
//    Worker processes will run for a limited number of iterations, then
// they will do exit(255), and then the parent will start a new worker.
// Exiting by any other code will cause the parent to also exit with the
// same error code. This limits the potential leak that a worker process
// can do, even without using the garbage collection mode.
//

struct VFileStat {
	path  string
	mtime i64
}

@[unsafe]
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
	child_process   &os.Process = unsafe { nil }
	child_has_tty   bool
	watcher_pgid    int
	resume_worker   bool
	is_exiting      bool     // set by SIGINT/Ctrl-C
	v_cycles        int      // how many times the worker has restarted the V compiler
	scan_cycles     int      // how many times the worker has scanned for source file changes
	clear_terminal  bool     // whether to clear the terminal before each re-run
	keep_running    bool     // when true, re-run the program automatically if it exits on its own. Useful for gg apps.
	silent          bool     // when true, watch will not print a timestamp line before each re-run
	add_files       []string // path to additional files that have to be watched for changes
	ignore_exts     []string // extensions of files that will be ignored, even if they change (useful for sqlite.db files for example)
	cmd_before_run  string   // a command to run before each re-run
	cmd_after_run   string   // a command to run after each re-run
	only_watch      []string // If not empty, *all* files that trigger updates, should match *at least one* of these s.match_glob() patterns. This is also triggered for veb apps, to monitor for just *.v,*.js,*.css,*.html in veb projects.
	has_run         bool     // whether the watched command is `run`
	is_veb          bool     // whether the program uses veb
	build_first     bool     // for veb apps: build while the old server runs, and swap on success
	build_process   &os.Process = unsafe { nil } // the build in progress, in build-first mode
	status_path     string // where build-first mode writes the build status, for veb's livereload
}

struct ParsedWatchOptions {
mut:
	remaining_options []string
	is_worker         bool
	silent            bool
	clear_terminal    bool
	keep_running      bool
	add_files         []string = ['']
	ignore_exts       []string = ['']
	only_watch        []string = ['']
	cmd_before_run    string
	cmd_after_run     string
	show_help         bool
	no_build_first    bool
}

@[if debug_vwatch ?]
fn (mut context Context) elog(msg string) {
	eprintln('> vwatch ${context.pid}, ${msg}')
}

fn (context &Context) str() string {
	return 'Context{ pid: ${context.pid}, is_worker: ${context.is_worker}, check_period_ms: ${context.check_period_ms}, vexe: ${context.vexe}, opts: ${context.opts}, is_exiting: ${context.is_exiting}, vfiles: ${context.vfiles}'
}

fn (mut context Context) is_ext_ignored(pf string, pf_ext string) bool {
	if pf.starts_with('.') && pf.contains('.v3cc.') {
		// V3 creates its temporary C build directory next to the output binary.
		// Watching it would interrupt the active compiler and start an endless
		// create/change/restart loop before the user's program can run.
		return true
	}
	for ipattern in context.ignore_exts {
		if pf_ext.match_glob(ipattern) {
			return true
		}
	}
	if pf_ext in ['', '.so', '.a'] {
		// on unix, the executables saved by compilers, usually do not have extensions at all, and shared libs are .so
		return true
	}
	if pf_ext in ['.exe', '.dll', '.def'] {
		// on windows, files with these extensions will be generated by the compiler
		return true
	}
	// ignore common backup files saved by editors like emacs/jed/vim:
	if pf_ext == '.bak' {
		return true
	}
	// ignore DB files (sqlite databases, that are likely to change during prototyping):
	if pf_ext in ['.db', '.sqlite'] {
		return true
	}
	if pf.starts_with('.#') {
		return true
	}
	if pf.ends_with('~') {
		return true
	}
	return false
}

// add_dir_tree adds `dir` and all of its subdirectories (not following
// symlinks) to the set of watched folders.
fn add_dir_tree(mut apaths map[string]bool, dir string) {
	apaths[os.real_path(dir)] = true
	for entry in os.ls(dir) or { []string{} } {
		sub := os.join_path_single(dir, entry)
		if os.is_dir(sub) && !os.is_link(sub) {
			add_dir_tree(mut apaths, sub)
		}
	}
}

fn (mut context Context) get_stats_for_affected_vfiles() []VFileStat {
	if context.affected_paths.len == 0 {
		mut apaths := map[string]bool{}
		// The next command will make V parse the program, and print all .v files,
		// needed for its compilation, without actually compiling it.
		mut paths := []string{}
		if context.add_files.len > 0 && context.add_files[0] != '' {
			paths << context.add_files
		}
		vfiles := os.exec([context.vexe, '-silent', '-print-watched-files', ...context.opts])
		if vfiles.exit_code == 0 {
			paths_trimmed := vfiles.output.trim_space()
			reported_used_files := paths_trimmed.split_any('\n')
			$if trace_reported_used_files ? {
				context.elog('reported_used_files: ${reported_used_files}')
			}
			paths << reported_used_files
		}
		mut is_veb_found := false
		for vf in paths {
			apaths[os.real_path(os.dir(vf))] = true
			if vf.contains('veb.v') {
				is_veb_found = true
				context.is_veb = true
			}
		}

		if is_veb_found {
			if !os.args.any(it.starts_with('--only-watch')) {
				context.only_watch = default_veb_suffixes.split_any(',')
				// veb is often used with SQLite .db or .sqlite3 files right next to the executable/source,
				// that are updated by the veb app, causing a restart of the app, which in turn causes the
				// browser to reload the current page, that probably triggered the update in the first place.
				// Note that the problem is not specific to SQLite, any database that stores its files in the
				// current (project) folder, will also cause this.
				println('`v watch` detected that you are compiling a veb project.')
				println('   Because of that, the `--only-watch=${default_veb_suffixes}` flag was also implied.')
				println('   In result, `v watch` will ignore changes to other files.')
				println('   Add your own --only-watch filter, if you wish to override that choice.')
				println('')
			}
			// .tr translation files are loaded at runtime by veb (see vlib/veb/tr.v),
			// so the compiler never reports them via -print-watched-files. Explicitly
			// add `translations/` subdirectories of each project path, so edits to
			// .tr files trigger a reload like .html does.
			for path in apaths.keys() {
				tr_dir := os.join_path_single(path, 'translations')
				if os.is_dir(tr_dir) {
					apaths[os.real_path(tr_dir)] = true
				}
				// Templates are compiled into the program, but the compiler does not
				// report them either. Watch `templates/` and its subdirectories, so
				// that a layout in `templates/layout/` triggers a rebuild too.
				tpl_dir := os.join_path_single(path, 'templates')
				if os.is_dir(tpl_dir) {
					add_dir_tree(mut apaths, tpl_dir)
				}
			}
		}
		context.affected_paths = apaths.keys()
		// context.elog('vfiles paths to be scanned: ${context.affected_paths}')
	}
	// scan all files in the found folders:
	mut newstats := []VFileStat{}
	for path in context.affected_paths {
		mut files := os.ls(path) or { []string{} }
		next_file: for pf in files {
			pf_path := os.join_path_single(path, pf)
			if context.only_watch.len > 0 && context.only_watch[0] != '' {
				// in the whitelist mode, first only allow files, which match at least one of the patterns in context.only_watch:
				mut matched_pattern_idx := -1
				for ow_pattern_idx, ow_pattern in context.only_watch {
					if pf_path.match_glob(ow_pattern) {
						matched_pattern_idx = ow_pattern_idx
						context.elog('> ${@METHOD} matched --only-watch pattern: ${ow_pattern}, for file: ${pf_path}')
						break
					}
				}
				if matched_pattern_idx == -1 {
					context.elog('> ${@METHOD} --only-watch ignored file: ${pf_path}')
					continue
				}
			}
			// by default allow everything, except very specific extensions (backup files, executables etc):
			pf_ext := os.file_ext(pf).to_lower()
			if context.is_ext_ignored(pf, pf_ext) {
				context.elog('> ${@METHOD} ignored extension: ${pf_ext}, for file: ${pf_path}')
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
					context.elog('> new updates for file: ${vfs}')
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
		context.elog('> get_changed_vfiles: ${changed}')
	}
	return changed
}

fn change_detection_loop(ocontext &Context) {
	mut context := unsafe { ocontext }
	for {
		if context.v_cycles >= max_v_cycles || context.scan_cycles >= max_scan_cycles {
			context.is_exiting = true
			context.kill_build()
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
	if unsafe { context.child_process == 0 } {
		return
	}
	context.restore_terminal()
	if context.child_process.is_alive() {
		context.child_process.signal_pgkill()
	}
	context.child_process.wait()
	context.child_process.close()
}

// kill_build stops the build in progress, if any. It returns true when there was one.
fn (mut context Context) kill_build() bool {
	if unsafe { context.build_process == 0 } {
		return false
	}
	mut bp := context.build_process
	context.build_process = unsafe { nil }
	if bp.is_alive() {
		bp.signal_pgkill()
	}
	bp.wait()
	bp.close()
	return true
}

// stop_app stops the running program, if any.
fn (mut context Context) stop_app() {
	context.kill_pgroup()
	context.child_process = unsafe { nil }
}

// write_status records the build status for a veb app's livereload: the state
// on the first line (`building`, `failed` or `ok`), then the compiler output.
// It replaces the file in one step, so the app never reads a partial status.
fn (mut context Context) write_status(state string, details string) {
	tmp := context.status_path + '.tmp'
	os.write_file(tmp, state + '\n' + details) or { return }
	os.mv(tmp, context.status_path) or {}
}

// start_app runs the built program, telling it where to read the build status.
fn (mut context Context) start_app(app_bin string, args []string) {
	mut p := os.new_process(app_bin)
	p.use_pgroup = true
	p.set_args(args)
	mut env := os.environ()
	env[veb_status_env] = context.status_path
	p.set_environment(env)
	p.run()
	context.child_process = p
	context.child_has_tty = vwatchtty.set_foreground_process_group(p.pid, context.watcher_pgid)
	if !context.silent {
		eprintln('${time.now().format_ss_milli()}: ${app_bin} | pid: ${p.pid:7d} | reload cycle: ${context.v_cycles:5d}')
	}
	context.v_cycles++
}

// default_output_path returns where `v` puts the executable for `target` by
// default: `dir/dir` for a folder, and `file` for `file.v`.
fn default_output_path(target string) string {
	abs := os.real_path(target)
	if os.is_dir(abs) {
		return os.join_path_single(abs, os.file_name(abs))
	}
	return abs.all_before_last('.v')
}

// drain_build_output appends what the build printed since the last call.
fn (mut context Context) drain_build_output(mut output []string) {
	if unsafe { context.build_process == 0 } {
		return
	}
	mut bp := context.build_process
	for {
		chunk := bp.stdout_read()
		if chunk == '' {
			break
		}
		output << chunk
	}
}

// build_first_loop is used for veb apps, unless `--no-build-first` is given.
// It builds a new executable while the old server keeps serving, then swaps
// them only if the build succeeds: a failed build leaves the old server
// running. A change during a build cancels it and starts a new one, since its
// result would already be out of date. The status of each build is written to
// `context.status_path`, where a livereload enabled veb app picks it up.
fn (mut context Context) build_first_loop() {
	run_pos := context.opts.index('run')
	mut build_opts := context.opts#[..run_pos].clone()
	after_run := context.opts#[run_pos + 1..].clone()
	target := if after_run.len > 0 { after_run[0] } else { '.' }
	program_args := after_run#[1..]
	mut app_bin := default_output_path(target)
	o_pos := build_opts.index('-o')
	if o_pos >= 0 && o_pos + 1 < build_opts.len {
		app_bin = os.abs_path(build_opts[o_pos + 1])
		build_opts.delete(o_pos + 1)
		build_opts.delete(o_pos)
	}
	$if windows {
		if !app_bin.ends_with('.exe') {
			app_bin += '.exe'
		}
	}
	new_bin := os.join_path_single(os.dir(app_bin), '.vwatch-new-' + os.file_name(app_bin))
	status_dir := os.join_path(os.vtmp_dir(), 'vwatch_${context.pid}')
	os.mkdir_all(status_dir) or {}
	context.status_path = os.join_path_single(status_dir, 'status')
	mut pending := true
	mut pending_since := time.now().add(-build_debounce_ms * time.millisecond)
	mut output := []string{}
	mut build_started := time.now()
	for {
		if context.is_exiting {
			context.kill_build()
			context.stop_app()
			return
		}
		if pending && time.since(pending_since) >= build_debounce_ms * time.millisecond {
			pending = false
			if context.kill_build() && !context.silent {
				eprintln('${time.now().format_ss_milli()}: build cancelled, a newer change arrived')
			}
			if context.clear_terminal {
				term.clear()
			}
			context.write_status('building', '')
			context.run_before_cmd()
			output = []string{}
			mut bp := os.new_process(context.vexe)
			bp.use_pgroup = true
			bp.set_args([...build_opts, '-o', new_bin, target])
			bp.set_redirect_stdio_merged()
			bp.run()
			context.build_process = bp
			build_started = time.now()
			if !context.silent {
				eprintln('${time.now().format_ss_milli()}: building ${target} | pid: ${bp.pid:7d}')
			}
		}
		if unsafe { context.build_process != 0 } {
			context.drain_build_output(mut output)
			mut bp := context.build_process
			if !bp.is_alive() {
				bp.wait()
				context.drain_build_output(mut output)
				code := bp.code
				bp.close()
				context.build_process = unsafe { nil }
				compiler_output := output.join('')
				if compiler_output.len > 0 {
					eprint(compiler_output)
				}
				if !context.silent {
					took := f64(time.since(build_started).milliseconds()) / 1000.0
					result := if code == 0 { 'succeeded' } else { 'failed' }
					eprintln('${time.now().format_ss_milli()}: build ${result} in ${took:.2f}s')
				}
				if code == 0 {
					context.stop_app()
					$if windows {
						os.rm(app_bin) or {}
					}
					os.mv(new_bin, app_bin) or {
						eprintln('v watch: cannot replace ${app_bin}: ${err}')
						context.write_status('failed', 'cannot replace ${app_bin}: ${err}')
						continue
					}
					context.write_status('ok', '')
					context.start_app(app_bin, program_args)
				} else {
					context.write_status('failed', compiler_output)
					if !context.silent {
						eprintln('${time.now().format_ss_milli()}: build failed; the previous version keeps running')
					}
				}
			}
		}
		if unsafe { context.child_process != 0 } && !context.child_process.is_alive() {
			context.restore_terminal()
			context.child_process.wait()
			context.child_process.close()
			context.child_process = unsafe { nil }
			context.run_after_cmd()
			if context.keep_running && os.exists(app_bin) {
				context.start_app(app_bin, program_args)
			}
		}
		select {
			action := <-context.rerun_channel {
				if action == .quit {
					context.kill_build()
					context.stop_app()
					return
				}
				pending = true
				pending_since = time.now()
			}
			100 * time.millisecond {
			}
		}
	}
}

fn (mut context Context) restore_terminal() {
	if context.child_has_tty {
		vwatchtty.restore_foreground_process_group(context.watcher_pgid)
		context.child_has_tty = false
	}
}

fn (mut context Context) run_before_cmd() {
	if context.cmd_before_run != '' {
		context.elog('> run_before_cmd: "${context.cmd_before_run}"')
		os.system_args(if os.user_os() == 'windows' {
			['cmd.exe', '/d', '/s', '/c', context.cmd_before_run]
		} else {
			['sh', '-c', context.cmd_before_run]
		})
	}
}

fn (mut context Context) run_after_cmd() {
	if context.cmd_after_run != '' {
		context.elog('> run_after_cmd: "${context.cmd_after_run}"')
		os.system_args(if os.user_os() == 'windows' {
			['cmd.exe', '/d', '/s', '/c', context.cmd_after_run]
		} else {
			['sh', '-c', context.cmd_after_run]
		})
	}
}

fn (mut context Context) compilation_runner_loop() {
	cmd := '"${context.vexe}" ${context.opts.join(' ')}'
	_ := <-context.rerun_channel
	if context.build_first && context.has_run && context.is_veb {
		context.build_first_loop()
		return
	}
	for {
		context.elog('>> loop: v_cycles: ${context.v_cycles}')
		if context.clear_terminal {
			term.clear()
		}
		context.run_before_cmd()
		timestamp := time.now().format_ss_milli()
		context.child_process = os.new_process(context.vexe)
		context.child_process.use_pgroup = true
		context.child_process.set_args(context.opts)
		context.child_process.run()
		context.child_has_tty = vwatchtty.set_foreground_process_group(context.child_process.pid, context.watcher_pgid)
		if !context.silent {
			eprintln('${timestamp}: ${cmd} | pid: ${context.child_process.pid:7d} | reload cycle: ${context.v_cycles:5d}')
		}
		for {
			mut notalive_count := 0
			mut cmds := []RerunCommand{}
			for {
				if context.is_exiting {
					return
				}
				if !context.child_process.is_alive() {
					context.restore_terminal()
					was_interrupted := context.child_process.status == .aborted
						&& context.child_process.code == 130
					context.child_process.wait()
					if was_interrupted {
						context.child_process.close()
						context.is_exiting = true
						return
					}
					notalive_count++
					if notalive_count == 1 {
						// a short lived process finished, do cleanup:
						context.run_after_cmd()
						if context.keep_running {
							break
						}
					}
				}
				select {
					action := <-context.rerun_channel {
						cmds << action
						if action == .quit {
							context.kill_pgroup()
							return
						}
					}
					100 * time.millisecond {
						should_restart := RerunCommand.restart in cmds
						cmds = []
						if should_restart {
							// context.elog('>>>>>>>> KILLING ${context.child_process.pid}')
							context.kill_pgroup()
							break
						}
					}
				}
			}
			if !context.child_process.is_alive() {
				context.restore_terminal()
				context.elog('> child_process is no longer alive | notalive_count: ${notalive_count}')
				context.child_process.wait()
				context.child_process.close()
				if notalive_count == 0 {
					// a long running process was killed, do cleanup:
					context.run_after_cmd()
				}
				break
			}
		}
		context.v_cycles++
	}
}

const ccontext = Context{}

fn main() {
	mut context := unsafe { &Context(voidptr(&ccontext)) }
	context.pid = os.getpid()
	context.vexe = os.getenv('VEXE')

	watch_pos := os.args.index('watch')
	all_args_before_watch_cmd := os.args#[1..watch_pos]
	all_args_after_watch_cmd := os.args#[watch_pos + 1..]

	// dump(os.getpid())
	// dump(all_args_before_watch_cmd)
	// dump(all_args_after_watch_cmd)

	// Options after `run` should be ignored, since they are intended for the user program, not for the watcher.
	// For example, `v watch run x.v -a -b -k', should pass all of -a -b -k to the compiled and run program.
	only_watch_options, has_run := all_before('run', all_args_after_watch_cmd)
	parsed_watch_options := parse_watch_options(only_watch_options) or {
		eprintln('Error: ${err}')
		exit(1)
	}
	if parsed_watch_options.show_help {
		help.print_and_exit('watch', exit_code: 0)
	}
	context.is_worker = parsed_watch_options.is_worker
	context.silent = parsed_watch_options.silent
	context.clear_terminal = parsed_watch_options.clear_terminal
	context.keep_running = parsed_watch_options.keep_running
	context.add_files = parsed_watch_options.add_files
	context.ignore_exts = parsed_watch_options.ignore_exts
	context.only_watch = parsed_watch_options.only_watch
	context.cmd_before_run = parsed_watch_options.cmd_before_run
	context.cmd_after_run = parsed_watch_options.cmd_after_run
	context.has_run = has_run
	context.build_first = !parsed_watch_options.no_build_first
	context.opts = []
	context.opts << all_args_before_watch_cmd
	context.opts << parsed_watch_options.remaining_options
	if has_run {
		context.opts << 'run'
		context.opts << all_after('run', all_args_after_watch_cmd)
	}
	context.elog('>>> context.pid: ${context.pid}')
	context.elog('>>> context.vexe: ${context.vexe}')
	context.elog('>>> context.opts: ${context.opts}')
	context.elog('>>> context.is_worker: ${context.is_worker}')
	context.elog('>>> context.clear_terminal: ${context.clear_terminal}')
	context.elog('>>> context.add_files: ${context.add_files}')
	context.elog('>>> context.ignore_exts: ${context.ignore_exts}')
	context.elog('>>> context.only_watch: ${context.only_watch}')
	if context.is_worker {
		context.worker_main()
	} else {
		context.manager_main(all_args_before_watch_cmd, all_args_after_watch_cmd)
	}
}

// parse_watch_options parses only vwatch's own flags and leaves compiler flags untouched.
// This avoids `flag.FlagParser` short-cluster handling from rewriting passthrough args like
// `-backend` into `-baend`.
fn parse_watch_options(args []string) !ParsedWatchOptions {
	mut parsed := ParsedWatchOptions{}
	mut i := 0
	for i < args.len {
		arg := args[i]
		if arg == '--vwatchworker' {
			parsed.is_worker = true
			i++
			continue
		}
		if is_watch_option_name(arg, 'silent', `s`) {
			parsed.silent = true
			i++
			continue
		}
		if is_watch_option_name(arg, 'clear', `c`) {
			parsed.clear_terminal = true
			i++
			continue
		}
		if is_watch_option_name(arg, 'keep', `k`) {
			parsed.keep_running = true
			i++
			continue
		}
		if is_watch_option_name(arg, 'no-build-first', 0) {
			parsed.no_build_first = true
			i++
			continue
		}
		if is_watch_option_name(arg, 'help', `h`) {
			parsed.show_help = true
			i++
			continue
		}
		if value := watch_option_inline_value(arg, 'add', `a`) {
			parsed.add_files = value.split_any(',')
			i++
			continue
		}
		if is_watch_option_name(arg, 'add', `a`) {
			i++
			if i >= args.len {
				return error('missing value for `${arg}`')
			}
			parsed.add_files = args[i].split_any(',')
			i++
			continue
		}
		if value := watch_option_inline_value(arg, 'ignore', `i`) {
			parsed.ignore_exts = value.split_any(',')
			i++
			continue
		}
		if is_watch_option_name(arg, 'ignore', `i`) {
			i++
			if i >= args.len {
				return error('missing value for `${arg}`')
			}
			parsed.ignore_exts = args[i].split_any(',')
			i++
			continue
		}
		if value := watch_option_inline_value(arg, 'only-watch', `o`) {
			parsed.only_watch = value.split_any(',')
			i++
			continue
		}
		if is_watch_option_name(arg, 'only-watch', `o`) {
			i++
			if i >= args.len {
				return error('missing value for `${arg}`')
			}
			parsed.only_watch = args[i].split_any(',')
			i++
			continue
		}
		if value := watch_option_inline_value(arg, 'before', 0) {
			parsed.cmd_before_run = value
			i++
			continue
		}
		if is_watch_option_name(arg, 'before', 0) {
			i++
			if i >= args.len {
				return error('missing value for `${arg}`')
			}
			parsed.cmd_before_run = args[i]
			i++
			continue
		}
		if value := watch_option_inline_value(arg, 'after', 0) {
			parsed.cmd_after_run = value
			i++
			continue
		}
		if is_watch_option_name(arg, 'after', 0) {
			i++
			if i >= args.len {
				return error('missing value for `${arg}`')
			}
			parsed.cmd_after_run = args[i]
			i++
			continue
		}
		parsed.remaining_options << arg
		i++
	}
	return parsed
}

fn is_watch_option_name(arg string, longhand string, shorthand u8) bool {
	return arg == watch_short_option_name(shorthand) || arg == '-${longhand}'
		|| arg == '--${longhand}'
}

fn watch_option_inline_value(arg string, longhand string, shorthand u8) ?string {
	prefixes := [watch_short_option_with_equals(shorthand), '-${longhand}=', '--${longhand}=']
	for prefix in prefixes {
		if prefix != '' && arg.starts_with(prefix) {
			return arg[prefix.len..]
		}
	}
	return none
}

fn watch_short_option_name(shorthand u8) string {
	if shorthand == 0 {
		return ''
	}
	return '-${shorthand.ascii_str()}'
}

fn watch_short_option_with_equals(shorthand u8) string {
	short_option := watch_short_option_name(shorthand)
	if short_option == '' {
		return ''
	}
	return '${short_option}='
}

fn (mut context Context) manager_main(all_args_before_watch_cmd []string, all_args_after_watch_cmd []string) {
	myexecutable := os.executable()
	mut worker_opts := all_args_before_watch_cmd.clone()
	worker_opts << ['watch', '--vwatchworker']
	worker_opts << all_args_after_watch_cmd
	$if !windows {
		os.signal_opt(vwatchtty.continue_signal(), fn (_ os.Signal) {
			mut context := unsafe { &Context(voidptr(&ccontext)) }
			context.resume_worker = true
		}) or { panic(err) }
		// Pass termination on to the worker, which stops the program it runs.
		os.signal_opt(.term, manager_stop_signal_handler) or { panic(err) }
		os.signal_opt(.hup, manager_stop_signal_handler) or { panic(err) }
	}
	for {
		mut worker_process := os.new_process(myexecutable)
		worker_process.set_args(worker_opts)
		worker_process.run()
		for {
			if context.is_exiting {
				if worker_process.is_alive() {
					worker_process.signal_term()
				}
				worker_process.wait()
				worker_process.close()
				return
			}
			if context.resume_worker {
				context.resume_worker = false
				vwatchtty.continue_process_group_of(worker_process.pid)
			}
			if !worker_process.is_alive() {
				worker_process.wait()
				break
			}
			time.sleep(200 * time.millisecond)
		}
		if !(worker_process.code == 255 && worker_process.status == .exited) {
			worker_process.close()
			break
		}
		worker_process.close()
	}
}

fn worker_stop_signal_handler(_ os.Signal) {
	mut context := unsafe { &Context(voidptr(&ccontext)) }
	context.is_exiting = true
	context.kill_build()
	context.kill_pgroup()
}

fn manager_stop_signal_handler(_ os.Signal) {
	mut context := unsafe { &Context(voidptr(&ccontext)) }
	context.is_exiting = true
}

fn propagate_terminal_stop(_ os.Signal) {
	context := unsafe { &Context(voidptr(&ccontext)) }
	vwatchtty.suspend_manager_process_group(context.watcher_pgid)
}

fn (mut context Context) worker_main() {
	context.watcher_pgid = vwatchtty.process_group()
	context.rerun_channel = chan RerunCommand{cap: 10}
	os.signal_opt(.int, worker_stop_signal_handler) or { panic(err) }
	$if !windows {
		// `kill`, a closed terminal, or a process supervisor: stop the build and
		// the program too. They run in their own process groups, so they would
		// otherwise keep running (and keep their port) after `v watch` is gone.
		os.signal_opt(.term, worker_stop_signal_handler) or { panic(err) }
		os.signal_opt(.hup, worker_stop_signal_handler) or { panic(err) }
	}
	$if !windows {
		for signal in vwatchtty.terminal_stop_signals() {
			os.signal_opt(signal, propagate_terminal_stop) or { panic(err) }
		}
	}
	spawn context.compilation_runner_loop()
	change_detection_loop(context)
}

fn all_before(needle string, all []string) ([]string, bool) {
	needle_pos := all.index(needle)
	if needle_pos == -1 {
		return all, false
	}
	return all#[..needle_pos], true
}

fn all_after(needle string, all []string) []string {
	needle_pos := all.index(needle)
	if needle_pos == -1 {
		return all
	}
	return all#[needle_pos + 1..]
}
