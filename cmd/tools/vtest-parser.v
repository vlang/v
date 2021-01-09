import os
import flag
import term
import time
import v.parser
import v.table
import v.ast
import v.pref

const (
	vexe          = pref.vexe_path()
	vroot         = os.dir(vexe)
	support_color = term.can_show_color_on_stderr() && term.can_show_color_on_stdout()
	ecode_timeout = 101
	ecode_memout  = 102
	ecode_exec    = 103
	ecode_details = {
		'101': 'too slow'
		'102': 'too memory hungry'
		'103': 'worker executable not found'
	}
)

struct Context {
mut:
	is_help    bool
	is_worker  bool
	is_verbose bool
	timeout_ms int
	myself     string // path to this executable, so the supervisor can launch worker processes
	all_paths  []string // all files given to the supervisor process
	path       string // the current path, given to a worker process
	cut_index  int // the cut position in the source from context.path
	max_index  int // the maximum index (equivalent to the file content length)
	// parser context in the worker processes:
	table      table.Table
	scope      ast.Scope
	pref       pref.Preferences
	period_ms  int // print periodic progress
	stop_print bool // stop printing the periodic progress
}

fn main() {
	mut context := process_cli_args()
	if context.is_worker {
		pid := os.getpid()
		context.log('> worker ${pid:5} starts parsing at cut_index: ${context.cut_index:5} | $context.path')
		// A worker's process job is to try to parse a single given file in context.path.
		// It can crash/panic freely.
		context.table = table.new_table()
		context.scope = &ast.Scope{
			parent: 0
		}
		context.pref = &pref.Preferences{
			output_mode: .silent
		}
		mut source := os.read_file(context.path) ?
		source = source[..context.cut_index]
		go fn (ms int) {
			time.sleep_ms(ms)
			exit(ecode_timeout)
		}(context.timeout_ms)
		_ := parser.parse_text(source, context.path, context.table, .skip_comments, context.pref,
			context.scope)
		context.log('> worker ${pid:5} finished parsing $context.path')
		exit(0)
	} else {
		// The process supervisor should NOT crash/panic, unlike the workers.
		// It's job, is to:
		// 1) start workers
		// 2) accumulate results
		// 3) produce a summary at the end
		context.expand_all_paths()
		mut fails := 0
		mut panics := 0
		sw := time.new_stopwatch({})
		for path in context.all_paths {
			filesw := time.new_stopwatch({})
			context.start_printing()
			new_fails, new_panics := context.process_whole_file_in_worker(path)
			fails += new_fails
			panics += new_panics
			context.stop_printing()
			context.info('File: ${path:-30} | new_fails: ${new_fails:5} | new_panics: ${new_panics:5} | Elapsed time: ${filesw.elapsed().milliseconds()}ms')
		}
		non_panics := fails - panics
		context.info('Total files processed: ${context.all_paths.len:5} | Errors found: ${fails:5} | Panics: ${panics:5} | Non panics: ${non_panics:5} | Elapsed time: ${sw.elapsed().milliseconds()}ms')
		if fails > 0 {
			exit(1)
		}
		exit(0)
	}
}

fn process_cli_args() &Context {
	mut context := &Context{}
	context.myself = os.executable()
	mut fp := flag.new_flag_parser(os.args_after('test-parser'))
	fp.application(os.file_name(context.myself))
	fp.version('0.0.1')
	fp.description('Test the V parser, by parsing each .v file in each PATH,\n' +
		'as if it was typed character by character by the user.\n' +
		'A PATH can be either a folder, or a specific .v file.\n' +
		'NB: you *have to quote* the PATH, if it contains spaces/punctuation.')
	fp.arguments_description('PATH1 PATH2 ...')
	fp.skip_executable()
	context.is_help = fp.bool('help', `h`, false, 'Show help/usage screen.')
	context.is_verbose = fp.bool('verbose', `v`, false, 'Be more verbose.')
	context.period_ms = fp.int('progress_ms', `s`, 500, 'print a status report periodically, the period is given in milliseconds.')
	context.is_worker = fp.bool('worker', `w`, false, 'worker specific flag - is this a worker process, that can crash/panic.')
	context.cut_index = fp.int('cut_index', `c`, 1, 'worker specific flag - cut index in the source file, everything before that will be parsed, the rest - ignored.')
	context.timeout_ms = fp.int('timeout_ms', `t`, 250, 'worker specific flag - timeout in ms; a worker taking longer, will self terminate.')
	context.path = fp.string('path', `p`, '', 'worker specific flag - path to the current source file, which will be parsed.')
	//
	if context.is_help {
		println(fp.usage())
		exit(0)
	}
	context.all_paths = fp.finalize() or {
		context.error(err)
		exit(1)
	}
	if !context.is_worker && context.all_paths.len == 0 {
		println(fp.usage())
		exit(0)
	}
	return context
}

// ////////////////
fn bold(msg string) string {
	if !support_color {
		return msg
	}
	return term.bold(msg)
}

fn red(msg string) string {
	if !support_color {
		return msg
	}
	return term.red(msg)
}

fn yellow(msg string) string {
	if !support_color {
		return msg
	}
	return term.yellow(msg)
}

fn (mut context Context) info(msg string) {
	println(msg)
}

fn (mut context Context) log(msg string) {
	if context.is_verbose {
		label := yellow('info')
		ts := time.now().format_ss_micro()
		eprintln('$label: $ts | $msg')
	}
}

fn (mut context Context) error(msg string) {
	label := red('error')
	eprintln('$label: $msg')
}

fn (mut context Context) expand_all_paths() {
	context.log('> context.all_paths before: $context.all_paths')
	mut files := []string{}
	for path in context.all_paths {
		if os.is_dir(path) {
			files << os.walk_ext(path, '.v')
			files << os.walk_ext(path, '.vsh')
			continue
		}
		if !path.ends_with('.v') && !path.ends_with('.vv') && !path.ends_with('.vsh') {
			context.error('`v test-parser` can only be used on .v/.vv/.vsh files.\nOffending file: "$path".')
			continue
		}
		if !os.exists(path) {
			context.error('"$path" does not exist.')
			continue
		}
		files << path
	}
	context.all_paths = files
	context.log('> context.all_paths after: $context.all_paths')
}

fn (mut context Context) process_whole_file_in_worker(path string) (int, int) {
	context.path = path // needed for the progress bar
	context.log('> context.process_whole_file_in_worker path: $path')
	if !(os.is_file(path) && os.is_readable(path)) {
		context.error('$path is not readable')
		return 1, 0
	}
	source := os.read_file(path) or { '' }
	if source == '' {
		// an empty file is a valid .v file
		return 0, 0
	}
	len := source.len - 1
	mut fails := 0
	mut panics := 0
	context.max_index = len
	for i in 0 .. len {
		verbosity := if context.is_verbose { '-v' } else { '' }
		context.cut_index = i // needed for the progress bar
		cmd := '"$context.myself" $verbosity --worker --timeout_ms ${context.timeout_ms:5} --cut_index ${i:5} --path "$path" '
		context.log(cmd)
		res := os.exec(cmd) or { os.Result{
			output: err
			exit_code: ecode_exec
		} }
		context.log('worker exit_code: $res.exit_code | worker output:\n$res.output')
		if res.exit_code != 0 {
			fails++
			mut is_panic := false
			if res.output.contains('V panic:') {
				is_panic = true
				panics++
			}
			part := source[..i]
			line := part.count('\n') + 1
			last_line := part.all_after_last('\n')
			col := last_line.len
			err := if is_panic { red('parser failure: panic') } else { red('parser failure: crash, ${ecode_details[res.exit_code.str()]}') }
			path_to_line := bold('$path:$line:$col:')
			err_line := last_line.trim_left('\t')
			println('$path_to_line $err')
			println('\t$line | $err_line')
			println('')
			eprintln(res.output)
		}
	}
	return fails, panics
}

fn (mut context Context) start_printing() {
	context.stop_print = false
	println('\n')
	go context.print_periodic_status()
}

fn (mut context Context) stop_printing() {
	context.stop_print = true
	time.sleep_ms(context.period_ms / 5)
}

fn (mut context Context) print_status() {
	term.cursor_up(1)
	eprint('\r  >   ${context.path:-30} | index: ${context.cut_index:5}/${context.max_index - 1:5}\n')
}

fn (mut context Context) print_periodic_status() {
	for !context.stop_print {
		context.print_status()
		for i := 0; i < 10 && !context.stop_print; i++ {
			time.sleep_ms(context.period_ms / 10)
		}
	}
	context.print_status()
}
