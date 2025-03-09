module testing

import os
import time
import term
import strings
import runtime

pub const empty = term.header(' ', ' ')

// TODO: AGAIN --- this !!!*reliably*!!! fails compilation of `v cmd/tools/vbuild-examples.v` with a cgen error, without `-no-parallel`:
// pub const report_running_period_ms = os.getenv_opt('VTEST_REPORT_RUNNING_PERIOD_MS') or { '60000' }.int() * time.millisecond

pub const report_running_period_ms = get_report_running_period_ms()

fn get_report_running_period_ms() time.Duration {
	return os.getenv_opt('VTEST_REPORT_RUNNING_PERIOD_MS') or { '300_000' }.int() * time.millisecond // 5 minutes by default
}

// NormalReporter implements the interface testing.Reporter.
// It is used by default by `v test .`
// It was extracted by the original non customiseable output implementation directly in cmd/tools/modules/testing/common.v
pub struct NormalReporter {
mut:
	runtime  time.Duration
	comptime time.Duration
	nfiles   int
	njobs    int
	//
	running   shared map[string]LogMessage
	compiling shared map[string]LogMessage
}

pub fn (mut r NormalReporter) session_start(message string, mut ts TestSession) {
	header(message)
	r.nfiles = ts.files.len
	r.njobs = runtime.nr_jobs()
	spawn r.report_current_running_and_compiling_status_periodically()
}

fn (r &NormalReporter) report_current_running_and_compiling_status_periodically() {
	if report_running_period_ms == 0 {
		return
	}
	mut start_t := time.now()
	mut pi := 0
	mut ccompiling := map[string]LogMessage{}
	mut crunning := map[string]LogMessage{}
	mut sb := strings.new_builder(1024)
	for {
		pi++
		time.sleep(report_running_period_ms)
		t := time.now()
		rlock r.compiling {
			ccompiling = r.compiling.clone()
		}
		rlock r.running {
			crunning = r.running.clone()
		}
		ckeys := ccompiling.keys()
		rkeys := crunning.keys()
		sb.writeln('')
		sb.writeln('       >>>>> ${t.format_ss_micro()} | period ${pi:2} | started: ${t - start_t:10} ago | vjobs: ${r.njobs} | _test.v files: ${r.nfiles:5} | C: ${ckeys.len:3} | R: ${rkeys.len:3}')
		for ik, k in ckeys {
			cval := ccompiling[k]
			sb.writeln('       >>>>> compiling ${ik + 1:2}/${ckeys.len:-2}, T: ${cval.flow_id:2}, started: ${t - cval.when:10} ago, `${k}`')
		}
		for ik, k in rkeys {
			cval := crunning[k]
			sb.writeln('       >>>>>   running ${ik + 1:2}/${rkeys.len:-2}, T: ${cval.flow_id:2}, started: ${t - cval.when:10} ago, `${k}`')
		}
		sb.writeln('')
		eprint(sb.str()) // write everything at once, to minimise the chance of interference with the normal output of `v test`
	}
}

pub fn (r &NormalReporter) session_stop(message string, mut ts TestSession) {
	println('${ts.benchmark.total_message(message)} Comptime: ${r.comptime.microseconds() / 1000} ms. Runtime: ${r.runtime.microseconds() / 1000} ms.')
}

// the most general form; it may be useful for other reporters
// in the normal one, it currently does nothing
pub fn (mut r NormalReporter) report(index int, message LogMessage) {
	// eprintln('> ${@METHOD} index: $index | message: $message')
	if message.kind == .compile_begin {
		lock r.compiling {
			r.compiling[message.file] = message
		}
	}
	if message.kind == .compile_end {
		r.comptime += message.took
		lock r.compiling {
			r.compiling.delete(message.file)
		}
	}
	if message.kind == .cmd_end {
		r.runtime += message.took
	}
	if message.kind == .cmd_begin {
		lock r.running {
			r.running[message.file] = message
		}
	}
	if message.kind == .cmd_end {
		lock r.running {
			r.running.delete(message.file)
		}
	}
}

pub fn (r NormalReporter) report_stop() {
	// eprintln('> ${@METHOD}')
	eprintln('')
}

// progress will show the given message normally
// TODO: reconsider if these should be public:
pub fn (r NormalReporter) progress(index int, message string) {
	eprintln(message)
}

// in progress mode, the last line will be rewritten many times, and does not end with \n
// the \n will be printed just once when some progress has been made.
pub fn (r NormalReporter) update_last_line(index int, message string) {
	print('\r${empty}\r${message}')
	flush_stdout()
}

pub fn (r NormalReporter) update_last_line_and_move_to_next(index int, message string) {
	// the last \n is needed, so SKIP/FAIL messages
	// will not get overwritten by the OK ones
	eprint('\r${empty}\r${message}\n')
}

pub fn (r NormalReporter) message(index int, message string) {
	eprintln(message)
}

pub fn (r NormalReporter) divider() {
	h_divider()
}

//

pub fn (r NormalReporter) worker_threads_start(files []string, mut ts TestSession) {
	// eprintln('> ${@METHOD}')
}

pub fn (r NormalReporter) worker_threads_finish(mut ts TestSession) {
	// eprintln('> ${@METHOD}')
}

pub fn (r NormalReporter) list_of_failed_commands(failed_cmds []string) {
	for i, cmd in failed_cmds {
		eprintln(term.failed('To reproduce just failure ${i + 1} run:') + '    ${cmd}')
	}
}
