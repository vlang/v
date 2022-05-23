[has_globals]
module builtin

#flag -I@VEXEROOT/thirdparty/libbacktrace
#flag @VEXEROOT/thirdparty/libbacktrace/backtrace.o
#include <backtrace.h>

// NOTE: Don't mark this as a [typedef] or it may cause compiler errors!
struct C.backtrace_state {
	// filename &char
}

type BacktraceErrorCallback = fn (data voidptr, msg &char, errnum int) voidptr

type BacktraceFullCallback = fn (data voidptr, pc voidptr, filename &char, lineno int, func &char) &int

fn C.backtrace_create_state(filename &char, threaded int, error_callback BacktraceErrorCallback, data voidptr) &C.backtrace_state
fn C.backtrace_full(state &C.backtrace_state, skip int, cb BacktraceFullCallback, err_cb BacktraceErrorCallback, data voidptr) int

__global bt_state = init_bt_state()

fn init_bt_state() &C.backtrace_state {
	$if !tinyc {
		mut filename := &char(0)
		$if windows {
			filename = unsafe { string_from_wide(&&u16(g_main_argv)[0]).str }
		} $else {
			filename = unsafe { &&char(g_main_argv)[0] }
		}
		return C.backtrace_create_state(filename, 1, bt_error_handler, 0)
	}
	return &C.backtrace_state(0)
}

// for bt_error_callback
struct BacktraceOptions {
	stdin bool = true
}

fn bt_print_callback(data &BacktraceOptions, pc voidptr, filename_ptr &char, line int, fn_name_ptr &char) int {
	filename := if isnil(filename_ptr) { '???' } else { unsafe { filename_ptr.vstring() } }
	fn_name := if isnil(fn_name_ptr) {
		'???'
	} else {
		(unsafe { fn_name_ptr.vstring() }).replace('__', '.')
	}
	// keep it for later
	// pc_64 := u64(pc)
	bt_str := '$filename:$line: by $fn_name'
	if data.stdin {
		println(bt_str)
	} else {
		eprintln(bt_str)
	}
	return 0
}

fn bt_error_callback(data voidptr, msg_ptr &char, errnum int) {
	// if !isnil(data) && !isnil(data.state) && !isnil(data.state.filename) {
	// 	filename := unsafe{ data.state.filename.vstring() }
	// 	eprint('$filename: ')
	// }

	msg := unsafe { msg_ptr.vstring() }
	eprint('libbacktrace: $msg')
	if errnum > 0 {
		eprint(': ${C.strerror(errnum)}')
	}

	eprintln('')
}

// for backtrace_create_state only
fn bt_error_handler(data voidptr, msg &char, errnum int) {
	eprint('libbacktrace: ')
	eprint(unsafe { msg.vstring() })
	if errnum > 0 {
		eprint(': ${C.strerror(errnum)}')
	}
	eprintln('')
	exit(1)
}

[noinline]
fn print_libbacktrace(frames_to_skip int) {
	$if no_backtrace ? {
		return
	}
	data := &BacktraceOptions{}
	C.backtrace_full(bt_state, frames_to_skip, bt_print_callback, bt_error_callback, data)
}

[noinline]
fn eprint_libbacktrace(frames_to_skip int) {
	$if no_backtrace ? {
		return
	}
	data := &BacktraceOptions{
		stdin: false
	}
	C.backtrace_full(bt_state, frames_to_skip, bt_print_callback, bt_error_callback, data)
}
