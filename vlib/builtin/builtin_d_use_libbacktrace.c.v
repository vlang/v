[has_globals]
module builtin

// TODO: not yet final. library needs to be amalgamated and packaged
#flag @VEXEROOT/thirdparty/libbacktrace/.libs/libbacktrace.a

#include "@VEXEROOT/thirdparty/libbacktrace/backtrace.h"
#include "@VEXEROOT/thirdparty/libbacktrace/backtrace-supported.h"

// NOTE: Don't mark this as a [typedef] or it may cause compiler errors!
struct C.backtrace_state {
	// filename &char
}

type BacktraceErrorCallback = fn (data voidptr, msg &char, errnum int) voidptr
type BacktraceFullCallback = fn (data voidptr, pc voidptr, filename &char, lineno int, func &char) &int

fn C.backtrace_create_state(filename &char, threaded int, error_callback BacktraceErrorCallback, data voidptr) &C.backtrace_state
fn C.backtrace_full(state &C.backtrace_state, skip int, cb BacktraceFullCallback, err_cb BacktraceErrorCallback, data voidptr) int

__global bt_state = C.backtrace_create_state(0, C.BACKTRACE_SUPPORTS_THREADS, bt_error_handler, 0)

// for bt_error_callback
// struct BacktraceData {
// 	state &C.backtrace_state
// }

fn bt_print_callback(data voidptr, pc voidptr, filename_ptr &char, line int, fn_name_ptr &char) int {
	filename := if isnil(filename_ptr) {'???'} else {unsafe{ filename_ptr.vstring() }}
	fn_name := if isnil(fn_name_ptr) {'???'} else { (unsafe { fn_name_ptr.vstring() }).replace('__', '.') }
	pc_64 := u64(pc)

	println('0x${pc_64:x} $fn_name')
	println('\tat $filename:$line')
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
	// data := &BacktraceData{bt_state}
	C.backtrace_full(bt_state, frames_to_skip + 1, bt_print_callback, bt_error_callback, 0)
}
