module fastc

import os
import strings

#flag darwin -I @VEXEROOT/thirdparty/tcc/include

$if !tinyc {
	#flag darwin @VEXEROOT/thirdparty/tcc/lib/libtcc.a
}

#include "libtcc.h"

$if !tinyc && !fastc_selfhost ? {
	#insert "@VEXEROOT/vlib/v3/gen/fastc/libtcc_system_darwin.h"
}

struct C.TCCState {}

fn C.tcc_new() &C.TCCState

fn C.tcc_delete(&C.TCCState)

fn C.tcc_set_lib_path(&C.TCCState, &char)

fn C.tcc_set_error_func(&C.TCCState, voidptr, fn (voidptr, &char))

fn C.tcc_set_options(&C.TCCState, &char) int

fn C.tcc_set_output_type(&C.TCCState, int) int

fn C.tcc_add_file(&C.TCCState, &char) int

fn C.tcc_add_library(&C.TCCState, &char) int

fn C.tcc_output_file(&C.TCCState, &char) int

fn C.v_fastc_tcc_set_skip_codesign(int)

fn C.v_fastc_tcc_skipped_codesign_count() int

const fastc_tcc_output_exe = 2

struct FastcLibtccDiagnostics {
mut:
	messages []string
}

fn fastc_libtcc_error_callback(opaque voidptr, message &char) {
	if isnil(opaque) || isnil(message) {
		return
	}
	mut diagnostics := unsafe { &FastcLibtccDiagnostics(opaque) }
	diagnostics.messages << unsafe { cstring_to_vstring(message) }.clone()
}

fn fastc_libtcc_quote_arg(arg string) string {
	if arg.len > 0 && !arg.contains_any(' \t\r\n\\"') {
		return arg
	}
	mut out := strings.new_builder(arg.len + 2)
	out.write_u8(`"`)
	for ch in arg {
		if ch in [`\\`, `"`] {
			out.write_u8(`\\`)
		}
		out.write_u8(ch)
	}
	out.write_u8(`"`)
	return out.str()
}

fn fastc_libtcc_options(args []string) string {
	mut out := strings.new_builder(args.len * 16)
	for index, arg in args {
		if index > 0 {
			out.write_u8(` `)
		}
		out.write_string(fastc_libtcc_quote_arg(arg))
	}
	return out.str()
}

fn fastc_libtcc_diagnostics(diagnostics &FastcLibtccDiagnostics, fallback string) string {
	if diagnostics.messages.len == 0 {
		return fallback
	}
	return diagnostics.messages.join('\n')
}

fn fastc_libtcc_is_link_input(arg string) bool {
	if !os.is_file(arg) {
		return false
	}
	lower := arg.to_lower()
	return lower.ends_with('.o') || lower.ends_with('.obj') || lower.ends_with('.a')
		|| lower.ends_with('.lib') || lower.ends_with('.dylib') || lower.ends_with('.so')
		|| lower.contains('.so.')
}

fn fastc_libtcc_apply_options(state &C.TCCState, args []string) int {
	if args.len == 0 {
		return 0
	}
	options := fastc_libtcc_options(args)
	return C.tcc_set_options(state, options.str)
}

fn fastc_libtcc_option_args(args []string) []string {
	mut options := []string{cap: args.len}
	for arg in args {
		if !fastc_libtcc_is_link_input(arg) {
			options << arg
		}
	}
	return options
}

fn fastc_prepare_libtcc_link(program string, tcc_lib string, base_args []string, final_args []string) FastcPreparedLink {
	state := C.tcc_new()
	if isnil(state) {
		return FastcPreparedLink{
			program: program
			base_args: base_args.clone()
		}
	}
	diagnostics := &FastcLibtccDiagnostics{}
	C.tcc_set_error_func(state, diagnostics, fastc_libtcc_error_callback)
	C.tcc_set_lib_path(state, tcc_lib.str)
	if fastc_libtcc_apply_options(state, base_args) != 0
		|| fastc_libtcc_apply_options(state, fastc_libtcc_option_args(final_args)) != 0
		|| C.tcc_set_output_type(state, fastc_tcc_output_exe) != 0 {
		C.tcc_delete(state)
		return FastcPreparedLink{
			program: program
			base_args: base_args.clone()
		}
	}
	return FastcPreparedLink{
		state: state
		diagnostics: diagnostics
		program: program
	}
}

fn fastc_libtcc_state(link &FastcPreparedLink) &C.TCCState {
	return unsafe { &C.TCCState(link.state) }
}

fn fastc_libtcc_add_library(state &C.TCCState, name string, diagnostics &FastcLibtccDiagnostics) ?os.Result {
	if C.tcc_add_library(state, name.str) != 0 {
		return os.Result{
			exit_code: 1
			output: fastc_libtcc_diagnostics(diagnostics, 'could not add library `${name}` to the TinyCC link')
		}
	}
	return none
}

fn fastc_finish_libtcc_link(mut link FastcPreparedLink, input_paths []string, final_args []string, output string) os.Result {
	state := fastc_libtcc_state(&link)
	if isnil(state) || isnil(link.diagnostics) {
		mut args := link.base_args.clone()
		args << ['-o', output]
		args << input_paths
		args << final_args
		return fastc_run_command(link.program, args)
	}
	mut diagnostics := unsafe { &FastcLibtccDiagnostics(link.diagnostics) }
	defer {
		C.tcc_delete(state)
		link.state = unsafe { nil }
		link.diagnostics = unsafe { nil }
	}
	for input_path in input_paths {
		if C.tcc_add_file(state, input_path.str) != 0 {
			return os.Result{
				exit_code: 1
				output: fastc_libtcc_diagnostics(diagnostics, 'could not add `${input_path}` to the TinyCC link')
			}
		}
	}
	mut expects_library_name := false
	for arg in final_args {
		if expects_library_name {
			if result := fastc_libtcc_add_library(state, arg, diagnostics) {
				return result
			}
			expects_library_name = false
			continue
		}
		if arg == '-l' {
			expects_library_name = true
			continue
		}
		if arg.starts_with('-l') && arg.len > 2 {
			if result := fastc_libtcc_add_library(state, arg[2..], diagnostics) {
				return result
			}
			continue
		}
		if fastc_libtcc_is_link_input(arg) {
			if C.tcc_add_file(state, arg.str) != 0 {
				return os.Result{
					exit_code: 1
					output: fastc_libtcc_diagnostics(diagnostics, 'could not add `${arg}` to the TinyCC link')
				}
			}
		}
	}
	if expects_library_name {
		return os.Result{
			exit_code: 1
			output: 'missing library name after `-l`'
		}
	}
	C.v_fastc_tcc_set_skip_codesign(1)
	output_result := C.tcc_output_file(state, output.str)
	C.v_fastc_tcc_set_skip_codesign(0)
	if output_result != 0 {
		return os.Result{
			exit_code: 1
			output: fastc_libtcc_diagnostics(diagnostics, 'TinyCC could not write `${output}`')
		}
	}
	return os.Result{
		exit_code: 0
		output: diagnostics.messages.join('\n')
	}
}

fn fastc_discard_libtcc_link(mut link FastcPreparedLink) {
	state := fastc_libtcc_state(&link)
	if !isnil(state) {
		C.tcc_delete(state)
	}
	link.state = unsafe { nil }
	link.diagnostics = unsafe { nil }
}
