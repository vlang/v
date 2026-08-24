module fastcdriver

import os
import v3.gen.fastc
import v3.pref

@[noreturn]
fn fail(message string) {
	eprintln(message)
	exit(1)
}

fn parse_arguments(args []string) (string, string, bool) {
	mut input := ''
	mut output := ''
	mut keep_c := false
	mut index := 0
	for index < args.len {
		arg := args[index]
		if arg in ['-o', '-b', '-gc', '-cc', '-d'] {
			if index + 1 >= args.len {
				fail('fastc: missing value after `${arg}`')
			}
			value := args[index + 1]
			if arg == '-o' {
				output = value
			} else if arg == '-b' && value != 'fastc' {
				fail('fastc self-host compiler only supports `-b fastc`')
			}
			index += 2
			continue
		}
		if arg == '-keepc' {
			keep_c = true
		} else if arg.ends_with('.v') {
			input = arg
		} else if arg !in ['-silent', '-selfhost'] {
			fail('fastc self-host compiler does not support `${arg}`')
		}
		index++
	}
	if input == '' {
		fail('fastc: expected one V source entry file')
	}
	if output == '' {
		output = os.file_name(input).all_before_last('.')
	}
	return input, output, keep_c
}

// run builds a program using only FastC's scanner-to-C pipeline.
pub fn run(args []string) {
	input, output, keep_c := parse_arguments(args)
	real_input := os.real_path(input)
	mut prefs := pref.new_preferences()
	if prefs.vroot == '' && real_input.ends_with('/vlib/v3/v3.v') {
		prefs.vroot = os.dir(os.dir(os.dir(real_input)))
	}
	prefs.backend = 'fastc'
	prefs.ccompiler = 'tinyc'
	prefs.building_v = real_input.ends_with('/vlib/v3/v3.v')
	prefs.selfhost = prefs.building_v
	prefs.user_defines = ['fastc_selfhost', 'v3_backend', 'skip_arm64', 'skip_wasm', 'skip_eval']

	c_source := fastc.generate_files([real_input], prefs) or { fail(err.msg()) }
	build_prefix := '${output}.fastc-build-${os.getpid()}'
	c_path := build_prefix + '.c'
	staged_output := build_prefix + '.out'
	os.write_file(c_path, c_source) or { fail(err.msg()) }
	tcc_dir := os.join_path(prefs.vroot, 'thirdparty', 'tcc')
	tcc := os.join_path_single(tcc_dir, 'tcc.exe')
	tcc_lib := os.join_path_single(tcc_dir, 'lib')
	command := '${os.quoted_path(tcc)} -std=gnu11 -I${os.quoted_path(os.join_path_single(tcc_lib,
		'include'))} -L${os.quoted_path(tcc_lib)} -w -o ${os.quoted_path(staged_output)} ${os.quoted_path(c_path)} -lm'
	result := os.execute(command)
	if result.exit_code != 0 {
		fail(result.output)
	}
	os.mv(staged_output, output) or { fail(err.msg()) }
	if keep_c {
		os.mv(c_path, output + '.c') or { fail(err.msg()) }
	} else {
		os.rm(c_path) or {}
	}
}
