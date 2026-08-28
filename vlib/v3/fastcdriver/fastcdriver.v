module fastcdriver

import os
import time
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
			match arg {
				'-o' {
					output = value
				}
				'-b' {
					if value != 'fastc' {
						fail('fastc self-host compiler only supports `-b fastc`')
					}
				}
				'-gc' {
					if value != 'none' {
						fail('fastc self-host compiler only supports `-gc none`')
					}
				}
				'-cc' {
					if value !in ['tinyc', 'tcc'] {
						fail('fastc self-host compiler only supports bundled TinyCC')
					}
				}
				'-d' {
					fail('fastc self-host compiler does not support custom `-d ${value}` defines')
				}
				else {}
			}
			index += 2
			continue
		}
		if arg == '-keepc' {
			keep_c = true
		} else if arg.ends_with('.v') {
			if input != '' {
				fail('fastc self-host compiler accepts only one V source entry file')
			}
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

fn self_command_index(args []string) int {
	mut index := 0
	for index < args.len {
		arg := args[index]
		if arg in ['-o', '-output', '-b', '-backend', '-gc', '-cc', '-d'] {
			index += 2
			continue
		}
		if arg == 'self' {
			return index
		}
		index++
	}
	return -1
}

fn repeat_count_arg(arg string) int {
	if arg.len < 2 || arg[0] != `x` {
		return 0
	}
	for ch in arg[1..].bytes() {
		if !ch.is_digit() {
			return 0
		}
	}
	count := arg[1..].int()
	return if count > 0 { count } else { 0 }
}

fn parse_self_arguments(args []string, command_index int) (int, []string, string) {
	mut repeat_count := 1
	mut has_repeat_count := false
	mut compile_args := []string{}
	mut output := ''
	mut index := 0
	for index < args.len {
		if index == command_index {
			index++
			continue
		}
		arg := args[index]
		if arg in ['-o', '-output', '-b', '-backend', '-gc', '-cc', '-d'] {
			if index + 1 >= args.len {
				fail('fastc self: missing value after `${arg}`')
			}
			value := args[index + 1]
			match arg {
				'-o', '-output' {
					output = value
				}
				'-b', '-backend' {
					if value != 'fastc' {
						fail('fastc self only supports `-b fastc`')
					}
				}
				'-gc' {
					if value != 'none' {
						fail('fastc self only supports `-gc none`')
					}
				}
				'-cc' {
					if value !in ['tinyc', 'tcc'] {
						fail('fastc self only supports bundled TinyCC')
					}
					compile_args << ['-cc', value]
				}
				'-d' {
					fail('fastc self does not support custom `-d ${value}` defines')
				}
				else {}
			}
			index += 2
			continue
		}
		if arg in ['-silent', '-keepc'] {
			compile_args << arg
			index++
			continue
		}
		if arg == '-selfhost' {
			index++
			continue
		}
		count := repeat_count_arg(arg)
		if count > 0 && !has_repeat_count {
			repeat_count = count
			has_repeat_count = true
			index++
			continue
		}
		fail('fastc self does not support `${arg}`')
	}
	compile_args << ['-b', 'fastc', '-gc', 'none', '-selfhost']
	return repeat_count, compile_args, output
}

fn run_self_compiler(compiler string, compile_args []string, output string, source string) {
	mut command := []string{cap: compile_args.len + 4}
	command << os.quoted_path(compiler)
	for arg in compile_args {
		command << os.quoted_path(arg)
	}
	command << ['-o', os.quoted_path(output), os.quoted_path(source)]
	result := os.execute(command.join(' '))
	if result.exit_code != 0 {
		fail(result.output)
	}
	if result.output.len > 0 {
		println(result.output.trim_space())
	}
}

fn unique_self_sibling_path(compiler string, role string) string {
	dir := os.dir(compiler)
	base := os.file_name(compiler)
	for counter := 0; true; counter++ {
		candidate := os.join_path_single(dir, '.${base}.${role}.${os.getpid()}.${counter}')
		if !os.exists(candidate) {
			return candidate
		}
	}
	return compiler
}

// self_replacement_path returns a collision-free sibling where the freshly
// self-built binary can be written before it is swapped onto the compiler.
fn self_replacement_path(compiler string) string {
	return unique_self_sibling_path(compiler, 'self-new')
}

fn replace_self_compiler(compiler string, replacement string) {
	backup_name := if os.user_os() == 'windows' { 'v_old.exe' } else { 'v_old' }
	backup := os.join_path_single(os.dir(compiler), backup_name)
	if backup == compiler {
		staging := unique_self_sibling_path(compiler, 'self-old')
		os.mv(compiler, staging) or { fail(err.msg()) }
		os.mv_by_cp(replacement, compiler) or {
			message := err.msg()
			os.mv(staging, compiler) or {}
			fail(message)
		}
		os.rm(staging) or {}
		return
	}
	if os.exists(backup) {
		os.rm(backup) or { fail(err.msg()) }
	}
	os.mv(compiler, backup) or { fail(err.msg()) }
	os.mv_by_cp(replacement, compiler) or {
		message := err.msg()
		os.mv(backup, compiler) or {}
		fail(message)
	}
}

fn run_self(args []string, command_index int) {
	$if windows {
		fail('fastc self is not yet supported on Windows')
	}
	repeat_count, compile_args, output := parse_self_arguments(args, command_index)
	if output != '' && repeat_count > 1 {
		fail('fastc self does not support xN together with `-o`')
	}
	prefs := pref.new_preferences()
	if prefs.vroot == '' {
		fail('fastc self could not locate the V source tree')
	}
	source := os.join_path(prefs.vroot, 'vlib', 'v3', 'v3.v')
	if !os.is_file(source) {
		fail('fastc self could not find `${source}`')
	}
	compiler := os.real_path(os.executable())
	if compiler == '' || !os.is_executable(compiler) {
		fail('fastc self could not locate its compiler executable')
	}
	if output != '' {
		println('V self compiling (-b fastc)...')
		run_self_compiler(compiler, compile_args, output, source)
		return
	}
	replacement := self_replacement_path(compiler)
	for run_index in 0 .. repeat_count {
		run_label := if repeat_count > 1 { ' [${run_index + 1}/${repeat_count}]' } else { '' }
		println('V self compiling${run_label} (-b fastc)...')
		run_self_compiler(compiler, compile_args, replacement, source)
		replace_self_compiler(compiler, replacement)
	}
	println('V built successfully as executable "${os.file_name(compiler)}".')
}

fn canonical_output_path(path string) string {
	if os.exists(path) {
		return os.real_path(path)
	}
	absolute_path := os.abs_path(path)
	canonical_parent := os.real_path(os.dir(absolute_path))
	return os.join_path_single(canonical_parent, os.file_name(absolute_path))
}

fn fastc_tcc_backtrace_enabled(target_os string, target_arch string) bool {
	return !(target_os == 'macos' && target_arch == 'arm64')
}

// run invokes the standalone FastC compiler or its self-build command.
pub fn run(args []string) {
	command_index := self_command_index(args)
	if command_index >= 0 {
		run_self(args, command_index)
		return
	}
	input, output, keep_c := parse_arguments(args)
	real_input := os.real_path(input)
	if pref.is_test_file_for_backend(real_input, 'fastc')
		|| pref.is_test_file_for_backend(real_input, 'c') {
		fail('fastc self-host compiler does not support test files')
	}
	if canonical_output_path(output) == real_input {
		fail('fastc output path `${output}` aliases input source `${input}`')
	}
	mut prefs := pref.new_preferences()
	if prefs.vroot == '' && real_input.ends_with('/vlib/v3/v3.v') {
		prefs.vroot = os.dir(os.dir(os.dir(real_input)))
	}
	prefs.backend = 'fastc'
	prefs.ccompiler = 'tinyc'
	prefs.building_v = real_input.ends_with('/vlib/v3/v3.v')
	prefs.selfhost = prefs.building_v
	prefs.user_defines = ['fastc_selfhost', 'v3_backend', 'skip_arm64', 'skip_wasm', 'skip_eval']
	backtrace_enabled := fastc_tcc_backtrace_enabled(prefs.normalized_target_os(),
		prefs.target.arch)
	// Mirror the driver's TinyCC compatibility plan (add_v3_tcc_compat_defines):
	// TCC's backtrace runtime cannot be linked on macOS arm64, so builtin must
	// not reference tcc_backtrace there. Descendant generations then compile
	// builtin exactly like the first FastC generation did.
	if !backtrace_enabled {
		prefs.user_defines << 'no_backtrace'
	}

	bench := os.getenv('FASTC_BENCH') != ''
	mut repeat := os.getenv('FASTC_BENCH_REPEAT').int()
	if repeat < 1 {
		repeat = 1
	}
	if bench && repeat > 1 {
		mut warm := fastc.generate_files_with_source_paths([real_input], prefs) or {
			fail(err.msg())
		}
		warm = warm
		mut best_us := i64(0)
		mut sw2 := time.new_stopwatch()
		for iteration in 0 .. repeat {
			sw2.restart()
			fastc.generate_files_with_source_paths([real_input], prefs) or { fail(err.msg()) }
			iter_us := sw2.elapsed().microseconds()
			if iteration == 0 || iter_us < best_us {
				best_us = iter_us
			}
		}
		mut total_lines := 0
		for source_path in warm.source_paths {
			content := os.read_file(source_path) or { '' }
			for ch in content {
				if ch == `\n` {
					total_lines++
				}
			}
		}
		gen_ms := f64(best_us) / 1000.0
		loc_per_s := f64(total_lines) * 1_000_000.0 / f64(best_us)
		eprintln('fastc-bench: files=${warm.source_paths.len} lines=${total_lines} best_gen=${gen_ms:.2f}ms loc/s=${loc_per_s:.0f} (repeat=${repeat})')
	}
	mut sw := time.new_stopwatch()
	generation := fastc.generate_files_with_source_paths([real_input], prefs) or { fail(err.msg()) }
	if bench && repeat == 1 {
		gen_us := sw.elapsed().microseconds()
		mut total_lines := 0
		for source_path in generation.source_paths {
			content := os.read_file(source_path) or { '' }
			for ch in content {
				if ch == `\n` {
					total_lines++
				}
			}
		}
		gen_ms := f64(gen_us) / 1000.0
		loc_per_s := f64(total_lines) * 1_000_000.0 / f64(gen_us)
		eprintln('fastc-bench: files=${generation.source_paths.len} lines=${total_lines} gen=${gen_ms:.2f}ms loc/s=${loc_per_s:.0f}')
	}
	canonical_output := canonical_output_path(output)
	for source_path in generation.source_paths {
		if canonical_output == source_path && source_path != real_input {
			fail('fastc output path `${output}` aliases imported source `${source_path}`')
		}
	}
	c_source := generation.c_source
	build_prefix := '${output}.fastc-build-${os.getpid()}'
	c_path := build_prefix + '.c'
	staged_output := build_prefix + '.out'
	os.write_file(c_path, c_source) or { fail(err.msg()) }
	tcc_dir := os.join_path(prefs.vroot, 'thirdparty', 'tcc')
	tcc := os.join_path_single(tcc_dir, 'tcc.exe')
	tcc_lib := os.join_path_single(tcc_dir, 'lib')
	// The emitted spawn runtime calls pthread functions, which live outside
	// libc on Linux with glibc before 2.34 and on the BSDs.
	mut thread_link_flag := ''
	if generation.uses_threads {
		thread_link_flag = '-lpthread '
	}
	backtrace_flag := if backtrace_enabled { '-bt25 ' } else { '' }
	command := '${os.quoted_path(tcc)} -std=gnu11 ${backtrace_flag}-I${os.quoted_path(os.join_path_single(tcc_lib,
		'include'))} -L${os.quoted_path(tcc_lib)} -w -o ${os.quoted_path(staged_output)} ${os.quoted_path(c_path)} ${thread_link_flag}-lm'
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
