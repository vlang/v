module fastcdriver

import os
import time
import v3.cmdexec
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

fn validate_output_source_paths(output string, real_input string, source_paths []string) ! {
	canonical_output := canonical_output_path(output)
	for source_path in source_paths {
		if canonical_output == source_path && source_path != real_input {
			return error('fastc output path `${output}` aliases imported source `${source_path}`')
		}
	}
}

fn fastc_canonical_vroot(vroot string) string {
	if vroot == '' {
		return ''
	}
	return os.real_path(vroot)
}

fn fastc_tcc_backtrace_enabled(target_os string, target_arch string) bool {
	return !(target_os == 'macos' && target_arch == 'arm64')
}

fn tcc_host_system_flags(target_os string) []string {
	if target_os != os.user_os() || target_os == 'windows' {
		return []
	}
	mut flags := ['-I/usr/local/include', '-L/usr/local/lib']
	if target_os == 'macos' {
		sdk_root := macos_sdk_root()
		if sdk_root != '' {
			flags << '-I${os.join_path(sdk_root, 'usr', 'include')}'
			flags << '-L${os.join_path(sdk_root, 'usr', 'lib')}'
		}
	}
	return flags
}

fn macos_sdk_root() string {
	return cmdexec.macos_sdk_root()
}

struct FastcBenchSample {
	gen_us i64
	files  int
	lines  int
}

fn fastc_bench_source_line_count(paths []string) int {
	mut total_lines := 0
	for source_path in paths {
		content := os.read_file(source_path) or { '' }
		for ch in content {
			if ch == `\n` {
				total_lines++
			}
		}
	}
	return total_lines
}

fn fastc_measure_generation(real_input string, prefs &pref.Preferences) !FastcBenchSample {
	mut sw := time.new_stopwatch()
	generation := fastc.generate_files_with_source_paths([real_input], prefs)!
	return FastcBenchSample{
		gen_us: sw.elapsed().microseconds()
		files: generation.source_paths.len
		lines: fastc_bench_source_line_count(generation.source_paths)
	}
}

fn fastc_parse_bench_child_output(output string) ?FastcBenchSample {
	for line in output.split_into_lines() {
		if !line.starts_with('fastc-bench-child ') {
			continue
		}
		parts := line.split(' ')
		if parts.len != 4 {
			return none
		}
		return FastcBenchSample{
			gen_us: parts[1].i64()
			files: parts[2].int()
			lines: parts[3].int()
		}
	}
	return none
}

fn fastc_run_bench_child(args []string) FastcBenchSample {
	result := cmdexec.run(os.executable(), args)
	if result.exit_code != 0 {
		fail(result.output)
	}
	return fastc_parse_bench_child_output(result.output) or {
		fail('fastc benchmark child returned no timing sample:\n${result.output}')
	}
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
	if pref.is_test_file_for_backend(real_input, 'fastc') || pref.is_test_file_for_backend(real_input, 'c') {
		fail('fastc self-host compiler does not support test files')
	}
	if canonical_output_path(output) == real_input {
		fail('fastc output path `${output}` aliases input source `${input}`')
	}
	mut prefs := pref.new_preferences()
	if prefs.vroot == '' && real_input.ends_with('/vlib/v3/v3.v') {
		prefs.vroot = os.dir(os.dir(os.dir(real_input)))
	}
	prefs.vroot = fastc_canonical_vroot(prefs.vroot)
	prefs.backend = 'fastc'
	prefs.ccompiler = 'tinyc'
	prefs.building_v = real_input.ends_with('/vlib/v3/v3.v')
	prefs.selfhost = prefs.building_v
	$if arm64 ? {
		prefs.target = pref.Target{
			os: 'macos'
			arch: 'arm64'
			abi: 'darwin'
			endian: 'little'
			pointer_bits: 64
			object_format: 'macho'
		}
		prefs.user_defines = ['fastc_selfhost', 'v3_backend', 'v3_no_parallel', 'arm64', 'skip_wasm',
			'skip_eval']
		fastc.generate_arm64_files([real_input], prefs, output) or { fail(err.msg()) }
		return
	}
	prefs.user_defines = ['fastc_selfhost', 'v3_backend', 'skip_arm64', 'skip_wasm', 'skip_eval']
	backtrace_enabled := fastc_tcc_backtrace_enabled(prefs.normalized_target_os(), prefs.target.arch)
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
	if bench && os.getenv('FASTC_BENCH_CHILD') != '' {
		sample := fastc_measure_generation(real_input, prefs) or { fail(err.msg()) }
		println('fastc-bench-child ${sample.gen_us} ${sample.files} ${sample.lines}')
		return
	}
	if bench && repeat > 1 {
		old_child_marker := os.getenv_opt('FASTC_BENCH_CHILD')
		os.setenv('FASTC_BENCH_CHILD', '1', true)
		warm := fastc_run_bench_child(args)
		mut best_us := i64(0)
		for iteration in 0 .. repeat {
			sample := fastc_run_bench_child(args)
			if iteration == 0 || sample.gen_us < best_us {
				best_us = sample.gen_us
			}
		}
		if old_child_marker_value := old_child_marker {
			os.setenv('FASTC_BENCH_CHILD', old_child_marker_value, true)
		} else {
			os.unsetenv('FASTC_BENCH_CHILD')
		}
		gen_ms := f64(best_us) / 1000.0
		loc_per_s := f64(warm.lines) * 1_000_000.0 / f64(best_us)
		eprintln('fastc-bench: files=${warm.files} lines=${warm.lines} best_gen=${gen_ms:.2f}ms loc/s=${loc_per_s:.0f} (repeat=${repeat})')
	}
	loop := os.getenv('FASTC_BENCH_LOOP').int()
	if bench && loop > 0 {
		// Repeat generation in-process so an external sampler can profile it.
		for _ in 0 .. loop {
			fastc.generate_files_with_source_paths([real_input], prefs) or { fail(err.msg()) }
		}
	}
	mut sw := time.new_stopwatch()
	generation := fastc.generate_files_with_source_paths([real_input], prefs) or { fail(err.msg()) }
	if bench && repeat == 1 {
		gen_us := sw.elapsed().microseconds()
		total_lines := fastc_bench_source_line_count(generation.source_paths)
		gen_ms := f64(gen_us) / 1000.0
		loc_per_s := f64(total_lines) * 1_000_000.0 / f64(gen_us)
		eprintln('fastc-bench: files=${generation.source_paths.len} lines=${total_lines} gen=${gen_ms:.2f}ms loc/s=${loc_per_s:.0f}')
	}
	validate_output_source_paths(output, real_input, generation.source_paths) or { fail(err.msg()) }
	build_prefix := '${output}.fastc-build-${os.getpid()}'
	c_path := build_prefix + '.c'
	staged_output := build_prefix + '.out'
	tcc_dir := os.join_path(prefs.vroot, 'thirdparty', 'tcc')
	tcc := os.join_path_single(tcc_dir, 'tcc.exe')
	tcc_lib := os.join_path_single(tcc_dir, 'lib')
	mut cc_args := ['-std=gnu11', '-I${os.join_path_single(tcc_lib, 'include')}', '-L${tcc_lib}']
	cc_args << tcc_host_system_flags(prefs.normalized_target_os())
	cc_args << generation.c_flags
	if backtrace_enabled {
		cc_args << '-bt25'
	}
	// A call without a prototype would silently truncate a pointer result
	// (the C carries no headers): it is an error, not a warning.
	cc_args << '-Werror=implicit-function-declaration'
	mut link_libs := []string{}
	if generation.uses_threads {
		// The emitted spawn runtime calls pthread functions, which live outside
		// libc on Linux with glibc before 2.34 and on the BSDs.
		link_libs << '-lpthread'
	}
	link_libs << '-lm'
	bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
	cc_sw := time.new_stopwatch()
	// The program's translation units are compiled by concurrent TinyCC
	// processes and linked; a program that does not split is compiled as
	// one file.
	unit_paths := fastc.fastc_write_c_units(build_prefix, generation.c_pieces, generation.units, fastc.fastc_tcc_job_count(prefs)) or { fail(err.msg()) }
	if bench_phases {
		eprintln('fastc-phase tcc.units_written ${cc_sw.elapsed().microseconds()}us units=${unit_paths.len}')
	}
	mut shim_dir := fastc.FastcCodesignShim{}
	mut result := os.Result{}
	mut sign_in_process := false
	if unit_paths.len > 1 {
		link_worker := spawn fastc.fastc_prepare_link(tcc, tcc_lib, cc_args, link_libs)
		unit_objects := fastc.fastc_compile_c_units(tcc, cc_args, unit_paths) or {
			mut prepared_link := link_worker.wait()
			fastc.fastc_discard_link(mut prepared_link)
			fastc.fastc_remove_codesign_shim_dir(shim_dir)
			fastc.fastc_remove_c_units(unit_paths)
			// The whole program is kept as one file for the error message.
			fastc.write_c_pieces(c_path, generation.c_pieces) or {}
			fail(err.msg())
		}
		if bench_phases {
			eprintln('fastc-phase tcc.units_compiled ${cc_sw.elapsed().microseconds()}us')
		}
		link_inputs := unit_objects.clone()
		mut prepared_link := link_worker.wait()
		sign_in_process = fastc.fastc_prepared_link_skips_codesign(&prepared_link)
		if !sign_in_process {
			shim_dir = fastc.fastc_codesign_shim_dir()
			sign_in_process = shim_dir.dir != ''
		}
		result = fastc.fastc_finish_link(mut prepared_link, link_inputs, link_libs, staged_output)
		if bench_phases {
			eprintln('fastc-phase tcc.linked ${cc_sw.elapsed().microseconds()}us')
		}
	} else {
		fastc.write_c_pieces(c_path, generation.c_pieces) or {
			fastc.fastc_remove_codesign_shim_dir(shim_dir)
			fail(err.msg())
		}
		mut single_args := cc_args.clone()
		single_args << ['-o', staged_output, c_path]
		single_args << link_libs
		shim_dir = fastc.fastc_codesign_shim_dir()
		sign_in_process = shim_dir.dir != ''
		result = fastc.fastc_run_command(tcc, single_args)
	}
	fastc.fastc_remove_codesign_shim_dir(shim_dir)
	fastc.fastc_remove_c_units(unit_paths)
	if result.exit_code != 0 {
		fail(result.output)
	}
	if sign_in_process {
		fastc.fastc_sign_macho_adhoc(staged_output) or {
			fail('could not sign ${staged_output}: ${err.msg()}')
		}
	}
	os.mv(staged_output, output) or { fail(err.msg()) }
	if keep_c {
		if unit_paths.len > 1 {
			fastc.write_c_pieces(output + '.c', generation.c_pieces) or { fail(err.msg()) }
		} else {
			os.mv(c_path, output + '.c') or { fail(err.msg()) }
		}
	} else {
		os.rm(c_path) or {}
	}
}
