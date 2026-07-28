import os
import time
import v3.cmdexec
import v3.pref

const driver_cli_vlib_dir = os.dir(os.dir(os.dir(@FILE)))
const driver_cli_v3_dir = os.dir(os.dir(@FILE))
const driver_cli_v3_src = os.join_path(driver_cli_v3_dir, 'v3.v')

fn build_driver_cli_v3(root string) string {
	return build_driver_cli_v3_with_flags(root, [])
}

fn build_driver_cli_v3_with_flags(root string, flags []string) string {
	bin := os.join_path(root, 'v3_driver_cli')
	mut args := ['-gc', 'none']
	args << flags
	args << ['-path', '${driver_cli_vlib_dir}|@vlib|@vmodules', '-o', bin, driver_cli_v3_src]
	mut result := os.Result{}
	$if macos {
		mut environment := os.environ()
		environment['V_MACOS_V3_BOOTSTRAP'] = '1'
		result = run_driver_with_environment(@VEXE, args, environment)
	} $else {
		result = cmdexec.run(@VEXE, args)
	}
	assert result.exit_code == 0, result.output
	return bin
}

fn assert_driver_cli_failure(v3_bin string, args []string, message string) {
	result := cmdexec.run(v3_bin, args)
	assert result.exit_code != 0
	assert result.output.contains(message), result.output
}

fn test_v3_build_rejects_garbage_collectors() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_gc_build_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	for mode in ['boehm', 'boehm_full', 'boehm_incr', 'boehm_full_opt', 'boehm_incr_opt',
		'boehm_leak', 'vgc'] {
		output := os.join_path(root, 'v3_${mode}')
		result := cmdexec.run(@VEXE, ['-gc', mode, '-path', '${driver_cli_vlib_dir}|@vlib|@vmodules',
			'-o', output, driver_cli_v3_src])
		assert result.exit_code != 0
		assert result.output.contains('v3 must be built without a garbage collector'), result.output
		assert !os.is_file(output)
	}
	for define in ['gcboehm', 'gcboehm_full', 'gcboehm_incr', 'gcboehm_opt', 'gcboehm_leak', 'vgc'] {
		output := os.join_path(root, 'v3_define_${define}')
		result := cmdexec.run(@VEXE, ['-gc', 'none', '-d', define, '-path',
			'${driver_cli_vlib_dir}|@vlib|@vmodules', '-o', output, driver_cli_v3_src])
		assert result.exit_code != 0
		assert result.output.contains('v3 must be built without a garbage collector'), result.output
		assert !os.is_file(output)
	}
}

fn test_standard_v3_excludes_ownership_checker() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_no_ownership_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'main.v')
	os.write_file(source, "fn main() {
	println('no ownership')
}
")!
	output := os.join_path(root, 'no_ownership')
	compile := cmdexec.run(v3_bin, ['-o', output, source])
	assert compile.exit_code == 0, compile.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'no ownership\n'
	assert_driver_cli_failure(v3_bin, ['-ownership', source],
		'ownership support is not compiled into this v3 executable')
	assert_driver_cli_failure(v3_bin, ['-d', 'ownership', source],
		'ownership support is not compiled into this v3 executable')
	assert_driver_cli_failure(v3_bin, ['-downership', source],
		'ownership support is not compiled into this v3 executable')
}

fn test_explicit_arm64_import_unskips_ssa_dependencies() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_arm64_import_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

import v3.gen.arm64

fn main() {}
')!
	output := os.join_path(root, 'arm64_import')
	compile := cmdexec.run(v3_bin, ['-o', output, source])
	assert compile.exit_code == 0, compile.output
}

fn test_driver_cflags_include_dir_is_visible_to_header_inliner() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_cflags_include_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	include_dir := os.join_path(root, 'headers with spaces')
	os.mkdir_all(include_dir) or { panic(err) }
	os.write_file(os.join_path(include_dir, 'cli_header.h'), 'typedef struct CliHeaderValue {
	int value;
} CliHeaderValue;

static inline int cli_header_value(CliHeaderValue* item) {
	return item->value;
}
') or {
		panic(err)
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

#include "cli_header.h"

@[typedef]
struct C.CliHeaderValue {
	value int
}

fn C.cli_header_value(item &C.CliHeaderValue) int

fn main() {
	mut item := C.CliHeaderValue{
		value: 73
	}
	println(C.cli_header_value(&item))
}
') or {
		panic(err)
	}
	output := os.join_path(root, 'cli_header_program')
	keep_c_dir := os.join_path(root, 'kept_c')
	os.mkdir(keep_c_dir)!
	mut environment := os.environ()
	environment['VTMP'] = keep_c_dir
	compile := run_driver_with_environment(v3_bin, ['-nocache', '-keepc', '-cflags',
		'-I "${include_dir}"', '-o', output, source], environment)
	assert compile.exit_code == 0, compile.output
	assert !os.exists(output + '.c')
	kept_files := kept_c_files(keep_c_dir)
	assert kept_files.len == 1, kept_files.str()
	generated_c := os.read_file(kept_files[0])!
	assert !generated_c.contains('#include "cli_header.h"'), generated_c
	assert generated_c.contains('static inline int cli_header_value(CliHeaderValue* item)')
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '73'
}

fn run_driver_with_stdin_file(v3_bin string, args []string, stdin_path string) os.Result {
	mut process := os.new_process(v3_bin)
	process.set_args(args)
	process.set_stdin_path(stdin_path)
	return collect_driver_process_result(mut process)
}

fn collect_driver_process_result(mut process os.Process) os.Result {
	process.set_redirect_stdio()
	process.run()
	process.wait()
	mut output := process.stdout_slurp()
	output += process.stderr_slurp()
	if process.err.len > 0 {
		output += process.err
	}
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	return os.Result{
		exit_code: exit_code
		output:    output
	}
}

fn run_driver_with_environment(v3_bin string, args []string, environment map[string]string) os.Result {
	mut process := os.new_process(v3_bin)
	process.set_args(args)
	process.set_environment(environment)
	return collect_driver_process_result(mut process)
}

fn test_driver_persistent_macos_output_survives_cache_removal() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_driver_persistent_output_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3_with_flags(root, ['-prealloc'])
	source := os.join_path(root, 'persistent.v')
	os.write_file(source, "fn main() {
	println('persistent-output-ok')
}
")!
	output := source.all_before_last('.v')
	cache_dir := os.join_path(root, 'cache')
	os.mkdir_all(cache_dir) or { panic(err) }
	mut environment := os.environ()
	environment['V3CACHE'] = cache_dir
	compile := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel', '-no-memory-limit',
		source], environment)
	assert compile.exit_code == 0, compile.output
	assert os.is_file(output)
	os.rmdir_all(cache_dir) or { panic(err) }
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'persistent-output-ok\n', run.output
}

fn kept_c_files(dir string) []string {
	mut files := (os.ls(dir) or { return []string{} }).filter(it.ends_with('.tmp.c'))
	files.sort()
	return files.map(os.join_path_single(dir, it))
}

fn test_driver_macos_wrapv_and_cg_link_flags() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_driver_macos_c_flags_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3_with_flags(root, ['-prealloc'])
	source := os.join_path(root, 'signed_overflow.v')
	os.write_file(source, '#flag -O2

fn increment_is_greater(value int) bool {
	return value + 1 > value
}

fn main() {
	println(increment_is_greater(int(2147483647)))
}
')!
	output := os.join_path(root, 'signed_overflow')
	compile := cmdexec.run(v3_bin, ['-nocache', '-no-memory-limit', '-prod', '-showcc', '-o', output,
		source])
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('-fwrapv'), compile.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'false\n', run.output

	tcc_output := os.join_path(root, 'signed_overflow_tcc')
	tcc_compile := cmdexec.run(v3_bin, ['-nocache', '-no-memory-limit', '-showcc', '-o', tcc_output,
		source])
	assert tcc_compile.exit_code == 0, tcc_compile.output
	assert tcc_compile.output.contains('tcc.exe'), tcc_compile.output
	assert tcc_compile.output.contains('-fwrapv'), tcc_compile.output

	cg_output := os.join_path(root, 'signed_overflow_cg')
	cg_compile := cmdexec.run(v3_bin, ['-nocache', '-no-memory-limit', '-prod', '-showcc', '-cg',
		'-o', cg_output, source])
	assert cg_compile.exit_code == 0, cg_compile.output
	assert cg_compile.output.contains('-Wl,-export_dynamic'), cg_compile.output

	g_output := os.join_path(root, 'signed_overflow_g')
	g_compile := cmdexec.run(v3_bin, ['-nocache', '-no-memory-limit', '-prod', '-showcc', '-g',
		'-o', g_output, source])
	assert g_compile.exit_code == 0, g_compile.output
	assert !g_compile.output.contains('-Wl,-export_dynamic'), g_compile.output
}

fn test_driver_requests_macos_compatibility_for_inline_assembly() {
	$if amd64 || arm64 {
		root := os.join_path(os.vtmp_dir(), 'v3_driver_inline_asm_fallback_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		v3_bin := build_driver_cli_v3(root)
		source := os.join_path(root, 'inline_asm.v')
		arch := pref.host_arch()
		os.write_file(source, 'fn main() {
	asm ${arch} {
		nop
	}
}
')!
		fallback_file := os.join_path(root, 'fallback')
		mut environment := os.environ()
		environment['V_MACOS_V3_FALLBACK_FILE'] = fallback_file
		result := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel', source],
			environment)
		assert result.exit_code != 0
		assert result.output == '', result.output
		assert os.read_file(fallback_file)! == 'inline_asm'
		os.rm(fallback_file)!
		environment_source := os.join_path(root, 'fallback_environment.v')
		os.write_file(environment_source, "import os

fn main() {
	println(os.getenv('V_MACOS_V3_FALLBACK_FILE'))
}
")!
		environment_run := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel',
			'-no-memory-limit', 'run', environment_source], environment)
		assert environment_run.exit_code == 0, environment_run.output
		assert environment_run.output == '\n', environment_run.output
		assert !os.exists(fallback_file)
	}
}

fn test_driver_requests_macos_compatibility_for_c_compilation_errors() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_c_error_fallback_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'missing_symbol.c.v')
	os.write_file(source, 'fn C.v3_missing_symbol()

fn main() {
	C.v3_missing_symbol()
}
')!
	fallback_file := os.join_path(root, 'fallback')
	report_dir := os.join_path(root, 'c_error')
	mut environment := os.environ()
	environment['V_MACOS_V3_FALLBACK_FILE'] = fallback_file
	environment['V_MACOS_V3_C_ERROR_DIR'] = report_dir
	result := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel', '-nocache',
		'-no-memory-limit', source], environment)
	assert result.exit_code != 0
	assert result.output == '', result.output
	assert os.read_file(fallback_file)! == 'c_compilation_error'
	assert os.read_file(os.join_path(report_dir, 'compiler'))!.trim_space() != ''
	assert os.read_file(os.join_path(report_dir, 'output'))!.to_lower().contains('v3_missing_symbol')
	source_name := os.read_file(os.join_path(report_dir, 'source_name'))!.trim_space()
	assert source_name == 'src.c'
	assert os.is_file(os.join_path(report_dir, source_name))
}

fn test_driver_cg_selects_debug_module_files() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_cg_debug_files_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	project := os.join_path(root, 'project')
	variant_dir := os.join_path(project, 'variant')
	os.mkdir_all(variant_dir) or { panic(err) }
	os.write_file(os.join_path(project, 'v.mod'), "Module {
	name: 'debug_file_selection'
}
")!
	os.write_file(os.join_path(project, 'main.v'), 'module main

import variant

fn main() {
	println(variant.selected())
}
')!
	os.write_file(os.join_path(variant_dir, 'variant_d_debug.v'), "module variant

pub fn selected() string {
	return 'debug'
}
")!
	os.write_file(os.join_path(variant_dir, 'variant_notd_debug.v'), "module variant

pub fn selected() string {
	return 'release'
}
")!
	output := os.join_path(root, 'debug_file_selection')
	compile := cmdexec.run(v3_bin, ['-silent', '-no-parallel', '-no-memory-limit', '-cg', '-o',
		output, project])
	assert compile.exit_code == 0, compile.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'debug\n', run.output
}

fn test_driver_propagates_default_compiler_and_hash_pseudos() {
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'v3_driver_compiler_hashes_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		v3_bin := build_driver_cli_v3(root)
		project := os.join_path(root, 'project')
		module_dir := os.join_path(project, 'compilerinfo')
		os.mkdir_all(module_dir) or { panic(err) }
		os.write_file(os.join_path(project, 'v.mod'), "Module {
	name: 'compiler_hash_selection'
}
")!
		os.write_file(os.join_path(project, 'main.v'), 'module main

import compilerinfo
import os

fn main() {
	println(compilerinfo.values().join("|"))
	println(os.getenv("V_MACOS_V3_VHASH") + "|" + os.getenv("V_MACOS_V3_VCURRENT_HASH"))
}
')!
		os.write_file(os.join_path(module_dir, 'compilerinfo.v'), 'module compilerinfo

pub fn values() []string {
	mut rows := []string{}
	$if clang {
		rows << "clang"
	} $else $if gcc {
		rows << "gcc"
	} $else {
		rows << "other"
	}
	rows << @CCOMPILER
	rows << @VHASH
	rows << @VCURRENTHASH
	$if @VHASH == "delegated-build-hash" && @VCURRENTHASH == "delegated-current-hash" {
		rows << "hash-condition"
	}
	return rows
}
')!
		output := os.join_path(root, 'compiler_hash_selection')
		mut environment := os.environ()
		environment['V_MACOS_V3_VHASH'] = 'delegated-build-hash'
		environment['V_MACOS_V3_VCURRENT_HASH'] = 'delegated-current-hash'
		environment['V3CACHE'] = os.join_path(root, 'cache')
		compile := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel', '-no-memory-limit',
			'-o', output, 'run', project], environment)
		assert compile.exit_code == 0, compile.output
		assert compile.output == 'clang|clang|delegated-build-hash|delegated-current-hash|hash-condition\n|\n', compile.output

		run := cmdexec.run(output, [])
		assert run.exit_code == 0, run.output
		assert run.output == 'clang|clang|delegated-build-hash|delegated-current-hash|hash-condition\n|\n', run.output

		environment['V_MACOS_V3_VHASH'] = 'second-build-hash'
		environment['V_MACOS_V3_VCURRENT_HASH'] = 'second-current-hash'
		second_output := os.join_path(root, 'compiler_hash_selection_second')
		second_compile := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel',
			'-no-memory-limit', '-o', second_output, project], environment)
		assert second_compile.exit_code == 0, second_compile.output
		second_run := cmdexec.run(second_output, [])
		assert second_run.exit_code == 0, second_run.output
		assert second_run.output == 'clang|clang|second-build-hash|second-current-hash\n|\n', second_run.output
	}
}

fn test_driver_preserves_macos_launcher_caller_environment() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_caller_environment_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'caller_environment.v')
	os.write_file(source, "import os

const compile_vexe = \$env('VEXE')
const compile_vchild = \$env('VCHILD')
const compile_private = \$env('V_MACOS_V3_CALLER_VEXE')

fn env_value(name string) string {
	return os.getenv_opt(name) or { '<unset>' }
}

fn main() {
	println('compile:' + compile_vexe + '|' + compile_vchild)
	println('runtime:' + env_value('VEXE') + '|' + env_value('VCHILD'))
	println('private:' + compile_private + '|' + env_value('V_MACOS_V3_CALLER_VEXE') + '|' +
		env_value('V_MACOS_V3_CALLER_VCHILD'))
}
")!
	cache_dir := os.join_path(root, 'cache')
	mut unset_environment := os.environ()
	unset_environment['VEXE'] = @VEXE
	unset_environment['VCHILD'] = 'true'
	unset_environment['V_MACOS_V3_CALLER_VEXE'] = ''
	unset_environment['V_MACOS_V3_CALLER_VEXE_PRESENT'] = '0'
	unset_environment['V_MACOS_V3_CALLER_VCHILD'] = ''
	unset_environment['V_MACOS_V3_CALLER_VCHILD_PRESENT'] = '0'
	unset_environment['V3CACHE'] = cache_dir
	unset_output := os.join_path(root, 'caller_unset')
	unset_run := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel', '-no-memory-limit',
		'-o', unset_output, 'run', source], unset_environment)
	assert unset_run.exit_code == 0, unset_run.output
	assert unset_run.output == 'compile:|\nruntime:<unset>|<unset>\nprivate:|<unset>|<unset>\n', unset_run.output

	mut set_environment := unset_environment.clone()
	set_environment['V_MACOS_V3_CALLER_VEXE'] = 'caller-vexe'
	set_environment['V_MACOS_V3_CALLER_VEXE_PRESENT'] = '1'
	set_environment['V_MACOS_V3_CALLER_VCHILD'] = 'caller-vchild'
	set_environment['V_MACOS_V3_CALLER_VCHILD_PRESENT'] = '1'
	set_output := os.join_path(root, 'caller_set')
	set_run := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel', '-no-memory-limit',
		'-o', set_output, 'run', source], set_environment)
	assert set_run.exit_code == 0, set_run.output
	assert set_run.output == 'compile:caller-vexe|caller-vchild\nruntime:caller-vexe|caller-vchild\nprivate:|<unset>|<unset>\n', set_run.output
}

fn test_driver_resolves_boolean_d_and_documented_pseudos() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_comptime_values_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	project := os.join_path(root, 'project')
	git_refs := os.join_path(project, '.git', 'refs', 'heads')
	os.mkdir_all(git_refs) or { panic(err) }
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'driver_comptime_values' }\n")!
	os.write_file(os.join_path(project, '.git', 'HEAD'), 'ref: refs/heads/main\n')!
	os.write_file(os.join_path(git_refs, 'main'), '0123456789abcdef0123456789abcdef01234567\n')!
	source := os.join_path(project, 'main.v')
	os.write_file(source, "module main

const enabled = \$d('feature', false)
const column = @COLUMN
const project_hash = @VMODHASH
const legacy_root_matches = @VROOT == @VEXEROOT
const column_condition = \$if @COLUMN != '' { true } \$else { false }
const hash_condition = \$if @VMODHASH == '0123456' { true } \$else { false }
const legacy_root_condition = \$if @VROOT == @VEXEROOT { true } \$else { false }

fn main() {
	println(enabled)
	println(column)
	println(project_hash)
	println(legacy_root_matches)
	println(column_condition)
	println(hash_condition)
	println(legacy_root_condition)
}
")!
	for i, define_args in [
		['-d', 'feature'],
		['-dfeature'],
	] {
		output := os.join_path(root, 'comptime_values_${i}')
		mut args := define_args.clone()
		args << ['-silent', '-no-parallel', '-o', output, source]
		compile := cmdexec.run(v3_bin, args)
		assert compile.exit_code == 0, compile.output
		run := cmdexec.run(output, [])
		assert run.exit_code == 0, run.output
		assert run.output == 'true\n16\n0123456\ntrue\ntrue\ntrue\ntrue\n', run.output
	}
	os.write_file(os.join_path(git_refs, 'main'), 'abcdef0123456789abcdef0123456789abcdef01\n')!
	updated_output := os.join_path(root, 'comptime_values_updated_hash')
	updated_compile := cmdexec.run(v3_bin, ['-d', 'feature', '-silent', '-no-parallel', '-o',
		updated_output, source])
	assert updated_compile.exit_code == 0, updated_compile.output
	updated_run := cmdexec.run(updated_output, [])
	assert updated_run.exit_code == 0, updated_run.output
	assert updated_run.output == 'true\n16\nabcdef0\ntrue\ntrue\nfalse\ntrue\n', updated_run.output

	invalid_source := os.join_path(project, 'invalid_define.v')
	os.write_file(invalid_source, "module main

const value = \$d('feature', 1)

fn main() {
	println(value)
}
")!
	assert_driver_cli_failure(v3_bin, ['-d', 'feature', '-silent', '-no-parallel', invalid_source],
		'i64 literal expected, found "true"')
}

fn test_delegated_driver_preserves_invoking_vroot() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_invoking_vroot_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	other_checkout := os.join_path(root, 'other_checkout')
	os.mkdir_all(os.join_path(other_checkout, 'vlib', 'builtin')) or { panic(err) }
	source := os.join_path(other_checkout, 'main.v')
	os.write_file(source, 'module main

fn main() {
	println(@VEXEROOT)
	println(@VROOT)
	println(\$if @VROOT == @VEXEROOT { true } \$else { false })
}
')!
	output := os.join_path(root, 'invoking_vroot')
	mut environment := os.environ()
	environment['VEXE'] = @VEXE
	environment['V_MACOS_V3_CALLER_VEXE'] = ''
	environment['V_MACOS_V3_CALLER_VEXE_PRESENT'] = '0'
	environment['V_MACOS_V3_CALLER_VCHILD'] = ''
	environment['V_MACOS_V3_CALLER_VCHILD_PRESENT'] = '0'
	compile := run_driver_with_environment(v3_bin, ['-nocache', '-silent', '-no-parallel', '-o',
		output, source], environment)
	assert compile.exit_code == 0, compile.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	invoking_root := os.real_path(os.dir(@VEXE))
	assert run.output == '${invoking_root}\n${invoking_root}\ntrue\n', run.output
}

fn test_driver_run_preserves_stdin() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_stdin_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'stdin_program.v')
	output := os.join_path(root, 'stdin_program')
	input_file := os.join_path(root, 'input.txt')
	os.write_file(source, "import os\n\nfn main() { println('read:' + os.input('')) }\n")!
	os.write_file(input_file, 'from-stdin\n')!
	result := run_driver_with_stdin_file(v3_bin, ['-o', output, 'run', source], input_file)
	assert result.exit_code == 0, result.output
	assert result.output.contains('read:from-stdin'), result.output
}

fn test_driver_accepts_dispatcher_arguments_and_runs_vsh_files() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_vsh_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3_with_flags(root, ['-prealloc'])
	program := os.join_path(root, 'dispatcher_build.v')
	binary := os.join_path(root, 'dispatcher_build')
	os.write_file(program, "println('built')")!
	build := cmdexec.run(v3_bin, ['-silent', '-no-parallel', '-cstrict', '-skip-running', '-usecache',
		'build', '-o', binary, program])
	assert build.exit_code == 0, build.output
	assert !os.exists(binary + '.c')
	built := cmdexec.run(binary, [])
	assert built.exit_code == 0, built.output
	assert built.output == 'built\n', built.output
	implicit_program := os.join_path(root, 'implicit_build.v')
	implicit_binary := os.join_path(root, 'implicit_build')
	implicit_c := implicit_binary + '.c'
	os.write_file(implicit_program, "println('implicit')")!
	os.write_file(implicit_c, 'existing C source')!
	implicit_build := cmdexec.run(v3_bin, ['-silent', '-no-parallel', implicit_program])
	assert implicit_build.exit_code == 0, implicit_build.output
	assert os.is_file(implicit_binary)
	assert os.read_file(implicit_c)! == 'existing C source'
	showcc_binary := os.join_path(root, 'showcc_build')
	showcc_build := cmdexec.run(v3_bin, ['-silent', '-showcc', '-no-parallel', '-o', showcc_binary,
		implicit_program])
	assert showcc_build.exit_code == 0, showcc_build.output
	assert showcc_build.output.contains('  > '), showcc_build.output
	assert !showcc_build.output.contains('=== v3 benchmark ==='), showcc_build.output
	assert !showcc_build.output.contains('MB RSS'), showcc_build.output
	keep_c_dir := os.join_path(root, 'kept_c')
	os.mkdir(keep_c_dir)!
	mut keep_c_environment := os.environ()
	keep_c_environment['VTMP'] = keep_c_dir
	keep_c_build := run_driver_with_environment(v3_bin, ['-silent', '-no-parallel', '-keepc',
		implicit_program], keep_c_environment)
	assert keep_c_build.exit_code == 0, keep_c_build.output
	assert os.read_file(implicit_c)! == 'existing C source'
	kept_files := kept_c_files(keep_c_dir)
	assert kept_files.len == 1, kept_files.str()
	assert os.file_size(kept_files[0]) > 0
	assert os.file_name(kept_files[0]).starts_with('implicit_build.')
	run_program := os.join_path(root, 'implicit_run.v')
	run_binary := run_program.all_before_last('.v')
	os.write_file(run_program, "println('ran once')")!
	implicit_run := cmdexec.run(v3_bin, ['-silent', '-no-parallel', 'run', run_program])
	assert implicit_run.exit_code == 0, implicit_run.output
	assert implicit_run.output == 'ran once\n', implicit_run.output
	assert !os.exists(run_binary)
	assert !os.exists(run_binary + '.c')
	$if !windows {
		interrupted_program := os.join_path(root, 'interrupted_run.v')
		interrupted_binary := interrupted_program.all_before_last('.v')
		interrupted_marker := os.join_path(root, 'interrupted_run_started')
		os.write_file(interrupted_program, "import os

fn main() {
	os.write_file('${interrupted_marker}', 'started') or { exit(2) }
	for {}
}
")!
		mut interrupted_run := os.new_process(v3_bin)
		interrupted_run.use_pgroup = true
		interrupted_run.set_args(['-silent', '-no-parallel', 'run', interrupted_program])
		interrupted_run.run()
		mut interrupted_started := false
		for _ in 0 .. 600 {
			if os.exists(interrupted_marker) {
				interrupted_started = true
				break
			}
			if !interrupted_run.is_alive() {
				break
			}
			time.sleep(100 * time.millisecond)
		}
		if !interrupted_started {
			if interrupted_run.is_alive() {
				interrupted_run.signal_pgkill()
			}
			interrupted_run.wait()
			run_error := interrupted_run.err
			interrupted_run.close()
			assert false, 'interrupted run did not start: ${run_error}'
		}
		interrupt := os.execute('kill -INT -${interrupted_run.pid}')
		if interrupt.exit_code != 0 {
			interrupted_run.signal_pgkill()
			interrupted_run.wait()
			interrupted_run.close()
			assert false, interrupt.output
		}
		interrupted_run.wait()
		interrupted_exit_code := interrupted_run.code
		interrupted_run.close()
		assert interrupted_exit_code != 0
		assert !os.exists(interrupted_binary)
	}
	debug_define_program := os.join_path(root, 'debug_define.v')
	os.write_file(debug_define_program, '@[if debug]
fn enabled_by_define() {
	println("attribute debug")
}

fn main() {
	$if debug {
		println("comptime debug")
	}
	enabled_by_define()
}
')!
	debug_define_run := cmdexec.run(v3_bin, ['-silent', '-no-parallel', '-d', 'debug', 'run',
		debug_define_program])
	assert debug_define_run.exit_code == 0, debug_define_run.output
	assert debug_define_run.output == 'attribute debug\n', debug_define_run.output
	source := os.join_path(root, 'implicit_script.vsh')
	os.write_file(source, "import os

println(os.executable().ends_with('.vsh'))
println(os.args[1..].join('|'))
")!
	run := cmdexec.run(v3_bin, ['-silent', '-no-parallel', source, 'one', '--two'])
	assert run.exit_code == 0, run.output
	assert run.output == 'false\none|--two\n', run.output
	assert !os.exists(source.all_before_last('.vsh'))
	assert !os.exists(source.all_before_last('.vsh') + '.c')
	compile_only_source := os.join_path(root, 'compile_only.vsh')
	compile_only_binary := compile_only_source.all_before_last('.vsh')
	run_marker := os.join_path(root, 'compile_only_ran')
	os.write_file(compile_only_source, "import os

os.write_file('${run_marker}', 'ran')!
")!
	skip_run := cmdexec.run(v3_bin,
		['-silent', '-no-parallel', '-skip-running', compile_only_source])
	assert skip_run.exit_code == 0, skip_run.output
	assert os.is_file(compile_only_binary)
	assert !os.exists(run_marker)
	os.rm(compile_only_binary)!
	build_script := cmdexec.run(v3_bin, ['-silent', '-no-parallel', 'build', compile_only_source])
	assert build_script.exit_code == 0, build_script.output
	assert os.is_file(compile_only_binary)
	assert !os.exists(run_marker)
}

fn assert_driver_wasm_output(path string) {
	bytes := os.read_bytes(path) or { panic(err) }
	assert bytes.len > 8
	assert bytes[..4] == [u8(0), 0x61, 0x73, 0x6d]
}

fn test_driver_platform_pseudo_uses_selected_target_arch() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_platform_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'platform.v')
	os.write_file(source, "fn main() {
	println(@PLATFORM)
	\$if @PLATFORM == 'amd64' {
		println('selected-amd64')
	} \$else \$if @PLATFORM == 'arm64' {
		println('selected-arm64')
	}
}
")!
	for target_arch, platform in {
		'x86_64': 'amd64'
		'arm64':  'arm64'
	} {
		output := os.join_path(root, 'platform_${platform}.c')
		compile := cmdexec.run(v3_bin, ['-nocache', '-no-parallel', '-os', 'macos', '-arch',
			target_arch, '-o', output, source])
		assert compile.exit_code == 0, compile.output
		generated_c := os.read_file(output)!
		assert generated_c.contains('{"${platform}", ${platform.len}'), generated_c
		assert generated_c.contains('selected-${platform}'), generated_c
		other_platform := if platform == 'amd64' { 'arm64' } else { 'amd64' }
		assert !generated_c.contains('selected-${other_platform}'), generated_c
	}
}

fn test_wasm_backend_defaults_target_unless_explicit() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_wasm_target_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)

	default_dir := os.join_path(root, 'default_target')
	os.mkdir_all(default_dir) or { panic(err) }
	os.write_file(os.join_path(default_dir, 'main.v'), 'module main

\$if wasm32 {
fn wasm_arch_selected() {}
}

fn main() {
	wasm_os_selected()
	wasm_arch_selected()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(default_dir, 'target_wasm32_emscripten.v'),
		'module main\n\nfn wasm_os_selected() {}\n') or { panic(err) }
	default_output := os.join_path(root, 'default_target.wasm')
	default_compile := cmdexec.run(v3_bin, ['-b', 'wasm', '-o', default_output, default_dir])
	assert default_compile.exit_code == 0, default_compile.output
	assert_driver_wasm_output(default_output)

	host := pref.host_target()
	explicit_dir := os.join_path(root, 'explicit_target')
	os.mkdir_all(explicit_dir) or { panic(err) }
	os.write_file(os.join_path(explicit_dir, 'main.v'),
		'module main\n\nfn main() { host_os_selected() }\n') or { panic(err) }
	os.write_file(os.join_path(explicit_dir, 'target_${host.os}.v'),
		'module main\n\nfn host_os_selected() {}\n') or { panic(err) }
	explicit_output := os.join_path(root, 'explicit_target.wasm')
	explicit_compile := cmdexec.run(v3_bin, ['-b', 'wasm', '-os', host.os, '-arch', host.arch,
		'-o', explicit_output, explicit_dir])
	assert explicit_compile.exit_code == 0, explicit_compile.output
	assert_driver_wasm_output(explicit_output)
}

fn test_driver_rejects_invalid_cli_and_parses_vmod_subdirs() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_cli_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'hello.v')
	os.write_file(source, "fn main() { println('ok') }\n") or { panic(err) }

	help := cmdexec.run(v3_bin, ['--help'])
	assert help.exit_code == 0
	assert help.output.contains('-cc <compiler>')
	assert help.output.contains('-no-memory-limit')
	c_output := os.join_path(root, 'hello.c')
	c_compile := cmdexec.run(v3_bin, ['-no-memory-limit', '-o', c_output, source])
	assert c_compile.exit_code == 0, c_compile.output
	assert !c_compile.output.contains('[ttime]'), c_compile.output
	$if macos {
		rss_index := c_compile.output.index('MB RSS') or { -1 }
		footprint_index := c_compile.output.index('MB physical footprint') or { -1 }
		assert rss_index >= 0, c_compile.output
		assert footprint_index > rss_index, c_compile.output
	}
	c_source := os.read_file(c_output)!
	assert c_source.len > 100
	assert c_source.contains('typedef signed char i8;')
	verbose_output := os.join_path(root, 'hello_verbose.c')
	verbose_compile := cmdexec.run(v3_bin, ['-nocache', '-v', '-o', verbose_output, source])
	assert verbose_compile.exit_code == 0, verbose_compile.output
	assert verbose_compile.output.contains('[ttime]'), verbose_compile.output
	compat_output := os.join_path(root, 'hello_compat')
	kept_before := kept_c_files(os.vtmp_dir())
	compat_compile := cmdexec.run(v3_bin, ['-stats', '-show-timings', '-showcc', '-keepc', '-w',
		'-g', '-cflags', '-w', '-enable-globals', '-o', compat_output, source])
	assert compat_compile.exit_code == 0, compat_compile.output
	assert os.is_file(compat_output)
	assert !os.exists(compat_output + '.c')
	new_kept_files := kept_c_files(os.vtmp_dir()).filter(it !in kept_before
		&& os.file_name(it).starts_with('hello_compat.'))
	assert new_kept_files.len == 1, new_kept_files.str()
	os.rm(new_kept_files[0])!
	debug_source := os.join_path(root, 'debug_comptime.v')
	os.write_file(debug_source,
		"fn main() {\n\t\$if debug {\n\t\tprintln('debug')\n\t} \$else {\n\t\tprintln('release')\n\t}\n}\n") or {
		panic(err)
	}
	release_output := os.join_path(root, 'debug_comptime_release')
	release_compile := cmdexec.run(v3_bin, ['-o', release_output, debug_source])
	assert release_compile.exit_code == 0, release_compile.output
	release_run := cmdexec.run(release_output, [])
	assert release_run.exit_code == 0, release_run.output
	assert release_run.output.trim_space() == 'release'
	debug_output := os.join_path(root, 'debug_comptime_debug')
	debug_compile := cmdexec.run(v3_bin, ['-g', '-o', debug_output, debug_source])
	assert debug_compile.exit_code == 0, debug_compile.output
	debug_run := cmdexec.run(debug_output, [])
	assert debug_run.exit_code == 0, debug_run.output
	assert debug_run.output.trim_space() == 'debug'
	wasm_c_output := os.join_path(root, 'hello_emscripten.c')
	wasm_compile := cmdexec.run(v3_bin, ['-os', 'wasm32_emscripten', '-o', wasm_c_output, source])
	assert wasm_compile.exit_code == 0, wasm_compile.output
	assert os.is_file(wasm_c_output)
	assert_driver_cli_failure(v3_bin, ['-os', 'wasm32_emscripten', '-arch', 'arm64', '-o',
		wasm_c_output, source], 'target OS `wasm32_emscripten` requires architecture `wasm32`')
	bits_source := os.join_path(root, 'bits_fallback.v')
	os.write_file(bits_source, 'import math.bits

fn main() {
	hi, lo := bits.mul_64(u64(0xffffffffffffffff), u64(2))
	println(hi.str() + ":" + lo.str())
}
') or {
		panic(err)
	}
	bits_output := os.join_path(root, 'bits_fallback')
	bits_compile := cmdexec.run(v3_bin, ['-prod', '-o', bits_output, bits_source])
	assert bits_compile.exit_code == 0, bits_compile.output
	bits_run := cmdexec.run(bits_output, [])
	assert bits_run.exit_code == 0, bits_run.output
	assert bits_run.output.trim_space() == '1:18446744073709551614'
	assert_driver_cli_failure(v3_bin, ['--bogus'], 'unknown option `--bogus`')
	assert_driver_cli_failure(v3_bin, ['-o'], 'option `-o` requires a value')
	assert_driver_cli_failure(v3_bin, ['-b', 'bogus', source], 'unknown backend `bogus`')
	assert_driver_cli_failure(v3_bin, ['-gc', 'boehm', source],
		'currently supports only `-gc none`')
	assert_driver_cli_failure(v3_bin, ['-d', 'gcboehm', source],
		'v3 programs must not use a garbage collector')
	assert_driver_cli_failure(v3_bin, ['-dvgc', source],
		'v3 programs must not use a garbage collector')
	assert_driver_cli_failure(v3_bin, [source, source], 'multiple input paths are not supported')
	assert_driver_cli_failure(v3_bin, ['-compile-backend', 'bogus', source],
		'unknown compile backend `bogus`')

	if false_exe := os.find_abs_path_of_executable('false') {
		cc_result := cmdexec.run(v3_bin, ['-prod', '-cc', false_exe, source, '-o',
			os.join_path(root, 'false_cc')])
		assert cc_result.exit_code != 0
		assert cc_result.output.contains(cmdexec.display(false_exe, ['-std=gnu11'])), cc_result.output
	}

	work_dir := os.join_path(root, 'work')
	project := os.join_path(root, 'project.with.dots')
	os.mkdir_all(os.join_path(project, 'one')) or { panic(err) }
	os.mkdir_all(os.join_path(project, 'two')) or { panic(err) }
	os.mkdir_all(work_dir) or { panic(err) }
	os.write_file(os.join_path(project, 'v.mod'), 'Module {\n' + "  name: 'driver_cli'\n" +
		"  description: 'subdirs: [wrong, value]'\n" + "  subdirs: ['one', 'two']\n" + '}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(project, 'main.v'), 'module main

import collision

struct App {
	value int
	other string
}

fn main() {
	app := App{value: one()}
	println(app.value + collision.value())
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(project, 'one', 'one.v'),
		'module main\n\nfn one() int { return 40 }\n') or { panic(err) }
	os.write_file(os.join_path(project, 'two', 'two.v'),
		'module main\n\nfn two() int { return 2 }\n') or { panic(err) }
	collision_dir := os.join_path(project, 'collision')
	os.mkdir_all(collision_dir) or { panic(err) }
	os.write_file(os.join_path(collision_dir, 'collision.v'), 'module collision

pub struct App {
pub:
	value int
}

pub fn value() int {
	return App{value: 2}.value
}
') or {
		panic(err)
	}

	compile := cmdexec.run_in(v3_bin, [project], work_dir)
	assert compile.exit_code == 0, compile.output
	output := os.join_path(work_dir, 'project.with.dots')
	assert os.exists(output)
	assert !os.exists(os.join_path(work_dir, 'project'))
	run := cmdexec.run(output, [])
	assert run.exit_code == 0
	assert run.output.trim_space() == '42'
}
