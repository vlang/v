module builder

import os

const target_linux_marker = '<target_marker_linux.h>'
const target_macos_marker = '<target_marker_macos.h>'
const target_windows_marker = '<target_marker_windows.h>'
const target_cross_marker = '<target_marker_cross.h>'
const target_freestanding_marker = '<target_marker_freestanding.h>'
const inactive_windows_marker = '<inactive_windows_marker.h>'
const inactive_freestanding_marker = '<inactive_freestanding_marker.h>'
const active_comptime_marker = '<active_comptime_marker.h>'

struct CleancCliResult {
	exit_code int
	output    string
	c_source  string
	out_path  string
	c_path    string
}

fn e2e_repo_root() string {
	mut dir := os.dir(@FILE)
	for _ in 0 .. 10 {
		if os.exists(os.join_path(dir, 'cmd', 'v2', 'v2.v'))
			&& os.exists(os.join_path(dir, 'vlib', 'builtin')) {
			return dir
		}
		dir = os.dir(dir)
	}
	panic('could not locate repo root for cleanc target e2e')
}

fn normalize_e2e_os_name(target_os string) string {
	return match target_os.to_lower() {
		'darwin', 'mac' { 'macos' }
		else { target_os.to_lower() }
	}
}

fn e2e_v2_binary_name() string {
	mut name := 'cleanc_target_e2e'
	$if windows {
		name += '.exe'
	}
	return name
}

fn build_v2_for_target_e2e(tmp_dir string) string {
	vroot := e2e_repo_root()
	v2_source := os.join_path(vroot, 'cmd', 'v2', 'v2.v')
	v2_binary := os.join_path(tmp_dir, e2e_v2_binary_name())
	res :=
		os.execute('"${@VEXE}" -path "${os.join_path(vroot, 'vlib')}|@vlib|@vmodules" -gc none "${v2_source}" -o "${v2_binary}"')
	if res.exit_code != 0 {
		panic('failed to build cmd/v2 for cleanc target e2e:\n${res.output}')
	}
	return v2_binary
}

fn run_v2_to_c(v2_binary string, tmp_dir string, name string, args []string, source string) CleancCliResult {
	source_path := os.join_path(tmp_dir, '${name}.v')
	out_path := os.join_path(tmp_dir, '${name}.c')
	os.write_file(source_path, source) or { panic(err) }
	cmd := 'cd "${e2e_repo_root()}" && "${v2_binary}" -gc none -nocache --no-parallel ${args.join(' ')} -o "${out_path}" "${source_path}"'
	res := os.execute(cmd)
	c_source := if os.exists(out_path) { os.read_file(out_path) or { '' } } else { '' }
	return CleancCliResult{
		exit_code: res.exit_code
		output:    res.output
		c_source:  c_source
		out_path:  out_path
		c_path:    out_path
	}
}

fn generated_c_output_path(output_path string) string {
	if output_path.ends_with('.c') {
		return output_path
	}
	return output_path + '.c'
}

fn e2e_binary_output_path(tmp_dir string, name string) string {
	mut path := os.join_path(tmp_dir, name)
	$if windows {
		path += '.exe'
	}
	return path
}

fn run_v2_to_binary(v2_binary string, tmp_dir string, name string, args []string, source string) CleancCliResult {
	source_path := os.join_path(tmp_dir, '${name}.v')
	out_path := e2e_binary_output_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	env_prefix := if host_c_e2e_flags().len > 0 { 'V2CFLAGS="${host_c_e2e_flags()}" ' } else { '' }
	cmd := 'cd "${e2e_repo_root()}" && ${env_prefix}"${v2_binary}" -gc none -nocache --no-parallel ${args.join(' ')} -o "${out_path}" "${source_path}"'
	res := os.execute(cmd)
	return CleancCliResult{
		exit_code: res.exit_code
		output:    res.output
		out_path:  out_path
		c_path:    generated_c_output_path(out_path)
	}
}

fn run_v2_to_output(v2_binary string, tmp_dir string, name string, args []string, source string, output_path string) CleancCliResult {
	source_path := os.join_path(tmp_dir, '${name}.v')
	os.write_file(source_path, source) or { panic(err) }
	cmd := 'cd "${e2e_repo_root()}" && "${v2_binary}" -gc none -nocache --no-parallel ${args.join(' ')} -o "${output_path}" "${source_path}"'
	res := os.execute(cmd)
	c_path := generated_c_output_path(output_path)
	c_source := if os.exists(c_path) { os.read_file(c_path) or { '' } } else { '' }
	return CleancCliResult{
		exit_code: res.exit_code
		output:    res.output
		c_source:  c_source
		out_path:  output_path
		c_path:    c_path
	}
}

fn run_v2_to_output_with_cache(v2_binary string, tmp_dir string, name string, args []string, source string, output_path string) CleancCliResult {
	source_path := os.join_path(tmp_dir, '${name}.v')
	os.write_file(source_path, source) or { panic(err) }
	cmd := 'cd "${e2e_repo_root()}" && "${v2_binary}" -gc none --no-parallel ${args.join(' ')} -o "${output_path}" "${source_path}"'
	res := os.execute(cmd)
	c_path := generated_c_output_path(output_path)
	c_source := if os.exists(c_path) { os.read_file(c_path) or { '' } } else { '' }
	return CleancCliResult{
		exit_code: res.exit_code
		output:    res.output
		c_source:  c_source
		out_path:  output_path
		c_path:    c_path
	}
}

fn assert_cli_success(res CleancCliResult) {
	assert res.exit_code == 0, res.output
	assert os.exists(res.out_path), res.output
	assert res.c_source.len > 0, res.output
}

fn assert_binary_success(res CleancCliResult) {
	assert res.exit_code == 0, res.output
	assert os.exists(res.out_path), res.output
}

fn assert_generated_c_only(res CleancCliResult) {
	assert res.exit_code == 0, res.output
	assert os.exists(res.c_path), res.output
	assert res.c_source.len > 0, res.output
	assert !os.exists(res.out_path), 'unexpected local executable ${res.out_path}\n${res.output}'
	assert res.output.contains('local C compilation disabled for this target'), res.output
}

fn assert_cli_failure_contains(res CleancCliResult, expected string) {
	assert res.exit_code != 0, res.output
	assert res.output.contains(expected), res.output
}

fn host_cc_available() bool {
	$if windows {
		return false
	}
	return os.execute('cc --version').exit_code == 0
}

fn host_c_e2e_flags() string {
	return '-D_GNU_SOURCE -Wno-error=incompatible-pointer-types -Wno-error=implicit-function-declaration'
}

fn concrete_non_host_e2e_os() string {
	host_os := normalize_e2e_os_name(os.user_os())
	return match host_os {
		'windows' { 'linux' }
		else { 'windows' }
	}
}

fn target_fixture_source(name string) string {
	path := os.join_path(e2e_repo_root(), 'vlib', 'v2', 'tests', 'target_codegen_example', name)
	return os.read_file(path) or { panic('cannot read ${path}: ${err}') }
}

fn target_directive_source() string {
	return target_fixture_source('target_directives.vv2')
}

fn cross_directive_source() string {
	return target_fixture_source('cross_directives.vv2')
}

fn freestanding_directive_source() string {
	return target_fixture_source('freestanding_directives.vv2')
}

fn freestanding_none_source() string {
	return target_fixture_source('freestanding_none.vv2')
}

fn assert_concrete_target_markers(c_source string, target string) {
	assert c_source.contains(match target {
		'linux' { target_linux_marker }
		'macos' { target_macos_marker }
		'windows' { target_windows_marker }
		else { '' }
	})
	if target != 'linux' {
		assert !c_source.contains(target_linux_marker)
	}
	if target != 'macos' {
		assert !c_source.contains(target_macos_marker)
	}
	if target != 'windows' {
		assert !c_source.contains(target_windows_marker)
	}
	assert !c_source.contains(target_cross_marker)
	assert !c_source.contains(target_freestanding_marker)
}

fn assert_no_os_runtime_headers(c_source string) {
	for header in ['#include <unistd.h>', '#include <pthread.h>', '#include <dirent.h>',
		'#include <windows.h>', '#include <mach/mach.h>', '#include <termios.h>',
		'#include <sys/wait.h>'] {
		assert !c_source.contains(header)
	}
}

fn test_cleanc_cli_generated_c_target_matrix() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cleanc_target_e2e_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	v2_binary := build_v2_for_target_e2e(tmp_dir)
	source := target_directive_source()

	default_res := run_v2_to_c(v2_binary, tmp_dir, 'default_host', [], source)
	assert_cli_success(default_res)
	assert !default_res.c_source.contains(target_cross_marker)
	assert !default_res.c_source.contains(target_freestanding_marker)
	host_os := normalize_e2e_os_name(os.user_os())
	if host_os in ['linux', 'macos', 'windows'] {
		assert_concrete_target_markers(default_res.c_source, host_os)
	}

	for target in ['linux', 'macos', 'windows'] {
		res := run_v2_to_c(v2_binary, tmp_dir, 'target_${target}', ['-os', target], source)
		assert_cli_success(res)
		assert_concrete_target_markers(res.c_source, target)
	}

	cross_res := run_v2_to_c(v2_binary, tmp_dir, 'target_cross', ['-os', 'cross'],
		cross_directive_source())
	assert_cli_success(cross_res)
	assert cross_res.c_source.contains(target_linux_marker)
	assert cross_res.c_source.contains(target_macos_marker)
	assert cross_res.c_source.contains(target_windows_marker)
	assert cross_res.c_source.contains(target_cross_marker)
	assert cross_res.c_source.contains('defined(__linux__)')
	assert cross_res.c_source.contains('defined(_WIN32)')
	assert cross_res.c_source.contains('defined(__APPLE__)')

	free_res := run_v2_to_c(v2_binary, tmp_dir, 'target_freestanding', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], freestanding_directive_source())
	assert_cli_success(free_res)
	assert free_res.c_source.contains(target_linux_marker)
	assert free_res.c_source.contains(target_freestanding_marker)
	assert !free_res.c_source.contains(target_windows_marker)
	assert_no_os_runtime_headers(free_res.c_source)
}

fn test_cleanc_cli_comptime_if_directives_follow_active_branch() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cleanc_comptime_directives_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	v2_binary := build_v2_for_target_e2e(tmp_dir)

	linux_res := run_v2_to_c(v2_binary, tmp_dir, 'linux_inactive_windows_directive', [
		'-os',
		'linux',
	], 'module main

\$if windows {
	#include <inactive_windows_marker.h>
}

#include <active_comptime_marker.h>

fn main() {}
')
	assert_cli_success(linux_res)
	assert !linux_res.c_source.contains(inactive_windows_marker)
	assert linux_res.c_source.contains(active_comptime_marker)

	freestanding_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_inactive_directive', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

\$if !freestanding {
	#include <inactive_freestanding_marker.h>
}

#include freestanding <active_comptime_marker.h>

fn main() {}
')
	assert_cli_success(freestanding_res)
	assert !freestanding_res.c_source.contains(inactive_freestanding_marker)
	assert freestanding_res.c_source.contains(active_comptime_marker)
}

fn test_cleanc_cli_freestanding_diagnostics_and_user_directives() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cleanc_target_diag_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	v2_binary := build_v2_for_target_e2e(tmp_dir)

	help_res := os.execute('"${v2_binary}" --definitely-unknown-freestanding-flag')
	assert help_res.exit_code != 0, help_res.output
	assert help_res.output.contains('-fhooks <values>'), help_res.output
	assert help_res.output.contains('Advanced freestanding hooks for --skip-builtin --skip-type-check stubs'), help_res.output
	assert help_res.output.contains('-b <name>'), help_res.output
	assert help_res.output.contains('omit for cleanc'), help_res.output

	assert help_res.output.contains('Override target OS (default: host OS)'), help_res.output

	fixture_empty := target_fixture_source('freestanding_empty.vv2')
	fixture_output := target_fixture_source('freestanding_output.vv2')
	fixture_panic := target_fixture_source('freestanding_panic.vv2')
	fixture_alloc := target_fixture_source('freestanding_alloc.vv2')

	none_without_freestanding_res := run_v2_to_c(v2_binary, tmp_dir, 'none_without_freestanding', [
		'-os',
		'none',
	], fixture_empty)
	assert_cli_failure_contains(none_without_freestanding_res, '-os none requires -freestanding')

	freestanding_cross_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_cross', [
		'-freestanding',
		'-os',
		'cross',
		'--skip-builtin',
	], fixture_empty)
	assert_cli_failure_contains(freestanding_cross_res, '-freestanding -os cross is not supported')

	os_import_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_os_import', [
		'-freestanding',
		'-os',
		'linux',
	], 'module main

import os

fn main() {
	_ := os.args.len
}
	')
	assert_cli_failure_contains(os_import_res, 'freestanding target cannot use module os')

	for import_name in ['time', 'term', 'net', 'net.http', 'sync'] {
		import_res := run_v2_to_c(v2_binary, tmp_dir,
			'freestanding_${import_name.replace('.', '_')}_import', [
			'-freestanding',
			'-os',
			'linux',
			'--skip-builtin',
		], 'module main

import ${import_name}

fn main() {}
')
		assert_cli_failure_contains(import_res,
			'freestanding target cannot use module ${import_name.all_before('.')}')
	}

	print_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_print', [
		'-freestanding',
		'-os',
		'linux',
	], fixture_output)
	assert_cli_failure_contains(print_res, 'freestanding target cannot use builtin println')

	minimal_runtime_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_minimal_runtime', [
		'-freestanding',
		'-os',
		'linux',
	], fixture_empty)
	assert_cli_failure_contains(minimal_runtime_res,
		'freestanding target currently needs explicit platform runtime hooks')

	output_hook_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_output_hook', [
		'-freestanding',
		'-fhooks',
		'output',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], fixture_output)
	assert_cli_success(output_hook_res)
	assert output_hook_res.c_source.contains('isize v_platform_write(int stream, const u8* buf, isize len);')
	assert output_hook_res.c_source.contains('v_platform_write(fd, ptr, remaining_bytes)')
	assert !output_hook_res.c_source.contains('v_platform_panic'), output_hook_res.c_source
	assert !output_hook_res.c_source.contains('v_platform_malloc'), output_hook_res.c_source
	assert !output_hook_res.c_source.contains('v_platform_realloc'), output_hook_res.c_source
	assert !output_hook_res.c_source.contains('v_platform_free'), output_hook_res.c_source

	output_hook_builtin_res := run_v2_to_c(v2_binary, tmp_dir,
		'freestanding_output_hook_builtin_runtime', [
		'-freestanding',
		'-fhooks',
		'output',
		'-os',
		'linux',
	], fixture_output)
	assert_cli_failure_contains(output_hook_builtin_res,
		'freestanding target platform hooks currently require --skip-builtin and --skip-type-check')
	assert !output_hook_builtin_res.output.contains('__malloc'), output_hook_builtin_res.output

	output_hook_skip_builtin_typecheck_res := run_v2_to_c(v2_binary, tmp_dir,
		'freestanding_output_hook_skip_builtin_typecheck_gate', [
		'-freestanding',
		'-fhooks',
		'output',
		'-os',
		'linux',
		'--skip-builtin',
	], fixture_output)
	assert_cli_failure_contains(output_hook_skip_builtin_typecheck_res,
		'freestanding target platform hooks currently require --skip-builtin and --skip-type-check')
	assert !output_hook_skip_builtin_typecheck_res.output.contains('unknown ident'), output_hook_skip_builtin_typecheck_res.output

	for hook_name in ['output', 'panic', 'alloc', 'minimal'] {
		hook_runtime_res := run_v2_to_c(v2_binary, tmp_dir,
			'freestanding_${hook_name}_builtin_runtime_gate', [
			'-freestanding',
			'-fhooks',
			hook_name,
			'-os',
			'linux',
		], fixture_empty)
		assert_cli_failure_contains(hook_runtime_res,
			'freestanding target platform hooks currently require --skip-builtin and --skip-type-check')
		assert !hook_runtime_res.output.contains('__malloc'), hook_runtime_res.output
	}

	output_missing_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_output_missing', [
		'-freestanding',
		'-fhooks',
		'panic',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], fixture_output)
	assert_cli_failure_contains(output_missing_res,
		'freestanding target cannot use builtin println without output platform hook')

	spoofed_output_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_spoofed_output_define', [
		'-freestanding',
		'-d',
		'freestanding_output',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], fixture_output)
	assert_cli_failure_contains(spoofed_output_res,
		'freestanding target cannot use builtin println without output platform hook')

	for helper_call in ['_write_buf_to_fd(1, 0, 0)', "_writeln_to_fd(1, '')", 'flush_stdout()',
		'flush_stderr()'] {
		helper_res := run_v2_to_c(v2_binary, tmp_dir,
			'freestanding_output_helper_${helper_call.all_before('(')}', [
			'-freestanding',
			'-os',
			'linux',
			'--skip-builtin',
			'--skip-type-check',
		], 'module main

fn main() {
	${helper_call}
}
')
		assert_cli_failure_contains(helper_res,
			'freestanding target cannot use output runtime helpers without output platform hook')
	}

	for print_call in ['println(123)', 'print(true)'] {
		print_conversion_res := run_v2_to_c(v2_binary, tmp_dir,
			'freestanding_output_only_${print_call.all_before('(')}_conversion', [
			'-freestanding',
			'-fhooks',
			'output',
			'-os',
			'linux',
			'--skip-builtin',
			'--skip-type-check',
		], 'module main

fn main() {
	${print_call}
}
')
		assert_cli_failure_contains(print_conversion_res,
			'freestanding target cannot print non-string values with output hook only')
	}

	panic_hook_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_panic_hook', [
		'-freestanding',
		'-fhooks',
		'panic',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], fixture_panic)
	assert_cli_success(panic_hook_res)
	assert panic_hook_res.c_source.contains('void v_platform_panic(const u8* msg, isize len);')
	assert !panic_hook_res.c_source.contains('v_platform_write'), panic_hook_res.c_source
	assert !panic_hook_res.c_source.contains('v_platform_malloc'), panic_hook_res.c_source

	panic_missing_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_panic_missing', [
		'-freestanding',
		'-fhooks',
		'output',
		'-os',
		'linux',
	], fixture_panic)
	assert_cli_failure_contains(panic_missing_res,
		'freestanding target cannot use builtin panic without panic platform hook')

	spoofed_panic_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_spoofed_panic_define', [
		'-freestanding',
		'-d',
		'freestanding_panic',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], fixture_panic)
	assert_cli_failure_contains(spoofed_panic_res,
		'freestanding target cannot use builtin panic without panic platform hook')

	alloc_missing_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_alloc_missing', [
		'-freestanding',
		'-fhooks',
		'output',
		'-os',
		'linux',
	], fixture_alloc)
	assert_cli_failure_contains(alloc_missing_res,
		'freestanding target cannot use heap allocation without alloc platform hook')

	spoofed_alloc_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_spoofed_alloc_define', [
		'-freestanding',
		'-d',
		'freestanding_alloc',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], fixture_alloc)
	assert_cli_failure_contains(spoofed_alloc_res,
		'freestanding target cannot use heap allocation without alloc platform hook')

	arguments_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_arguments', [
		'-freestanding',
		'-fhooks',
		'alloc',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], 'module main

fn main() {
	_ := arguments()
}
')
	assert_cli_failure_contains(arguments_res,
		'freestanding target cannot use arguments() because command-line arguments need hosted runtime support')

	alloc_hook_string_interpolation_res := run_v2_to_c(v2_binary, tmp_dir,
		'freestanding_alloc_string_interpolation', [
		'-freestanding',
		'-fhooks',
		'alloc',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], "module main

fn main() {
	_ := '\${1}'
}
	")
	assert_cli_failure_contains(alloc_hook_string_interpolation_res,
		'freestanding target cannot use string interpolation because cleanc currently needs hosted formatting support')

	alloc_hook_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_alloc_hook', [
		'-freestanding',
		'-fhooks',
		'alloc',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], fixture_alloc)
	assert_cli_success(alloc_hook_res)
	assert alloc_hook_res.c_source.contains('void* v_platform_malloc(isize n);')
	assert alloc_hook_res.c_source.contains('void* v_platform_realloc(void* ptr, isize n);')
	assert alloc_hook_res.c_source.contains('void v_platform_free(void* ptr);')
	assert !alloc_hook_res.c_source.contains('v_platform_write'), alloc_hook_res.c_source
	assert !alloc_hook_res.c_source.contains('v_platform_panic'), alloc_hook_res.c_source

	prealloc_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_prealloc', [
		'-freestanding',
		'-prealloc',
		'-os',
		'linux',
		'--skip-builtin',
	], fixture_empty)
	assert_cli_failure_contains(prealloc_res, 'freestanding target cannot use -prealloc')

	shared_lib_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_shared_lib', [
		'-freestanding',
		'-shared',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

fn main() {}
')
	assert_cli_failure_contains(shared_lib_res, 'freestanding target cannot use -shared')

	hot_fn_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_hot_fn', [
		'-freestanding',
		'-hot-fn',
		'main',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

fn main() {}
')
	assert_cli_failure_contains(hot_fn_res, 'freestanding target cannot use -hot-fn')

	for call_name in ['eprint', 'eprintln', 'panic'] {
		call_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_${call_name}', [
			'-freestanding',
			'-os',
			'linux',
			'--skip-builtin',
		], "module main

fn main() {
	${call_name}('x')
}
")
		assert_cli_failure_contains(call_res, 'freestanding target cannot use builtin ${call_name}')
	}

	spawn_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_spawn', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

fn work() {}

fn main() {
	spawn work()
}
')
	assert_cli_failure_contains(spawn_res, 'freestanding target cannot use spawn')

	lock_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_lock', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

fn main() {
	lock {}
}
')
	assert_cli_failure_contains(lock_res, 'freestanding target cannot use lock/rlock')

	shared_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_shared', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

struct State {
	value shared int
}

fn main() {}
')
	assert_cli_failure_contains(shared_res, 'freestanding target cannot use shared data')

	live_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_live', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

@[live]
fn step() {}

fn main() {}
')
	assert_cli_failure_contains(live_res, 'freestanding target cannot use @[live]')

	inactive_branch_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_inactive_branches', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], "module main

fn main() {
	\$if !freestanding {
		println('inactive')
	}
	\$if windows {
		eprintln('inactive')
	}
}
	")
	assert_cli_success(inactive_branch_res)

	inactive_fn_attr_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_inactive_fn_attributes', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], "module main

	@[if !freestanding]
	fn hosted_only() {
		println('inactive')
	}

	@[if windows]
	fn windows_only() {
		eprintln('inactive')
	}

	fn main() {}
	")
	assert_cli_success(inactive_fn_attr_res)

	fixed_array_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_fixed_array_no_alloc', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], 'module main

	fn main() {
		_ := [3]int{init: 0}
		_ := [1, 2, 3]!
	}
	')
	assert_cli_success(fixed_array_res)

	inactive_import_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_inactive_import', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

\$if !freestanding {
	import os
}

fn main() {}
')
	assert_cli_success(inactive_import_res)

	active_branch_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_active_branch', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], "module main

fn main() {
	\$if freestanding {
		eprint('active')
	}
}
	")
	assert_cli_failure_contains(active_branch_res, 'freestanding target cannot use builtin eprint')

	hook_inactive_branch_res := run_v2_to_c(v2_binary, tmp_dir,
		'freestanding_hook_inactive_branch', [
		'-freestanding',
		'-fhooks',
		'output',
		'-os',
		'linux',
		'--skip-builtin',
		'--skip-type-check',
	], "module main

fn main() {
	\$if !freestanding_output {
		println('inactive')
	}
}
	")
	assert_cli_success(hook_inactive_branch_res)

	user_directive_res := run_v2_to_c(v2_binary, tmp_dir, 'freestanding_user_directives', [
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], 'module main

#include <platform_user_header.h>
#flag -DPLATFORM_USER_FLAG
fn C.platform_external() int

fn main() {
	_ := C.platform_external()
}
	')
	assert_cli_success(user_directive_res)
	assert user_directive_res.c_source.contains('#include <platform_user_header.h>')
	assert user_directive_res.c_source.contains('platform_external'), user_directive_res.c_source
}

fn test_cleanc_cli_compiles_generated_c_on_host_when_cc_available() {
	if !host_cc_available() {
		eprintln('skip: cc is not available for cleanc host compile e2e')
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cleanc_host_cc_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	v2_binary := build_v2_for_target_e2e(tmp_dir)
	minimal_source := 'module main

fn main() {}
'

	minimal_res := run_v2_to_binary(v2_binary, tmp_dir, 'host_minimal', [
		'-cc',
		'cc',
	], minimal_source)
	assert_binary_success(minimal_res)
	minimal_run := os.execute('"${minimal_res.out_path}"')
	assert minimal_run.exit_code == 0, minimal_run.output

	host_os := normalize_e2e_os_name(os.user_os())
	if host_os in ['linux', 'macos', 'windows'] {
		explicit_host_res := run_v2_to_binary(v2_binary, tmp_dir, 'host_explicit_${host_os}', [
			'-cc',
			'cc',
			'-os',
			host_os,
		], minimal_source)
		assert_binary_success(explicit_host_res)
		explicit_host_run := os.execute('"${explicit_host_res.out_path}"')
		assert explicit_host_run.exit_code == 0, explicit_host_run.output
	}

	cross_res := run_v2_to_binary(v2_binary, tmp_dir, 'host_cross', [
		'-cc',
		'cc',
		'-os',
		'cross',
	], minimal_source)
	assert_binary_success(cross_res)
	cross_run := os.execute('"${cross_res.out_path}"')
	assert cross_run.exit_code == 0, cross_run.output

	c_impl_path := os.join_path(tmp_dir, 'platform_flag_impl.c')
	os.write_file(c_impl_path, '#ifndef PLATFORM_FLAG_FROM_V2
#error PLATFORM_FLAG_FROM_V2 missing
#endif

int platform_external(void) {
	return 7;
}
') or {
		panic(err)
	}
	flag_res := run_v2_to_binary(v2_binary, tmp_dir, 'host_flag_observable', [
		'-cc',
		'cc',
	], 'module main

#flag -DPLATFORM_FLAG_FROM_V2
#flag ${c_impl_path}

fn C.exit(code int)
fn C.platform_external() int

fn main() {
	if C.platform_external() != 7 {
		C.exit(17)
	}
}
')
	assert_binary_success(flag_res)
	flag_run := os.execute('"${flag_res.out_path}"')
	assert flag_run.exit_code == 0, flag_run.output
}

fn test_cleanc_cli_writes_c_only_for_freestanding_and_concrete_non_host_targets() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cleanc_c_only_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	v2_binary := build_v2_for_target_e2e(tmp_dir)
	missing_cc := os.join_path(tmp_dir, 'cc_must_not_run')
	minimal_source := 'module main

fn main() {}
'

	freestanding_out := os.join_path(tmp_dir, 'freestanding_app')
	freestanding_res := run_v2_to_output(v2_binary, tmp_dir, 'freestanding_generation_only', [
		'-cc',
		missing_cc,
		'-freestanding',
		'-os',
		'linux',
		'--skip-builtin',
	], minimal_source, freestanding_out)
	assert_generated_c_only(freestanding_res)
	assert freestanding_res.c_path == freestanding_out + '.c'

	freestanding_none_out := os.join_path(tmp_dir, 'freestanding_none_app')
	freestanding_none_res := run_v2_to_output(v2_binary, tmp_dir,
		'freestanding_none_generation_only', [
		'-cc',
		missing_cc,
		'-freestanding',
		'-os',
		'none',
		'--skip-builtin',
	], freestanding_none_source(), freestanding_none_out)
	assert_generated_c_only(freestanding_none_res)
	assert freestanding_none_res.c_path == freestanding_none_out + '.c'
	assert freestanding_none_res.c_source.contains('#include <platform_none.h>')
	assert freestanding_none_res.c_source.contains('platform_none_tick')
	assert_no_os_runtime_headers(freestanding_none_res.c_source)

	non_host_target := concrete_non_host_e2e_os()
	non_host_out := os.join_path(tmp_dir, 'target_${non_host_target}_app')
	non_host_res := run_v2_to_output(v2_binary, tmp_dir, 'concrete_non_host_generation_only', [
		'-cc',
		missing_cc,
		'-os',
		non_host_target,
	], minimal_source, non_host_out)
	assert_generated_c_only(non_host_res)
	assert non_host_res.c_path == non_host_out + '.c'

	non_host_cached_out := os.join_path(tmp_dir, 'target_${non_host_target}_cached_app')
	non_host_cached_res := run_v2_to_output_with_cache(v2_binary, tmp_dir,
		'concrete_non_host_generation_only_cached', [
		'-cc',
		missing_cc,
		'-os',
		non_host_target,
	], 'module main

fn main() {
	println(123)
}
', non_host_cached_out)
	assert_generated_c_only(non_host_cached_res)
	assert non_host_cached_res.c_path == non_host_cached_out + '.c'
	assert non_host_cached_res.c_source.contains('void println(string s) {'), non_host_cached_res.c_source
}

fn test_cleanc_cli_does_not_auto_run_stale_test_binary_for_generation_only_target() {
	$if windows {
		eprintln('skip: stale executable auto-run guard uses a POSIX shell script')
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cleanc_no_stale_autorun_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	v2_binary := build_v2_for_target_e2e(tmp_dir)
	source_path := os.join_path(tmp_dir, 'stale_autorun_test.v')
	stale_path := os.join_path(tmp_dir, 'stale_autorun_test')
	os.write_file(source_path, 'module main

fn main() {}
') or { panic(err) }
	os.write_file(stale_path, '#!/bin/sh
echo STALE_AUTORUN_EXECUTED
exit 73
') or { panic(err) }
	os.chmod(stale_path, 0o755) or { panic(err) }

	res :=
		os.execute('cd "${tmp_dir}" && "${v2_binary}" -gc none -nocache --no-parallel -freestanding -os none --skip-builtin "${source_path}"')
	assert res.exit_code == 0, res.output
	assert !res.output.contains('STALE_AUTORUN_EXECUTED'), res.output
	assert os.exists(os.join_path(tmp_dir, 'stale_autorun_test.c')), res.output
	assert os.exists(stale_path), res.output

	native_source_path := os.join_path(tmp_dir, 'native_cross_autorun_test.v')
	native_stale_path := os.join_path(tmp_dir, 'native_cross_autorun_test')
	os.write_file(native_source_path, 'module main

fn main() {}
') or { panic(err) }
	os.write_file(native_stale_path, '#!/bin/sh
echo NATIVE_STALE_AUTORUN_EXECUTED
exit 74
') or {
		panic(err)
	}
	os.chmod(native_stale_path, 0o755) or { panic(err) }
	non_host_target := concrete_non_host_e2e_os()
	native_res :=
		os.execute('cd "${tmp_dir}" && "${v2_binary}" -gc none -nocache --no-parallel -b x64 -os ${non_host_target} --skip-builtin "${native_source_path}"')
	assert native_res.exit_code == 0, native_res.output
	assert !native_res.output.contains('NATIVE_STALE_AUTORUN_EXECUTED'), native_res.output
	assert os.exists(native_stale_path), native_res.output
}

fn test_cleanc_cli_auto_runs_cross_test_binary_when_compiled_for_host() {
	if !host_cc_available() {
		eprintln('skip: cc is not available for cleanc cross auto-run e2e')
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cleanc_cross_autorun_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	v2_binary := build_v2_for_target_e2e(tmp_dir)
	source_path := os.join_path(tmp_dir, 'cross_autorun_test.v')
	os.write_file(source_path, 'module main

fn main() {
	println("CROSS_AUTORUN_OK")
}
') or {
		panic(err)
	}
	env_prefix := if host_c_e2e_flags().len > 0 { 'V2CFLAGS="${host_c_e2e_flags()}" ' } else { '' }
	res :=
		os.execute('cd "${tmp_dir}" && ${env_prefix}"${v2_binary}" -gc none -nocache --no-parallel -cc cc -os cross "${source_path}"')
	assert res.exit_code == 0, res.output
	assert res.output.contains('CROSS_AUTORUN_OK'), res.output
	assert !os.exists(os.join_path(tmp_dir, 'cross_autorun_test')), res.output
}
