import os
import v3.flat
import v3.modulecache
import v3.types

const module_cache_tests_dir = os.dir(@FILE)
const module_cache_v3_dir = os.dir(module_cache_tests_dir)
const module_cache_vlib_dir = os.dir(module_cache_v3_dir)
const module_cache_v3_src = os.join_path(module_cache_v3_dir, 'v3.v')

fn build_module_cache_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_module_cache_test_${os.getpid()}')
	if os.is_file(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${os.quoted_path(@VEXE)} -gc none -path "${module_cache_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(module_cache_v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn write_module_cache_file(root string, relative_path string, source string) {
	path := os.join_path(root, relative_path)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn create_module_cache_vroot(root string) {
	vlib_dir := os.join_path(root, 'vlib')
	os.mkdir_all(vlib_dir) or { panic(err) }
	entries := os.ls(module_cache_vlib_dir) or { panic(err) }
	for entry in entries {
		source := os.join_path(module_cache_vlib_dir, entry)
		destination := os.join_path(vlib_dir, entry)
		if entry == 'builtin' {
			os.cp_all(source, destination, true) or { panic(err) }
		} else {
			os.symlink(source, destination) or { panic(err) }
		}
	}
	repo_dir := os.dir(module_cache_vlib_dir)
	os.symlink(os.join_path(repo_dir, 'thirdparty'), os.join_path(root, 'thirdparty')) or {
		panic(err)
	}
}

fn compile_module_cache_project(v3_bin string, cache_dir string, main_file string, output string) {
	result :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(output)} ${os.quoted_path(main_file)}')
	assert result.exit_code == 0, result.output
}

fn run_module_cache_binary(path string) string {
	result := os.execute(os.quoted_path(path))
	assert result.exit_code == 0, result.output
	return result.output.trim_space()
}

fn module_cache_object_hash(path string) u64 {
	bytes := os.read_bytes(path) or { panic(err) }
	mut hash := u64(1469598103934665603)
	for byte in bytes {
		hash = (hash ^ u64(byte)) * u64(1099511628211)
	}
	return hash
}

fn module_cache_object_hashes(cache_dir string) map[string]u64 {
	mut hashes := map[string]u64{}
	for path in os.walk_ext(cache_dir, '.o') {
		hashes[os.file_name(path)] = module_cache_object_hash(path)
	}
	return hashes
}

fn module_cache_artifact(cache_dir string, prefix string, extension string) string {
	for path in os.walk_ext(cache_dir, extension) {
		if os.file_name(path).starts_with(prefix) {
			return path
		}
	}
	return ''
}

fn changed_module_cache_objects(before map[string]u64, after map[string]u64) []string {
	mut changed := []string{}
	for name, hash in before {
		if name !in after || after[name] != hash {
			changed << name
		}
	}
	for name in after.keys() {
		if name !in before {
			changed << name
		}
	}
	changed.sort()
	return changed
}

fn test_module_cache_split_uses_marker_lines() {
	source := 'static string marker = {"/* V3CACHE_BODY_BEGIN */", 24};\n#ifdef __cplusplus\nextern "C" {\n#endif\nextern int cached_api(void *);\n#ifdef __cplusplus\n}\n#endif\n/* V3CACHE_BODY_BEGIN */\n/* V3CACHE_MODULE main */\nint main(void) { return 0; }\n/* V3CACHE_BODY_END */\n'
	split := modulecache.split_generated_c(source) or { panic(err) }
	assert split.prefix.contains('static string marker')
	assert split.modules['main'].contains('int main(void)')
	header := modulecache.declaration_header(split.prefix)
	assert header.contains('extern "C" {')
	assert header.contains('extern int cached_api(void *);')
	assert !header.contains('extern extern "C"')
}

fn test_module_cache_split_ignores_module_marker_text() {
	source := '/* V3CACHE_BODY_BEGIN */\n/* V3CACHE_MODULE main */\nstatic string marker = {"/* V3CACHE_MODULE fake */", 25};\nint main(void) { return 0; }\n/* V3CACHE_BODY_END */\n'
	split := modulecache.split_generated_c(source) or { panic(err) }
	assert split.modules.len == 1
	assert 'fake' !in split.modules
	assert split.modules['main'].contains('"/* V3CACHE_MODULE fake */"')
	assert split.modules['main'].contains('int main(void)')
}

fn test_module_cache_declaration_header_preserves_preprocessor_after_comment() {
	prefix := '/* embedded header */\n#ifndef CACHED_HEADER\n#define CACHED_HEADER\ntypedef struct Cached { int value; } Cached;\n#endif\n'
	header := modulecache.declaration_header(prefix)
	assert header.contains('/* embedded header */\n#ifndef CACHED_HEADER')
	assert !header.contains('extern #ifndef')
}

fn test_module_cache_string_symbol_rewrite_preserves_literal_bytes() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_string_literal_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', "module main

fn main() {
	println('_str_0')
}
")
	cache_dir := os.join_path(root, 'cache')
	output := os.join_path(root, 'out')
	compile_module_cache_project(v3_bin, cache_dir, main_file, output)
	assert run_module_cache_binary(output) == '_str_0'
}

fn test_module_cache_rebuilds_objects_when_c_flags_change() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_c_flags_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

#insert "@DIR/flag_value.h"

fn C.cached_flag_value() int

pub fn value() int {
	return C.cached_flag_value()
}
')
	write_module_cache_file(root, 'wrapper/flag_value.h', 'static inline int cached_flag_value(void) {
#ifdef V3_CACHE_FLAG_PROBE
	return 2;
#else
	return 1;
#endif
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '1'
	first_wrapper_stamps :=
		os.walk_ext(cache_dir, '.o.stamp').filter(os.file_name(it).starts_with('wrapper_'))
	assert first_wrapper_stamps.len == 1

	write_module_cache_file(root, 'main.v', 'module main

#flag -DV3_CACHE_FLAG_PROBE

import wrapper

fn main() {
	println(wrapper.value())
}
')
	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '2'
	second_wrapper_stamps :=
		os.walk_ext(cache_dir, '.o.stamp').filter(os.file_name(it).starts_with('wrapper_'))
	assert second_wrapper_stamps.len == 2
	assert second_wrapper_stamps.any(it !in first_wrapper_stamps)
}

fn test_module_cache_restart_preserves_compiler_stderr() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_restart_stdio_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

pub fn value() int {
	return 41
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)

	write_module_cache_file(root, 'main.v', 'module main

#flag -DV3_CACHE_RESTART_STDIO_PROBE

import wrapper

fn C.v3_missing_restart_stdio_symbol() int

fn main() {
	println(wrapper.value() + C.v3_missing_restart_stdio_symbol())
}
')
	second_output := os.join_path(root, 'second')
	stdout_file := os.join_path(root, 'stdout.txt')
	stderr_file := os.join_path(root, 'stderr.txt')
	result :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)} > ${os.quoted_path(stdout_file)} 2> ${os.quoted_path(stderr_file)}')
	assert result.exit_code != 0
	stdout := os.read_file(stdout_file) or { panic(err) }
	stderr := os.read_file(stderr_file) or { panic(err) }
	assert stderr.contains('C compilation failed:'), 'stdout:\n${stdout}\nstderr:\n${stderr}'
	assert !stdout.contains('C compilation failed:'), 'stdout:\n${stdout}\nstderr:\n${stderr}'
}

fn test_cached_objects_receive_forced_include_flags() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_forced_include_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

#flag -L@DIR -include @DIR/probe.h

fn C.cached_forced_include_value() int

pub fn value() int {
	return C.cached_forced_include_value()
}
')
	write_module_cache_file(root, 'wrapper/probe.h', 'static inline int cached_forced_include_value(void) {
	return 55;
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '55'
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '55'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0

	write_module_cache_file(root, 'wrapper/probe.h', 'static inline int cached_forced_include_value(void) {
	return 66;
}
')
	third_output := os.join_path(root, 'third')
	compile_module_cache_project(v3_bin, cache_dir, main_file, third_output)
	assert run_module_cache_binary(third_output) == '66'
	changed := changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir))
	assert changed.any(it.starts_with('wrapper_')), changed.str()
}

fn test_cached_header_preserves_c_preprocessor_state() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_preprocessor_state_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

#define V3_CACHE_USE_FEATURE 1
#if defined(V3_CACHE_USE_FEATURE)
#endif
#if defined(V3_CACHE_USE_FEATURE)
#pragma GCC diagnostic push
#insert "@DIR/api.h"
#pragma GCC diagnostic pop
#endif

fn C.cached_preprocessor_value() int

pub fn value() int {
	return C.cached_preprocessor_value()
}
')
	write_module_cache_file(root, 'wrapper/api.h', '#ifndef V3_CACHE_USE_FEATURE
#error "V3_CACHE_USE_FEATURE must be defined before api.h"
#endif

static inline int cached_preprocessor_value(void) {
	return 73;
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '73'
	header_path := module_cache_artifact(cache_dir, 'wrapper_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	define_pos := header.index('#define V3_CACHE_USE_FEATURE 1') or { -1 }
	if_pos := header.index('#if defined(V3_CACHE_USE_FEATURE)') or { -1 }
	pragma_pos := header.index('#pragma GCC diagnostic push') or { -1 }
	insert_pos := header.index('#insert ') or { -1 }
	endif_pos := header.last_index('#endif') or { -1 }
	assert define_pos >= 0, header
	assert header.count('#if defined(V3_CACHE_USE_FEATURE)') == 2, header
	assert header.count('#endif') == 2, header
	assert define_pos < if_pos && if_pos < pragma_pos && pragma_pos < insert_pos, header
	assert insert_pos < endif_pos, header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '73'
}

fn test_cached_objects_honor_strict_c_warnings() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_strict_c_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	library_dir := os.join_path(root, 'wrapper')
	write_module_cache_file(root, 'wrapper/strict_helper.c', 'int cached_strict_implicit_helper(void) {
	return 91;
}
')
	library_object := os.join_path(library_dir, 'strict_helper.o')
	library := os.join_path(library_dir, 'libstrictcache.a')
	cc_result := os.execute('cc -c -o ${os.quoted_path(library_object)} ${os.quoted_path(os.join_path(library_dir,
		'strict_helper.c'))}')
	assert cc_result.exit_code == 0, cc_result.output
	ar_result := os.execute('ar rcs ${os.quoted_path(library)} ${os.quoted_path(library_object)}')
	assert ar_result.exit_code == 0, ar_result.output
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

#flag -L@DIR
#flag -lstrictcache
#insert "@DIR/strict_probe.h"

fn C.cached_strict_call() int

pub fn value() int {
	return C.cached_strict_call()
}
')
	write_module_cache_file(root, 'wrapper/strict_probe.h', 'int cached_strict_implicit_helper(void);

static inline int cached_strict_call(void) {
	return cached_strict_implicit_helper();
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '91'
	first_wrapper_stamps :=
		os.walk_ext(cache_dir, '.o.stamp').filter(os.file_name(it).starts_with('wrapper_'))
	assert first_wrapper_stamps.len == 1

	strict_output := os.join_path(root, 'strict')
	strict_result :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -strict -o ${os.quoted_path(strict_output)} ${os.quoted_path(main_file)}')
	assert strict_result.exit_code == 0, strict_result.output
	assert run_module_cache_binary(strict_output) == '91'
	strict_wrapper_stamps :=
		os.walk_ext(cache_dir, '.o.stamp').filter(os.file_name(it).starts_with('wrapper_'))
	assert strict_wrapper_stamps.len == 2, strict_wrapper_stamps.str()
	assert strict_wrapper_stamps.any(it !in first_wrapper_stamps)

	write_module_cache_file(root, 'wrapper/strict_probe.h', 'static inline int cached_strict_call(void) {
	return cached_strict_implicit_helper();
}
')
	warning_cache_dir := os.join_path(root, 'warning_cache')
	warning_output := os.join_path(root, 'warning_nonstrict')
	warning_result :=
		os.execute('V3CACHE=${os.quoted_path(warning_cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(warning_output)} ${os.quoted_path(main_file)}')

	no_cache_output := os.join_path(root, 'strict_nocache')
	no_cache_result :=
		os.execute('${os.quoted_path(v3_bin)} -strict -nocache -o ${os.quoted_path(no_cache_output)} ${os.quoted_path(main_file)}')
	assert no_cache_result.exit_code != 0, no_cache_result.output
	assert no_cache_result.output.contains('cached_strict_implicit_helper'), no_cache_result.output
	if warning_result.exit_code == 0 {
		assert run_module_cache_binary(warning_output) == '91'
		warning_strict_output := os.join_path(root, 'warning_strict')
		warning_strict_result :=
			os.execute('V3CACHE=${os.quoted_path(warning_cache_dir)} ${os.quoted_path(v3_bin)} -strict -o ${os.quoted_path(warning_strict_output)} ${os.quoted_path(main_file)}')
		assert warning_strict_result.exit_code != 0, warning_strict_result.output
		assert warning_strict_result.output.contains('cached_strict_implicit_helper'), warning_strict_result.output
	} else {
		assert warning_result.output.contains('cached_strict_implicit_helper'), warning_result.output
	}
}

fn test_module_cache_rebuilds_objects_when_external_inputs_change() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_external_inputs_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

#insert "@DIR/value.h"

fn C.cached_external_value() int

pub fn value() int {
	return C.cached_external_value()
}
')
	write_module_cache_file(root, 'wrapper/value.h', '#include "nested_value.h"

static inline int cached_external_value(void) {
	return CACHED_EXTERNAL_VALUE;
}
')
	write_module_cache_file(root, 'wrapper/nested_value.h', '#define CACHED_EXTERNAL_VALUE 1
')
	write_module_cache_file(root, 'assets/assets.v', 'module assets

pub fn size() int {
	return $embed_file("payload.bin").len
}
')
	write_module_cache_file(root, 'assets/payload.bin', 'abc')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import assets
import wrapper

fn main() {
	println(wrapper.value())
	println(assets.size())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '1\n3'
	first_hashes := module_cache_object_hashes(cache_dir)

	write_module_cache_file(root, 'wrapper/nested_value.h', '#define CACHED_EXTERNAL_VALUE 2
')
	write_module_cache_file(root, 'assets/payload.bin', 'abcdef')
	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '2\n6'
	changed := changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir))
	assert changed.any(it.starts_with('assets_')), changed.str()
	assert changed.any(it.starts_with('wrapper_')), changed.str()
}

fn test_module_cache_rebuilds_modules_when_comptime_env_changes() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_comptime_env_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'wrapper/wrapper.v', "module wrapper

pub fn value() string {
	return \$env('V3_MODULE_CACHE_ENV_PROBE')
}
")
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	first :=
		os.execute('V3_MODULE_CACHE_ENV_PROBE=first V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(first_output)} ${os.quoted_path(main_file)}')
	assert first.exit_code == 0, first.output
	assert run_module_cache_binary(first_output) == 'first'

	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3_MODULE_CACHE_ENV_PROBE=second V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert run_module_cache_binary(second_output) == 'second'
}

fn test_cached_dependents_rebuild_when_imported_external_inputs_change() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_dependency_inputs_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'foo/foo.v', 'module foo

#insert "@DIR/api.h"

fn C.cached_transitive_value() int

pub fn touch() {}
')
	write_module_cache_file(root, 'foo/api.h', 'static inline int cached_transitive_value(void) {
	return 41;
}
')
	write_module_cache_file(root, 'bar/bar.v', 'module bar

import foo

pub fn value() int {
	foo.touch()
	return C.cached_transitive_value()
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import bar

fn main() {
	println(bar.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41'
	first_hashes := module_cache_object_hashes(cache_dir)

	write_module_cache_file(root, 'foo/api.h', 'static inline int cached_transitive_value(void) {
	return 42;
}
')
	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
	changed := changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir))
	assert changed.any(it.starts_with('bar_')), changed.str()
}

fn test_cached_object_rebuilds_when_import_resolves_to_new_header() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_import_shadow_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'foo/foo.v', 'module foo

pub struct Item {
pub:
	value int
}

pub fn marker() int {
	return 1
}
')
	project := os.join_path(root, 'project')
	write_module_cache_file(project, 'v.mod', "Module { name: 'import_shadow' }\n")
	write_module_cache_file(project, 'bar/bar.v', 'module bar

import foo

pub fn item_size() int {
	return int(sizeof(foo.Item))
}

pub fn marker() int {
	return foo.marker()
}
')
	main_file := os.join_path(project, 'main.v')
	write_module_cache_file(project, 'main.v', 'module main

import bar

fn main() {
	println(bar.item_size())
	println(bar.marker())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '4\n1'
	first_hashes := module_cache_object_hashes(cache_dir)

	write_module_cache_file(project, 'foo/foo.v', 'module foo

pub struct Item {
pub:
	left i64
	right i64
}

pub fn marker() int {
	return 2
}
')
	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '16\n2'
	changed := changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir))
	assert changed.any(it.starts_with('bar_')), changed.str()
}

fn test_cached_objects_precede_static_libraries_on_link_command() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_link_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	library_dir := os.join_path(root, 'archiveuser')
	write_module_cache_file(root, 'archiveuser/cache_order.c', 'int cache_order_value(void) {
	return 77;
}
')
	library_object := os.join_path(library_dir, 'cache_order.o')
	library := os.join_path(library_dir, 'libcacheorder.a')
	cc_result := os.execute('cc -c -o ${os.quoted_path(library_object)} ${os.quoted_path(os.join_path(library_dir,
		'cache_order.c'))}')
	assert cc_result.exit_code == 0, cc_result.output
	ar_result := os.execute('ar rcs ${os.quoted_path(library)} ${os.quoted_path(library_object)}')
	assert ar_result.exit_code == 0, ar_result.output
	write_module_cache_file(root, 'archiveuser/archiveuser.v', 'module archiveuser

#flag -L@DIR
#flag -lcacheorder

fn C.cache_order_value() int

pub fn value() int {
	return C.cache_order_value()
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import archiveuser

fn main() {
	println(archiveuser.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '77'

	second_output := os.join_path(root, 'second')
	result :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert result.exit_code == 0, result.output
	assert run_module_cache_binary(second_output) == '77'
	link_lines := result.output.split_into_lines().filter(it.contains('> cc '))
	assert link_lines.len > 0, result.output
	link_line := link_lines.last()
	object_pos := link_line.index(cache_dir) or { -1 }
	library_pos := link_line.index('-lcacheorder') or { -1 }
	assert object_pos >= 0, link_line
	assert library_pos >= 0, link_line
	assert object_pos < library_pos, link_line
}

fn test_cached_header_imports_use_original_source_context() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_import_context_${os.getpid()}')
	cache_dir := os.join_path(os.temp_dir(), 'v3_module_cache_import_context_store_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.rmdir_all(cache_dir) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
		os.rmdir_all(cache_dir) or {}
	}
	write_module_cache_file(root, 'app/v.mod', "Module { name: 'cache_import_context' }\n")
	write_module_cache_file(root, 'app/main.v', 'module main

import outer

fn main() {
	println(outer.value())
}
')
	write_module_cache_file(root, 'outer/outer.v', 'module outer

import sibling

pub fn value() int {
	return sibling.value() + 1
}
')
	write_module_cache_file(root, 'sibling/sibling.v', 'module sibling

pub fn value() int {
	return 41
}
')
	main_file := os.join_path(root, 'app/main.v')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '42'
	outer_header := module_cache_artifact(cache_dir, 'outer_', '.vh')
	assert outer_header.len > 0
	assert (os.read_file(outer_header) or { panic(err) }).contains('import sibling')

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
}

fn test_program_generic_specializations_stay_out_of_builtin_object() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_program_generics_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

struct First {
	id int
	name string
}

fn render[T](value T) string {
	return "\${value}"
}

fn main() {
	value := First{
		id: 7
		name: "v"
	}
	mut rows := []string{}
	$for field in First.fields {
		rows << render(value.$(field.name))
	}
	println(rows.join("|"))
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '7|v'
	first_hashes := module_cache_object_hashes(cache_dir)
	builtin_names := first_hashes.keys().filter(it.starts_with('builtin_'))
	assert builtin_names.len == 1
	builtin_name := builtin_names[0]
	builtin_hash := first_hashes[builtin_name]

	write_module_cache_file(root, 'main.v', 'module main

struct Second {
	count int
	label string
}

fn combine[A, B](a A, b B) string {
	_ = a
	return "\${b}"
}

fn main() {
	value := Second{
		count: 2
		label: "x"
	}
	mut rows := []string{}
	$for field in Second.fields {
		rows << combine(value.$(field.name), field.name)
	}
	println(rows.join("|"))
}
')
	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == 'count|label'
	second_hashes := module_cache_object_hashes(cache_dir)
	assert second_hashes[builtin_name] == builtin_hash
}

fn test_dotted_user_module_stays_out_of_builtin_bundle() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_dotted_hash_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'v.mod', "Module { name: 'dotted_hash_cache' }\n")
	write_module_cache_file(root, 'foo/hash/hash.v', 'module hash

fn internal_value() int {
	return 40
}

pub fn value() int {
	return internal_value() + 1
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import foo.hash

fn main() {
	println(hash.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41'
	first_hashes := module_cache_object_hashes(cache_dir)
	user_objects := first_hashes.keys().filter(it.starts_with('foo_hash_'))
	assert user_objects.len == 1, first_hashes.keys().str()
	builtin_objects := first_hashes.keys().filter(it.starts_with('builtin_'))
	assert builtin_objects.len == 1
	builtin_name := builtin_objects[0]

	write_module_cache_file(root, 'foo/hash/hash.v', 'module hash

fn internal_value() int {
	return 41
}

pub fn value() int {
	return internal_value() + 1
}
')
	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
	second_hashes := module_cache_object_hashes(cache_dir)
	assert second_hashes[builtin_name] == first_hashes[builtin_name]
	changed := changed_module_cache_objects(first_hashes, second_hashes)
	assert changed.any(it.starts_with('foo_hash_')), changed.str()
	assert !changed.any(it.starts_with('builtin_')), changed.str()
}

fn test_top_level_user_module_stays_out_of_builtin_bundle() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_top_level_hash_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	first_project := os.join_path(root, 'first_project')
	write_module_cache_file(first_project, 'v.mod', "Module { name: 'first_project' }\n")
	write_module_cache_file(first_project, 'hash/hash.v', 'module hash

pub fn value() int {
	return 41
}
')
	first_main := os.join_path(first_project, 'main.v')
	write_module_cache_file(first_project, 'main.v', 'module main

import hash

fn main() {
	println(hash.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, first_main, first_output)
	assert run_module_cache_binary(first_output) == '41'
	first_hashes := module_cache_object_hashes(cache_dir)
	user_objects := first_hashes.keys().filter(it.starts_with('hash_'))
	assert user_objects.len == 1, first_hashes.keys().str()
	builtin_objects := first_hashes.keys().filter(it.starts_with('builtin_'))
	assert builtin_objects.len == 1

	second_project := os.join_path(root, 'second_project')
	write_module_cache_file(second_project, 'v.mod', "Module { name: 'second_project' }\n")
	second_main := os.join_path(second_project, 'main.v')
	write_module_cache_file(second_project, 'main.v', 'module main

fn main() {
	println(42)
}
')
	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, second_main, second_output)
	assert run_module_cache_binary(second_output) == '42'
	second_hashes := module_cache_object_hashes(cache_dir)
	assert second_hashes.keys().filter(it.starts_with('builtin_')).len == 1
}

fn test_cached_headers_keep_full_module_identity() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_full_identity_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'v.mod', "Module { name: 'full_identity' }\n")
	write_module_cache_file(root, 'bar/bar.v', 'module bar

pub fn unused() int {
	return 0
}
')
	write_module_cache_file(root, 'foo/bar/bar.v', 'module bar

pub struct FooValue {
pub:
	n int
}

pub fn make() FooValue {
	return FooValue{
		n: 41
	}
}
')
	write_module_cache_file(root, 'baz/bar/bar.v', "module bar

pub struct BazValue {
pub:
	text string
}

pub fn make() BazValue {
	return BazValue{
		text: 'ok'
	}
}
")
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import foo.bar as foo_bar
import baz.bar as baz_bar

fn main() {
	println(foo_bar.make().n)
	println(baz_bar.make().text)
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41\nok'
	foo_header_path := module_cache_artifact(cache_dir, 'foo_bar_', '.vh')
	baz_header_path := module_cache_artifact(cache_dir, 'baz_bar_', '.vh')
	assert foo_header_path.len > 0
	assert baz_header_path.len > 0
	foo_header := os.read_file(foo_header_path) or { panic(err) }
	baz_header := os.read_file(baz_header_path) or { panic(err) }
	assert foo_header.contains('struct FooValue'), foo_header
	assert !foo_header.contains('struct BazValue'), foo_header
	assert baz_header.contains('struct BazValue'), baz_header
	assert !baz_header.contains('struct FooValue'), baz_header
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '41\nok'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_module_body_recreates_cross_module_generic_specializations() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_cross_module_generics_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'generic/generic.v', 'module generic

pub fn identity[T](value T) T {
	return value
}
')
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

import generic

pub fn value() int {
	return generic.identity[int](42)
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '42'
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_generic_receiver_method_keeps_its_body() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_generic_receiver_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'genericbox/genericbox.v', 'module genericbox

pub struct Box[T] {
pub:
	value T
}

pub fn (box Box[T]) get() T {
	return box.value
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import genericbox

fn main() {
	box := genericbox.Box[int]{
		value: 42
	}
	println(box.get())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '42'
	first_hashes := module_cache_object_hashes(cache_dir)
	header_path := module_cache_artifact(cache_dir, 'genericbox_', '.vh')
	assert header_path.len > 0
	assert modulecache.header_needs_source(modulecache.Entry{
		header: header_path
	})

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_header_preserves_immutable_reference_receiver() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_reference_receiver_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'refbox/refbox.v', 'module refbox

pub struct Item {
pub:
	value int
}

pub fn (item &Item) read() int {
	return item.value
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import refbox

fn main() {
	item := refbox.Item{
		value: 37
	}
	println(item.read())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '37'
	header_path := module_cache_artifact(cache_dir, 'refbox_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('fn (item &Item) read() int'), header
	assert !header.contains('fn (mut item Item) read() int'), header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '37'
}

fn test_cached_header_preserves_immutable_interface_parameter() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_interface_reference_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'contract/contract.v', 'module contract

pub interface Sink {
	put(value &int) int
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import contract

struct Writer {}

fn (writer Writer) put(value &int) int {
	_ = writer
	_ = value
	return 43
}

fn apply(sink contract.Sink, value &int) int {
	return sink.put(value)
}

fn main() {
	value := 1
	println(apply(Writer{}, &value))
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '43'
	header_path := module_cache_artifact(cache_dir, 'contract_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('put(arg0 &int) int'), header
	assert !header.contains('put(mut arg0 int) int'), header
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '43'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_header_preserves_comptime_struct_fields() {
	mut a := flat.FlatAst.new()
	module_id := a.add_node(flat.Node{
		kind:  .module_decl
		value: 'config'
	})
	direct_field := a.add_node(flat.Node{
		kind:           .field_decl
		value:          'value'
		typ:            'int'
		generic_params: ['p']
	})
	comptime_field := a.add_node(flat.Node{
		kind:           .field_decl
		value:          'enabled'
		typ:            'bool'
		generic_params: ['p']
	})
	block_children := a.begin_children()
	a.add_child(comptime_field)
	comptime_block := a.add_node(flat.Node{
		kind:           .block
		children_start: block_children
		children_count: 1
	})
	struct_children := a.begin_children()
	a.add_child(direct_field)
	a.add_child(comptime_block)
	struct_id := a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'Config'
		children_start: struct_children
		children_count: 2
	})
	file_children := a.begin_children()
	a.add_child(module_id)
	a.add_child(struct_id)
	a.add_node(flat.Node{
		kind:           .file
		value:          'config.v'
		children_start: file_children
		children_count: 2
	})
	tc := types.TypeChecker.new(a)
	header := modulecache.module_header(a, tc, 'config', '', map[string]string{})
	assert !header.contains('source bodies required')
	assert header.contains('\tvalue int')
	assert header.contains('\tenabled bool')
}

fn test_cached_struct_default_with_unsupported_initializer_keeps_source() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_struct_default_index_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'defaulted/defaulted.v', 'module defaulted

const values = [41, 42]

pub struct Config {
pub:
	first int = values[0]
}

pub fn cached_value() int {
	return Config{}.first
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import defaulted

fn main() {
	println(defaulted.Config{}.first)
	println(defaulted.cached_value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41\n41'
	header_path := module_cache_artifact(cache_dir, 'defaulted_', '.vh')
	assert header_path.len > 0
	assert modulecache.header_needs_source(modulecache.Entry{
		header: header_path
	})
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '41\n41'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_const_with_unsupported_initializer_keeps_source() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_const_index_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'indexed/indexed.v', 'module indexed

pub const values = [41, 42]
pub const first = values[0]

pub fn cached_value() int {
	return first
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import indexed

fn main() {
	println(indexed.first)
	println(indexed.cached_value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41\n41'
	header_path := module_cache_artifact(cache_dir, 'indexed_', '.vh')
	assert header_path.len > 0
	assert modulecache.header_needs_source(modulecache.Entry{
		header: header_path
	})

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '41\n41'
}

fn test_cached_global_with_unsupported_initializer_keeps_source() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_global_index_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'initialized/initialized.v', 'module initialized

const values = [41, 42]

__global first = values[0]

pub fn cached_value() int {
	return first
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import initialized

fn main() {
	println(initialized.cached_value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41'
	header_path := module_cache_artifact(cache_dir, 'initialized_', '.vh')
	assert header_path.len > 0
	assert modulecache.header_needs_source(modulecache.Entry{
		header: header_path
	})
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '41'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_header_preserves_noreturn_attribute() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_noreturn_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'terminator/terminator.v', 'module terminator

@[noreturn]
pub fn die() {
	exit(0)
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import terminator

fn value() int {
	terminator.die()
}

fn main() {}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	header_path := module_cache_artifact(cache_dir, 'terminator_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('@[noreturn]\nfn die()')
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_module_cache_rebuilds_modules_when_builtin_layout_changes() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_builtin_layout_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	create_module_cache_vroot(root)
	write_module_cache_file(root, 'vlib/builtin/cache_probe.v', 'module builtin

pub struct CacheProbe {
pub:
	value int
}
')
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

pub fn cached_size() int {
	return int(sizeof(CacheProbe))
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println("\${wrapper.cached_size()}:\${int(sizeof(CacheProbe))}")
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	first_sizes := run_module_cache_binary(first_output).split(':')
	assert first_sizes.len == 2
	assert first_sizes[0] == first_sizes[1]
	wrapper_stamp := module_cache_artifact(cache_dir, 'wrapper_', '.o.stamp')
	assert wrapper_stamp.len > 0
	stamp := os.read_file(wrapper_stamp) or { panic(err) }
	assert stamp.contains('dependency=') && stamp.contains('builtin_'), stamp

	write_module_cache_file(root, 'vlib/builtin/cache_probe.v', 'module builtin

pub struct CacheProbe {
pub:
	value int
	extra i64
}
')
	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	second_sizes := run_module_cache_binary(second_output).split(':')
	assert second_sizes.len == 2
	assert second_sizes[0] == second_sizes[1]
	assert second_sizes[0] != first_sizes[0]
}

fn test_module_cache_reuses_headers_and_rebuilds_only_changed_module() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_project_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}

	write_module_cache_file(root, 'foo/foo.v', 'module foo

pub struct Item {
pub:
	value int
}

pub const answer = 21

pub fn value() int {
	return answer
}

pub fn unused_but_cached() int {
	return 900
}
')
	write_module_cache_file(root, 'bar/bar.v', 'module bar

import foo

pub fn doubled() int {
	return foo.value() * 2
}

pub fn item_size() int {
	return int(sizeof(foo.Item))
}
')
	write_module_cache_file(root, 'genericmod/genericmod.v', 'module genericmod

pub fn identity[T](value T) T {
	return value
}

pub fn internal_value() int {
	return identity[int](23)
}
')
	write_module_cache_file(root, 'sumcache/sumcache.v', 'module sumcache

pub type Value = int | string

pub fn equal_ints() bool {
	a := Value(1)
	b := Value(1)
	return a == b
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import bar
import genericmod
import sumcache

fn main() {
	println(bar.doubled())
	println(bar.item_size())
	println(genericmod.identity[int](17))
	println(genericmod.internal_value())
	println(sumcache.equal_ints())
}
')

	cache_dir := os.join_path(root, 'cache')
	first_bin := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_bin)
	assert run_module_cache_binary(first_bin) == '42\n4\n17\n23\ntrue'

	first_hashes := module_cache_object_hashes(cache_dir)
	assert first_hashes.keys().any(it.starts_with('foo_'))
	assert first_hashes.keys().any(it.starts_with('bar_'))
	assert first_hashes.keys().any(it.starts_with('genericmod_'))
	assert first_hashes.keys().any(it.starts_with('sumcache_'))
	core_objects := first_hashes.keys().filter(it.starts_with('builtin_')
		|| it.starts_with('strconv_') || it.starts_with('strings_') || it.starts_with('hash_')
		|| it.starts_with('bits_'))
	assert core_objects.len == 1, core_objects.str()
	assert core_objects[0].starts_with('builtin_'), core_objects.str()

	foo_header_path := module_cache_artifact(cache_dir, 'foo_', '.vh')
	assert foo_header_path.len > 0
	foo_header := os.read_file(foo_header_path) or { panic(err) }
	assert foo_header.contains('struct Item')
	assert foo_header.contains('const (')
	assert foo_header.contains('fn value() int')
	assert !foo_header.contains('return answer')
	assert module_cache_artifact(cache_dir, 'hash_', '.vh').len > 0

	write_module_cache_file(root, 'foo/foo.v', 'module foo

pub struct Item {
pub:
	value int
}

pub const answer = 21

pub fn value() int {
	return 22
}

pub fn unused_but_cached() int {
	return 900
}
')
	second_bin := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_bin)
	assert run_module_cache_binary(second_bin) == '44\n4\n17\n23\ntrue'
	second_hashes := module_cache_object_hashes(cache_dir)
	changed_after_foo := changed_module_cache_objects(first_hashes, second_hashes)
	assert changed_after_foo.len == 1, changed_after_foo.str()
	assert changed_after_foo[0].starts_with('foo_'), changed_after_foo.str()

	write_module_cache_file(root, 'foo/foo.v', 'module foo

pub struct Item {
pub:
	value int
	extra i64
}

pub const answer = 21

pub fn value() int {
	return 22
}

pub fn unused_but_cached() int {
	return 900
}
')
	layout_bin := os.join_path(root, 'layout')
	compile_module_cache_project(v3_bin, cache_dir, main_file, layout_bin)
	assert run_module_cache_binary(layout_bin) == '44\n16\n17\n23\ntrue'
	layout_hashes := module_cache_object_hashes(cache_dir)
	changed_after_layout := changed_module_cache_objects(second_hashes, layout_hashes)
	assert changed_after_layout.len == 1, changed_after_layout.str()
	assert changed_after_layout[0].starts_with('bar_'), changed_after_layout.str()

	bar_stamp_path := module_cache_artifact(cache_dir, 'bar_', '.o.stamp')
	assert bar_stamp_path.len > 0
	bar_stamp_before := module_cache_object_hash(bar_stamp_path)
	current_foo_header := os.read_file(foo_header_path) or { panic(err) }
	os.write_file(foo_header_path, current_foo_header + '\n// dependency signature probe\n') or {
		panic(err)
	}
	signature_bin := os.join_path(root, 'signature')
	compile_module_cache_project(v3_bin, cache_dir, main_file, signature_bin)
	assert run_module_cache_binary(signature_bin) == '44\n16\n17\n23\ntrue'
	assert module_cache_object_hash(bar_stamp_path) != bar_stamp_before
	signature_hashes := module_cache_object_hashes(cache_dir)
	assert changed_module_cache_objects(layout_hashes, signature_hashes).len == 0

	alternate_main_file := os.join_path(root, 'alternate_main.v')
	write_module_cache_file(root, 'alternate_main.v', 'module main

import foo
import hash

fn main() {
	println(foo.unused_but_cached())
	println(hash.wymum(2, 3))
	println(["a", "b"].join("-"))
}
')
	alternate_bin := os.join_path(root, 'alternate')
	compile_module_cache_project(v3_bin, cache_dir, alternate_main_file, alternate_bin)
	assert run_module_cache_binary(alternate_bin) == '900\n6\na-b'
	alternate_hashes := module_cache_object_hashes(cache_dir)
	assert changed_module_cache_objects(signature_hashes, alternate_hashes).len == 0

	write_module_cache_file(root, 'main.v', "module main

import foo
import genericmod

fn main() {
\tprintln(foo.unused_but_cached())
\tprintln(genericmod.identity[string]('new'))
}
")
	third_bin := os.join_path(root, 'third')
	compile_module_cache_project(v3_bin, cache_dir, main_file, third_bin)
	assert run_module_cache_binary(third_bin) == '900\nnew'
	third_hashes := module_cache_object_hashes(cache_dir)
	assert changed_module_cache_objects(alternate_hashes, third_hashes).len == 0
}

fn test_cached_bundle_imports_do_not_leak_into_user_scope() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_import_scope_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

fn main() {
	_ := strings.new_builder(16)
}
')
	cache_dir := os.join_path(root, 'cache')
	output := os.join_path(root, 'out')
	result :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(output)} ${os.quoted_path(main_file)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('strings'), result.output
}

fn test_interface_type_id_hash_collision_is_resolved() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_interface_type_id_collision_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

interface CollisionValue {
	value() int
}

struct TZjXQlDs6 {}
struct T2nAMbYQH {}

fn (v TZjXQlDs6) value() int {
	_ = v
	return 1
}

fn (v T2nAMbYQH) value() int {
	_ = v
	return 2
}

fn interface_value(value CollisionValue) int {
	return value.value()
}

fn main() {
	println(interface_value(TZjXQlDs6{}))
	println(interface_value(T2nAMbYQH{}))
	first := CollisionValue(TZjXQlDs6{})
	second := CollisionValue(T2nAMbYQH{})
	println(first is TZjXQlDs6)
	println(first is T2nAMbYQH)
	println(second is TZjXQlDs6)
	println(second is T2nAMbYQH)
}
')
	cache_dir := os.join_path(root, 'cache')
	output := os.join_path(root, 'out')
	compile_module_cache_project(v3_bin, cache_dir, main_file, output)
	assert run_module_cache_binary(output) == '1\n2\ntrue\nfalse\nfalse\ntrue'
}
