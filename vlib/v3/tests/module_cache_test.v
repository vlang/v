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
	assert result.exit_code == 0, '${path}: ${result.output}'
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

fn test_cached_object_accepts_recorded_dependency_superset() {
	root := os.join_path(os.temp_dir(), 'v3_module_cache_dependency_superset_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	old_cache := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := old_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'foo.v')
	first_dependency := os.join_path(root, 'first.h')
	extra_dependency := os.join_path(root, 'extra.h')
	new_dependency := os.join_path(root, 'new.h')
	write_module_cache_file(root, 'foo.v', 'module foo')
	write_module_cache_file(root, 'first.h', '#define FIRST 1')
	write_module_cache_file(root, 'extra.h', '#define EXTRA 1')
	write_module_cache_file(root, 'new.h', '#define NEW 1')
	manager := modulecache.new_manager(root, 'dependency-superset', true, '')
	assert manager.ensure_dir()
	compile_signature := 'flags'
	entry := manager.object_entry('foo', [source], compile_signature)
	os.write_file(entry.object, '') or { panic(err) }
	manager.write_stamp('foo', [source], {
		first_dependency: modulecache.file_signature(first_dependency)
		extra_dependency: modulecache.file_signature(extra_dependency)
	}, compile_signature) or { panic(err) }
	_ := manager.valid_object_for_compile_signature('foo', [source], compile_signature, {
		first_dependency: modulecache.file_signature(first_dependency)
	}) or {
		assert false, 'a recorded dependency superset should remain valid'
		return
	}
	if _ := manager.valid_object_for_compile_signature('foo', [source], compile_signature, {
		first_dependency: modulecache.file_signature(first_dependency)
		new_dependency:   modulecache.file_signature(new_dependency)
	})
	{
		assert false, 'a newly discovered dependency must invalidate the object'
	}
}

fn module_cache_object_hashes(cache_dir string) map[string]u64 {
	mut hashes := map[string]u64{}
	for path in os.walk_ext(cache_dir, '.o') {
		name := os.file_name(path)
		if !name.starts_with('program_prefix_') {
			hashes[name] = module_cache_object_hash(path)
		}
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

fn test_module_cache_reuses_and_invalidates_whole_program_cgen_plan() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_cgen_plan_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'cached/cached.v', 'module cached

pub fn value() int {
	return 40
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import cached

fn identity[T](value T) T {
	return value
}

fn main() {
	println(identity(cached.value() + 2))
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	first :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(first_output)} ${os.quoted_path(main_file)}')
	assert first.exit_code == 0, first.output
	assert !first.output.contains('check (cached)'), first.output
	assert !first.output.contains('cgen (cached)'), first.output
	assert !first.output.contains('monomorphize (cached)'), first.output
	assert run_module_cache_binary(first_output) == '42'

	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert second.output.contains('check (cached)'), second.output
	assert second.output.contains('markused (cached)'), second.output
	assert second.output.contains('transform (cached)'), second.output
	assert second.output.contains('annotate types (cached)'), second.output
	assert second.output.contains('C module plan (cached)'), second.output
	assert second.output.contains('cgen (cached)'), second.output
	assert second.output.contains('monomorphize (cached)'), second.output
	$if macos {
		assert second.output.contains('cc (cached)'), second.output
	}
	assert run_module_cache_binary(second_output) == '42'

	write_module_cache_file(root, 'main.v', 'module main

import cached

fn identity[T](value T) T {
	return value
}

fn main() {
	println(identity(cached.value() + 3))
}
')
	changed_output := os.join_path(root, 'changed')
	changed :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(changed_output)} ${os.quoted_path(main_file)}')
	assert changed.exit_code == 0, changed.output
	assert !changed.output.contains('check (cached)'), changed.output
	assert !changed.output.contains('cgen (cached)'), changed.output
	assert !changed.output.contains('monomorphize (cached)'), changed.output
	assert run_module_cache_binary(changed_output) == '43'

	write_module_cache_file(root, 'cached/cached.v', 'module cached

pub fn value() int {
	return 41
}
')
	changed_module_output := os.join_path(root, 'changed_module')
	changed_module :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(changed_module_output)} ${os.quoted_path(main_file)}')
	assert changed_module.exit_code == 0, changed_module.output
	assert !changed_module.output.contains('check (cached)'), changed_module.output
	assert !changed_module.output.contains('cgen (cached)'), changed_module.output
	assert !changed_module.output.contains('monomorphize (cached)'), changed_module.output
	assert run_module_cache_binary(changed_module_output) == '44'
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

fn test_module_cache_declaration_header_keeps_directives_in_pending_declaration() {
	prefix := 'int cached_call(
#ifdef CACHED_EXTRA_PARAMETER
	int extra,
#endif
	int value
);
'
	header := modulecache.declaration_header(prefix)
	assert header.count('int cached_call(') == 1, header
	assert header.contains(prefix), header
}

fn test_module_cache_declaration_header_ignores_semicolons_inside_block_comments() {
	prefix := '/*
revision history:
  feature works again;
  #ifdef text inside the comment
  another fix;
*/
int value = 1;
'
	header := modulecache.declaration_header(prefix)
	assert header.contains('extern int value;')
	assert !header.contains('extern feature')
	assert !header.contains('extern another')
	assert !header.contains('#ifdef text')
	assert !modulecache.c_source_has_static_storage(prefix)
}

fn test_module_cache_declaration_header_handles_extern_c_and_trailing_comments() {
	prefix := '#ifdef __cplusplus
extern "C" {
#endif
int cached_fn(int value); // public API
typedef struct
{
	float x;
	float y;
} CachedPoint;
#ifdef __cplusplus
} // extern "C"
#endif
'
	header := modulecache.declaration_header(prefix)
	assert header.contains('extern "C" {')
	assert header.contains('int cached_fn(int value); // public API')
	assert header.contains('float x;\n\tfloat y;\n} CachedPoint;')
	assert !header.contains('typedef struct;')
	assert header.contains('} // extern "C"')
}

fn test_module_cache_declaration_header_keeps_macro_function_conditionals_bounded() {
	prefix := '#if defined(CACHED_EMSCRIPTEN)
EM_JS(void, cached_js_fn, (void), {
	if (Module.value) {
		Module.value++;
	}
})
#endif
#if defined(CACHED_WINDOWS)
void cached_windows_fn(bool enabled) {
#if defined(CACHED_OPTION)
	if (enabled) {
		cached_js_fn();
	}
#endif
}
#endif
int cached_after(void) {
	return 1;
}
'
	header := modulecache.declaration_header(prefix)
	assert header.count('#endif') == 2
	assert header.contains('void cached_windows_fn(bool enabled);')
	assert header.contains('int cached_after(void);')
	assert !header.contains('extern Module.value')
	assert !header.contains('extern return 1')
}

fn test_module_cache_declaration_header_keeps_native_headers_without_implementations() {
	prefix := 'int generated_state = 1;
/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */
#define SOKOL_APP_IMPL
#define STB_IMAGE_IMPLEMENTATION
#define FEATURE_FLAG
/* Usage:
   #define STB_IMAGE_IMPLEMENTATION
*/
typedef struct NativeType { int value; } NativeType;
#ifdef SOKOL_APP_IMPL
int native_implementation(void) { return 1; }
#endif
/* V3CACHE_NATIVE_DIRECTIVES_END */
/* V3CACHE_SOURCE_DIRECTIVES_BEGIN */
int native_source_body(void) { return 2; }
/* V3CACHE_SOURCE_DIRECTIVES_END */
int generated_fn(void) { return 3; }
'
	header := modulecache.declaration_header(prefix)
	assert header.contains('extern int generated_state;')
	assert header.contains('typedef struct NativeType { int value; } NativeType;')
	assert header.contains('#define FEATURE_FLAG')
	assert header.contains('/* Usage:\n   #define STB_IMAGE_IMPLEMENTATION\n*/')
	assert header.contains('/* v3 cache omitted SOKOL_APP_IMPL */')
	assert header.contains('/* v3 cache omitted STB_IMAGE_IMPLEMENTATION */')
	assert !header.contains('#define SOKOL_APP_IMPL')
	assert header.count('#define STB_IMAGE_IMPLEMENTATION') == 1
	assert !header.contains('native_source_body')
	assert header.contains('int generated_fn(void);')
}

fn test_module_cache_native_header_conditional_functions_become_declarations() {
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */
#if defined(CACHED_GL)
int cached_readback(int value) {
	return value;
}
#else
int cached_readback(int value) {
	return -value;
}
#endif
/* V3CACHE_NATIVE_DIRECTIVES_END */
'
	header := modulecache.declaration_header(prefix)
	assert header.count('static int cached_readback(int value) {') == 2
	assert header.contains('return value')
	assert header.contains('return -value')
}

fn test_module_cache_native_header_localizes_split_brace_functions() {
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */
int cached_readback(int value)
{
	return value;
}
int cached_sum(
	int left,
	int right
)
{
	return left + right;
}
__attribute__((visibility("default"))) int cached_attributed(void)
{
	return 42;
}
/* V3CACHE_NATIVE_DIRECTIVES_END */
'
	header := modulecache.declaration_header(prefix)
	assert header.contains('static int cached_readback(int value)\n{'), header
	assert header.contains('static int cached_sum(\n\tint left,\n\tint right\n)\n{'), header
	assert header.contains('static __attribute__((visibility("default"))) int cached_attributed(void)\n{'), header
}

fn test_module_cache_declaration_header_keeps_column_zero_inner_block_closes() {
	prefix := 'int cached_nested(int value) {
if (value) {
	value++;
}
return value;
}
int cached_after(void) {
	return 1;
}
'
	header := modulecache.declaration_header(prefix)
	assert header.contains('int cached_nested(int value);'), header
	assert header.contains('int cached_after(void);'), header
	assert !header.contains('extern return value'), header
}

fn test_module_cache_declaration_header_bounds_raw_conditional_branches() {
	prefix := '#if defined(CACHED_BRANCH)
int cached_branch(bool enabled) {
#if defined(CACHED_POSITIVE)
	if (enabled) {
#else
	if (!enabled) {
#endif
		return 1;
	}
}
#endif
int cached_after_branch(void) {
	return 2;
}
'
	header := modulecache.declaration_header(prefix)
	assert header.contains('int cached_branch(bool enabled);'), header
	assert header.contains('int cached_after_branch(void);'), header
	assert header.count('#endif') == 1, header
}

fn test_cached_header_preserves_include_search_path_names() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_include_search_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'wrapper/include/cached_value.h', '#define CACHED_VALUE 42\n')
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

#flag -I @DIR/include
#include "cached_value.h"

pub fn value() int {
	return C.CACHED_VALUE
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
	header_path := module_cache_artifact(cache_dir, 'wrapper_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('#include "cached_value.h"'), header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
}

fn test_cached_unused_callback_declaration_emits_fn_pointer_typedef() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_unused_callback_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

pub fn apply(cb fn (int) int, value int) int {
	return cb(value)
}

pub fn value() int {
	return 42
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

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
}

fn test_module_cache_static_inline_attributes_are_not_storage() {
	source := 'static __inline void atomic_fence(int order __attribute__((unused))) {\n\t(void)order;\n}\n'
	assert !modulecache.c_source_has_static_storage(source)
	assert modulecache.c_source_has_static_storage('static int state = 1;\n')
	assert modulecache.c_source_has_static_storage('static inline int next(void) {\n\tstatic int state;\n\treturn ++state;\n}\n')
}

fn test_module_cache_declaration_header_keeps_directives_inside_static_inline_functions() {
	prefix := 'static inline void *aligned_alloc_for_target(void) {
#ifdef _WIN32
	return _aligned_malloc(16, 16);
#else
	return malloc(16);
#endif
}
'
	header := modulecache.declaration_header(prefix)
	assert header.contains('static inline void *aligned_alloc_for_target(void) {\n#ifdef _WIN32')
	assert header.contains('#else\n\treturn malloc(16);\n#endif\n}')
}

fn test_module_cache_declaration_header_keeps_directives_inside_type_blocks() {
	prefix := 'typedef struct CachedConditional {
#ifdef CACHED_WIDE_FIELD
	long value;
#else
	int value;
#endif
} CachedConditional;
'
	header := modulecache.declaration_header(prefix)
	assert header.contains(prefix), header
}

fn test_stateful_native_module_is_cached_with_its_owner() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_stateful_native_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'native/native.v', 'module native

#include "@DIR/native.c"

fn C.native_value() int

pub fn value() int {
	identity := fn (value int) int {
		return value
	}
	return identity(C.native_value())
}
')
	write_module_cache_file(root, 'native/native.c', 'static int native_state = 40;

int native_value(void) {
	return ++native_state;
}
')
	write_module_cache_file(root, 'cached/cached.v', 'module cached

pub fn value() int {
	return 1
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import cached
import native

fn main() {
	println(cached.value() + native.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '42'
	assert module_cache_artifact(cache_dir, 'cached_', '.o').len > 0
	assert module_cache_artifact(cache_dir, 'native_', '.o').len > 0
	native_header := module_cache_artifact(cache_dir, 'native_', '.vh')
	assert native_header.len > 0
	assert !(os.read_file(native_header) or { panic(err) }).contains('compile source in program unit')

	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert run_module_cache_binary(second_output) == '42'
	vh_lines := second.output.split_into_lines().filter(it.contains('parsed .vh files'))
	assert vh_lines.len == 1, second.output
	fields := vh_lines[0].fields()
	assert fields.len >= 4 && fields[3].int() > 0, second.output
	vh_file_lines :=
		second.output.split_into_lines().filter(it.trim_space().starts_with('.vh files:'))
	assert vh_file_lines.len == 1, second.output
	vh_files := vh_file_lines[0].all_after(':').trim_space().split(' ')
	assert vh_files.len == fields[3].int(), second.output
	assert vh_files.all(it.ends_with('.vh')), second.output
	mut expected_vh_lines := 0
	for path in vh_files {
		vh_source := os.read_file(path) or { panic(err) }
		expected_vh_lines += vh_source.count('\n') + if vh_source.ends_with('\n') { 0 } else { 1 }
	}
	parsed_vh_line_metrics :=
		second.output.split_into_lines().filter(it.contains('parsed .vh lines'))
	assert parsed_vh_line_metrics.len == 1, second.output
	vh_line_fields := parsed_vh_line_metrics[0].fields()
	assert vh_line_fields.len >= 5 && vh_line_fields[3].int() == expected_vh_lines, second.output
	v_lines := second.output.split_into_lines().filter(it.contains('parsed .v files'))
	assert v_lines.len == 1, second.output
	v_fields := v_lines[0].fields()
	assert v_fields.len >= 4 && v_fields[3].int() > 0, second.output
	v_file_lines :=
		second.output.split_into_lines().filter(it.trim_space().starts_with('.v files:'))
	assert v_file_lines.len == 1, second.output
	v_files := v_file_lines[0].all_after(':').trim_space().split(' ')
	assert v_files.len == v_fields[3].int(), second.output
	assert v_files == [main_file], second.output
	v_source := os.read_file(main_file) or { panic(err) }
	expected_v_lines := v_source.count('\n') + if v_source.ends_with('\n') { 0 } else { 1 }
	parsed_v_line_metrics := second.output.split_into_lines().filter(it.contains('parsed .v lines'))
	assert parsed_v_line_metrics.len == 1, second.output
	line_fields := parsed_v_line_metrics[0].fields()
	assert line_fields.len >= 5 && line_fields[3].int() == expected_v_lines, second.output
}

fn test_cold_module_cache_prints_reuse_hint_only_once() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_cold_hint_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'cached/cached.v', 'module cached

pub fn value() int {
	return 42
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import cached

fn main() {
	println(cached.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	hint := 'will not be recompiled on the next run unless they change'
	cold_stdout := os.join_path(root, 'cold.stdout')
	cold_stderr := os.join_path(root, 'cold.stderr')
	cold_output := os.join_path(root, 'cold')
	cold :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(cold_output)} ${os.quoted_path(main_file)} > ${os.quoted_path(cold_stdout)} 2> ${os.quoted_path(cold_stderr)}')
	assert cold.exit_code == 0, cold.output
	cold_stdout_text := os.read_file(cold_stdout) or { panic(err) }
	cold_stderr_text := os.read_file(cold_stderr) or { panic(err) }
	assert cold_stdout_text.contains('Hint: cached '), cold_stdout_text
	assert cold_stdout_text.contains(hint), cold_stdout_text
	assert !cold_stderr_text.contains(hint), cold_stderr_text

	warm_stdout := os.join_path(root, 'warm.stdout')
	warm_output := os.join_path(root, 'warm')
	warm :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(warm_output)} ${os.quoted_path(main_file)} > ${os.quoted_path(warm_stdout)}')
	assert warm.exit_code == 0, warm.output
	warm_stdout_text := os.read_file(warm_stdout) or { panic(err) }
	assert !warm_stdout_text.contains(hint), warm_stdout_text
}

fn test_cached_native_source_fallback_declaration_is_not_in_program_unit() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_native_fallback_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'native/native.v', 'module native

#include "@DIR/native.m"

fn C.native_value(&int) int

pub fn value() int {
	n := 90
	return C.native_value(&n)
}
')
	write_module_cache_file(root, 'native/native.m', 'int native_value(const int* value) {
	return *value;
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import native

fn main() {
	println(native.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '90'
	assert module_cache_artifact(cache_dir, 'native_', '.o').len > 0

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '90'
}

fn test_cached_unused_function_handles_plain_multi_discard_assignment() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_multi_discard_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'cached/cached.v', 'module cached

fn unused(key string, value string) {
	_, _ = key, value
}

pub fn value() int {
	return 42
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import cached

fn main() {
	println(cached.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '42'
	assert module_cache_artifact(cache_dir, 'cached_', '.o').len > 0

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
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

fn module_cache_string_symbol(value string) string {
	mut hash := u64(1469598103934665603)
	for c in value.bytes() {
		hash = (hash ^ u64(c)) * u64(1099511628211)
	}
	return '_v3_lit_${value.len}_${hash.hex()}'
}

fn test_module_cache_materializes_cached_body_string_definitions() {
	stable_symbol := module_cache_string_symbol('stable')
	before_symbol := module_cache_string_symbol('before')
	after_symbol := module_cache_string_symbol('after')
	stable_definition := 'static string ${stable_symbol} = {"stable", 6, 1};'
	before_definition := 'static string ${before_symbol} = {"before", 6, 1};'
	after_definition := 'static string ${after_symbol} = {"after", 5, 1};'
	cached := '// V3CACHE_BASELINE ${stable_definition}\n// V3CACHE_BASELINE ${before_definition}\n/* V3CACHE_BODY_BEGIN */\nconsume(${stable_symbol});\nconsume(${before_symbol});\n'
	materialized := modulecache.materialize_cached_body_string_definitions(cached)
	assert materialized.contains(stable_definition)
	assert materialized.contains(before_definition)
	assert !materialized.contains('// V3CACHE_BASELINE')
	with_actual := modulecache.materialize_cached_body_string_definitions(before_definition + '\n' +
		cached)
	assert with_actual.count(before_definition) == 1
	rewritten := modulecache.rewrite_cached_runtime_strings(materialized, ['stable', 'before'], [
		'stable',
		'after',
	]) or { panic('runtime string rewrite failed') }
	assert rewritten.contains(stable_definition)
	assert rewritten.contains(after_definition)
	assert !rewritten.contains(before_definition)
}

fn test_module_cache_string_symbol_rewrite_handles_swapped_literals() {
	alpha_symbol := module_cache_string_symbol('alpha')
	beta_symbol := module_cache_string_symbol('beta')
	alpha_definition := 'static string ${alpha_symbol} = {"alpha", 5, 1};'
	beta_definition := 'static string ${beta_symbol} = {"beta", 4, 1};'
	source := '${alpha_definition}\n${beta_definition}\n/* V3CACHE_BODY_BEGIN */\nconsume(${alpha_symbol});\nconsume(${beta_symbol});\n'
	rewritten := modulecache.rewrite_cached_runtime_strings(source, ['alpha', 'beta'], [
		'beta',
		'alpha',
	]) or { panic('runtime string rewrite failed') }
	body := rewritten.all_after('/* V3CACHE_BODY_BEGIN */')
	assert body.contains('consume(${beta_symbol});\nconsume(${alpha_symbol});')
	assert rewritten.count(alpha_definition) == 1
	assert rewritten.count(beta_definition) == 1
}

fn test_module_cache_string_symbol_rewrite_skips_c_string_contents() {
	old_value := 'before'
	new_value := 'after'
	old_symbol := module_cache_string_symbol(old_value)
	new_symbol := module_cache_string_symbol(new_value)
	unchanged_value := old_symbol
	unchanged_symbol := module_cache_string_symbol(unchanged_value)
	old_definition := 'static string ${old_symbol} = {"${old_value}", ${old_value.len}, 1};'
	unchanged_definition := 'static string ${unchanged_symbol} = {"${unchanged_value}", ${unchanged_value.len}, 1};'
	source := '${old_definition}\n${unchanged_definition}\n/* ${old_symbol} */\nint ${old_symbol}_suffix;\n/* V3CACHE_BODY_BEGIN */\nconsume(${old_symbol});\nconsume(${unchanged_symbol});\n'
	rewritten := modulecache.rewrite_cached_runtime_strings(source, [old_value, unchanged_value], [
		new_value,
		unchanged_value,
	]) or { panic('runtime string rewrite failed') }
	body := rewritten.all_after('/* V3CACHE_BODY_BEGIN */')
	assert body.contains('consume(${new_symbol});\nconsume(${unchanged_symbol});')
	assert rewritten.contains(unchanged_definition)
	assert rewritten.contains('/* ${old_symbol} */')
	assert rewritten.contains('int ${old_symbol}_suffix;')
}

fn test_module_cache_string_symbol_rewrite_rejects_partially_changed_duplicates() {
	x_symbol := module_cache_string_symbol('x')
	x_definition := 'static string ${x_symbol} = {"x", 1, 1};'
	source := '${x_definition}\n/* V3CACHE_BODY_BEGIN */\nconsume(${x_symbol});\nconsume(${x_symbol});\n'
	if rewritten := modulecache.rewrite_cached_runtime_strings(source, ['x', 'x'], [
		'y',
		'x',
	])
	{
		assert false, rewritten
	}
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

fn test_cgen_cache_tracks_pkgconfig_output() {
	$if windows {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_pkgconfig_cache_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	old_path := os.getenv('PATH')
	old_flags := os.getenv_opt('V3_TEST_PKGCONFIG_FLAGS')
	defer {
		os.setenv('PATH', old_path, true)
		if value := old_flags {
			os.setenv('V3_TEST_PKGCONFIG_FLAGS', value, true)
		} else {
			os.unsetenv('V3_TEST_PKGCONFIG_FLAGS')
		}
		os.rmdir_all(root) or {}
	}
	pkgconfig := os.join_path(root, 'pkg-config')
	write_module_cache_file(root, 'pkg-config', '#!/bin/sh
printf "%s\\n" "\$V3_TEST_PKGCONFIG_FLAGS"
')
	os.chmod(pkgconfig, 0o700) or { panic(err) }
	os.setenv('PATH', '${root}${os.path_delimiter}${old_path}', true)
	write_module_cache_file(root, 'value.c', '#ifndef V3_PKG_VALUE
#error V3_PKG_VALUE is required
#endif
int v3_pkg_value(void) { return V3_PKG_VALUE; }
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

#pkgconfig v3-cache-test
#insert "@DIR/value.c"

fn C.v3_pkg_value() int

fn main() {
	println(C.v3_pkg_value())
}
')
	cache_dir := os.join_path(root, 'cache')
	os.setenv('V3_TEST_PKGCONFIG_FLAGS', '-DV3_PKG_VALUE=41', true)
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41'
	warm_output := os.join_path(root, 'warm')
	warm :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(warm_output)} ${os.quoted_path(main_file)}')
	assert warm.exit_code == 0, warm.output
	assert warm.output.contains('cgen (cached)'), warm.output
	assert run_module_cache_binary(warm_output) == '41'

	os.setenv('V3_TEST_PKGCONFIG_FLAGS', '-DV3_PKG_VALUE=42', true)
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (cached)'), second.output
	assert run_module_cache_binary(second_output) == '42'
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

fn test_whole_program_cgen_invalidates_when_main_external_input_changes() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_main_external_input_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'payload.txt', 'abc')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

fn main() {
	println($embed_file("payload.txt").len)
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '3'

	write_module_cache_file(root, 'payload.txt', 'abcdef')
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (cached)'), second.output
	assert run_module_cache_binary(second_output) == '6'
}

fn test_whole_program_cgen_invalidates_when_implicit_main_external_input_changes() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(),
		'v3_module_cache_implicit_main_external_input_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'payload.txt', 'abc')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'println($embed_file("payload.txt").len)\n')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '3'

	warm_output := os.join_path(root, 'warm')
	warm :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(warm_output)} ${os.quoted_path(main_file)}')
	assert warm.exit_code == 0, warm.output
	assert warm.output.contains('cgen (cached)'), warm.output
	assert run_module_cache_binary(warm_output) == '3'

	write_module_cache_file(root, 'payload.txt', 'abcdef')
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (cached)'), second.output
	assert run_module_cache_binary(second_output) == '6'
}

fn test_whole_program_cgen_invalidates_when_forced_include_changes() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_forced_include_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'forced.h')
	write_module_cache_file(root, 'forced.h', 'static inline int v3_forced_value(void) {
	return 1;
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

fn C.v3_forced_value() int

fn main() {
	println(C.v3_forced_value())
}
')
	cache_dir := os.join_path(root, 'cache')
	c_flags := '-include ${header}'
	first_output := os.join_path(root, 'first')
	first :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -cflags ${os.quoted_path(c_flags)} -o ${os.quoted_path(first_output)} ${os.quoted_path(main_file)}')
	assert first.exit_code == 0, first.output
	assert run_module_cache_binary(first_output) == '1'

	write_module_cache_file(root, 'forced.h', 'static inline int v3_forced_value(void) {
	return 2;
}
')
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -cflags ${os.quoted_path(c_flags)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (cached)'), second.output
	assert run_module_cache_binary(second_output) == '2'
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
	mut link_lines := []string{}
	$if macos {
		link_lines = result.output.split_into_lines().filter(it.contains('tcc.exe'))
	} $else {
		link_lines = result.output.split_into_lines().filter(it.contains('> cc '))
	}
	assert link_lines.len > 0, result.output
	link_line := link_lines.last()
	mut object_pos := -1
	$if macos {
		object_pos = link_line.index('.dylib') or { -1 }
	} $else {
		object_pos = link_line.index(cache_dir) or { -1 }
	}
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
	assert second_hashes.keys().filter(it.starts_with('builtin_')).len == 1, second_hashes.keys().str()
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
	write_module_cache_file(root, 'outer/outer.v', 'module outer

import wrapper

pub fn value() int {
	return wrapper.value()
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import outer

fn main() {
	println(outer.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '42'
	wrapper_header_path := module_cache_artifact(cache_dir, 'wrapper_', '.vh')
	assert wrapper_header_path.len > 0
	assert !modulecache.header_needs_source(modulecache.Entry{
		header: wrapper_header_path
	})
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_module_body_recreates_generic_receiver_specializations() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_generic_receiver_chain_${os.getpid()}')
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
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

import genericbox

pub fn value() int {
	return genericbox.Box[int]{
		value: 45
	}.get()
}
')
	write_module_cache_file(root, 'outer/outer.v', 'module outer

import wrapper

pub fn value() int {
	return wrapper.value()
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import outer

fn main() {
	println(outer.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '45'
	wrapper_header_path := module_cache_artifact(cache_dir, 'wrapper_', '.vh')
	assert wrapper_header_path.len > 0
	wrapper_header := os.read_file(wrapper_header_path) or { panic(err) }
	assert wrapper_header.contains('pub fn value() int {'), wrapper_header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '45'
}

fn test_cached_module_body_recreates_captured_callback_symbols() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_captured_callback_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'callbacks/callbacks.v', 'module callbacks

pub fn value(input int) int {
	offset := 3
	add := fn [offset] (value int) int {
		return offset + value
	}
	return add(input)
}
')
	write_module_cache_file(root, 'outer/outer.v', 'module outer

import callbacks

pub fn value() int {
	return callbacks.value(43)
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import outer

fn main() {
	println(outer.value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '46'
	callback_header_path := module_cache_artifact(cache_dir, 'callbacks_', '.vh')
	assert callback_header_path.len > 0
	callback_header := os.read_file(callback_header_path) or { panic(err) }
	assert callback_header.contains('pub fn value(input int) int {'), callback_header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '46'
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
	assert !modulecache.header_needs_source(modulecache.Entry{
		header: header_path
	})
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('pub fn (box Box[T]) get() T {'), header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_generic_body_resolves_relative_embed_file() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_generic_embed_file_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	payload_path := os.join_path(root, 'assets/payload.txt')
	write_module_cache_file(root, 'assets/payload.txt', 'cached payload')
	write_module_cache_file(root, 'assets/assets.v', 'module assets

pub fn payload_len[T]() int {
	_ = sizeof(T)
	marker := r"trailing\\"
	_ = marker
	return $embed_file("payload.txt").len
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import assets

fn main() {
	println(assets.payload_len[int]())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '14'
	header_path := module_cache_artifact(cache_dir, 'assets_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains(os.real_path(payload_path)), header
	assert !header.contains('$embed_file("payload.txt")'), header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '14'
}

fn test_cached_generic_body_resolves_escaped_relative_embed_file() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_generic_escaped_embed_file_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	payload_path := os.join_path(root, "assets/it's.txt")
	write_module_cache_file(root, "assets/it's.txt", 'cached payload')
	embed_expr := r"$embed_file('it\'s.txt')"
	write_module_cache_file(root, 'assets/assets.v', 'module assets

pub fn payload_len[T]() int {
	_ = sizeof(T)
	return ${embed_expr}.len
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import assets

fn main() {
	println(assets.payload_len[int]())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '14'
	first_hashes := module_cache_object_hashes(cache_dir)
	header_path := module_cache_artifact(cache_dir, 'assets_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	escaped_payload_path := os.real_path(payload_path).replace("'", "\\'")
	assert header.contains(escaped_payload_path), header
	assert !header.contains(embed_expr), header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '14'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_comptime_body_resolves_relative_insert() {
	$if windows {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_comptime_insert_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	header_path := os.join_path(root, 'wrapper/api.h')
	write_module_cache_file(root, 'wrapper/api.h', 'static inline int cached_comptime_value(void) {
	return 42;
}
')
	write_module_cache_file(root, 'wrapper/wrapper.v', 'module wrapper

$if !windows {
	#insert "@DIR/api.h"
	fn C.cached_comptime_value() int
}

pub fn value() int {
	$if !windows {
		return C.cached_comptime_value()
	} $else {
		return 0
	}
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
	cache_header_path := module_cache_artifact(cache_dir, 'wrapper_', '.vh')
	assert cache_header_path.len > 0
	cache_header := os.read_file(cache_header_path) or { panic(err) }
	assert cache_header.contains(os.real_path(header_path)), cache_header
	assert !cache_header.contains('@DIR/api.h'), cache_header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '42'
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

fn test_cached_interface_implementer_with_embedded_body_has_forward_declaration() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_interface_embedded_body_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'contract/contract.v', 'module contract

pub interface Writer {
	write(value int) int
}
')
	write_module_cache_file(root, 'writer/writer.v', 'module writer

import contract

pub struct NumberWriter {}

fn identity[T](value T) T {
	return value
}

pub fn (mut writer NumberWriter) write(value int) int {
	_ = writer
	return identity[int](value)
}

pub fn make() contract.Writer {
	return &NumberWriter{}
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import writer

fn main() {
	mut output := writer.make()
	println(output.write(44))
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '44'
	header_path := module_cache_artifact(cache_dir, 'writer_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('fn (mut writer NumberWriter) write(value int) int {'), header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '44'
}

fn test_cached_header_preserves_comptime_struct_fields() {
	mut a := flat.FlatAst.new()
	module_id := a.add_node(flat.Node{
		kind:  .module_decl
		value: 'config'
	})
	direct_field := a.add_node(flat.Node{
		kind:    .field_decl
		value:   'value'
		typ:     'int'
		payload: flat.node_payload(['p'])
	})
	comptime_field := a.add_node(flat.Node{
		kind:    .field_decl
		value:   'enabled'
		typ:     'bool'
		payload: flat.node_payload(['p'])
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

fn test_cached_struct_default_with_unsupported_initializer_is_embedded() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_struct_default_index_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'defaulted/defaulted.v', 'module defaulted

const values = [41, 42]

@[params]
pub struct Config {
pub:
	first int = values[0]
}

pub fn cached_value() int {
	return Config{}.first
}

pub fn configured(config Config) int {
	return config.first
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import defaulted

fn main() {
	println(defaulted.Config{}.first)
	println(defaulted.cached_value())
	println(defaulted.configured())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41\n41\n41'
	header_path := module_cache_artifact(cache_dir, 'defaulted_', '.vh')
	assert header_path.len > 0
	assert !modulecache.header_needs_source(modulecache.Entry{
		header: header_path
	})
	header := os.read_file(header_path) or { panic(err) }
	assert header.count('pub struct Config {') == 1, header
	assert header.contains('@[params]\npub struct Config {'), header
	assert header.contains('values = [41, 42].clone()'), header
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '41\n41\n41'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_const_preserves_positional_struct_initializers() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_positional_struct_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'records/records.v', 'module records

pub struct Pair {
pub:
	left  int
	right int
}

const pairs = [Pair{41, 42}]!

pub fn first() int {
	return pairs[0].left
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import records

fn main() {
	println(records.first())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41'
	header_path := module_cache_artifact(cache_dir, 'records_', '.vh')
	assert header_path.len > 0
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('Pair{41, 42}]!'), header
	assert !header.contains('Pair{:'), header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '41'
}

fn test_cached_const_preserves_escaped_char_literals() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_module_cache_escaped_chars_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'escaped/escaped.v', 'module escaped

pub const chars = [`\\\\`, `\\n`, `\\t`, `*`]

pub fn backslash() rune {
	return chars[0]
}

pub fn newline() rune {
	return chars[1]
}

pub fn tab() rune {
	return chars[2]
}

pub fn star() rune {
	return chars[3]
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import escaped

fn main() {
	println(int(escaped.backslash()))
	println(int(escaped.newline()))
	println(int(escaped.tab()))
	println(int(escaped.star()))
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '92\n10\n9\n42'
	first_hashes := module_cache_object_hashes(cache_dir)

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '92\n10\n9\n42'
	assert changed_module_cache_objects(first_hashes, module_cache_object_hashes(cache_dir)).len == 0
}

fn test_cached_const_with_unsupported_initializer_is_embedded() {
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
	assert !modulecache.header_needs_source(modulecache.Entry{
		header: header_path
	})
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('pub const values = [41, 42].clone()'), header
	assert header.contains('pub const first = values[0]'), header

	second_output := os.join_path(root, 'second')
	compile_module_cache_project(v3_bin, cache_dir, main_file, second_output)
	assert run_module_cache_binary(second_output) == '41\n41'
}

fn test_cached_global_with_unsupported_initializer_is_embedded() {
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

pub union Holder {
pub mut:
	value int
}

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
	assert !modulecache.header_needs_source(modulecache.Entry{
		header: header_path
	})
	header := os.read_file(header_path) or { panic(err) }
	assert header.contains('pub union Holder {'), header
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

@[markused; params]
pub struct ExitConfig {
pub:
	code int
}

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
	assert header.contains('@[noreturn]\npub fn die()'), header
	assert header.contains('@[markused; params]\npub struct ExitConfig {'), header
	assert !header.contains('@[markused; params]\n@[params]\npub struct ExitConfig {'), header
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
	assert foo_header.contains('pub const answer = 21')
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
	cached_output := os.join_path(root, 'cached_out')
	cached :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(cached_output)} ${os.quoted_path(main_file)}')
	assert cached.exit_code == 0, cached.output
	assert cached.output.contains('monomorphize (cached)'), cached.output
	assert cached.output.contains('cgen (cached)'), cached.output
	assert run_module_cache_binary(cached_output) == '1\n2\ntrue\nfalse\nfalse\ntrue'
}

fn test_generic_program_cache_rehydrates_body_string_literals() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_generic_body_strings_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', "module main

fn identity[T](value T) T {
\treturn value
}

fn main() {
\tprintln(identity('seed'))
\tprintln('initial')
}
")
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == 'seed\ninitial'

	write_module_cache_file(root, 'main.v', "module main

fn identity[T](value T) T {
\treturn value
}

fn main() {
\tprintln(identity('stable'))
\tprintln('before')
}
")
	baseline_output := os.join_path(root, 'baseline')
	compile_module_cache_project(v3_bin, cache_dir, main_file, baseline_output)
	assert run_module_cache_binary(baseline_output) == 'stable\nbefore'

	write_module_cache_file(root, 'main.v', "module main

fn identity[T](value T) T {
\treturn value
}

fn main() {
\tprintln(identity('stable'))
\tprintln('before')
}

// A source-only edit must retain both cached literal definitions.
")
	comment_output := os.join_path(root, 'comment')
	comment :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(comment_output)} ${os.quoted_path(main_file)}')
	assert comment.exit_code == 0, comment.output
	assert comment.output.contains('cgen (cached)'), comment.output
	assert run_module_cache_binary(comment_output) == 'stable\nbefore'

	write_module_cache_file(root, 'main.v', "module main

fn identity[T](value T) T {
\treturn value
}

fn main() {
\tprintln(identity('stable'))
\tprintln('after')
}
")
	changed_output := os.join_path(root, 'changed')
	changed :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(changed_output)} ${os.quoted_path(main_file)}')
	assert changed.exit_code == 0, changed.output
	assert run_module_cache_binary(changed_output) == 'stable\nafter'
}

fn test_incremental_program_cache_recompiles_real_main_logic() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_program_logic_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', "module main

import os

fn identity[T](value T) T {
\treturn value
}

fn message(value string) string {
\treturn identity(value)
}

fn main() {
\tprintln(message('before'))
}
")
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == 'before'

	write_module_cache_file(root, 'main.v', "module main

import os

fn identity[T](value T) T {
\treturn value
}

fn message(value string) string {
\treturn identity(value)
}

fn main() {
\tprintln(message('baseline'))
}
")
	baseline_output := os.join_path(root, 'baseline')
	compile_module_cache_project(v3_bin, cache_dir, main_file, baseline_output)
	assert run_module_cache_binary(baseline_output) == 'baseline'

	write_module_cache_file(root, 'main.v', "module main

import os

fn identity[T](value T) T {
\treturn value
}

fn message(value string) string {
\treturn identity(value)
}

fn main() {
\targ_count := os.args.len
\t$if macos {
\t\t_ = arg_count
\t}
\tprintln(message('snapshot logic'))
}
")
	snapshot_output := os.join_path(root, 'snapshot')
	compile_module_cache_project(v3_bin, cache_dir, main_file, snapshot_output)
	assert run_module_cache_binary(snapshot_output) == 'snapshot logic'

	write_module_cache_file(root, 'main.v', "module main

import os

fn identity[T](value T) T {
\treturn value
}

fn message(value string) string {
\treturn identity(value)
}

fn main() {
\targ_count := os.args.len
\t$if macos {
\t\t_ = arg_count
\t}
\tif arg_count > 0 {
\t\tprintln(message('after real logic'))
\t}
}
")
	incremental_output := os.join_path(root, 'incremental')
	incremental :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(incremental_output)} ${os.quoted_path(main_file)}')
	assert incremental.exit_code == 0, incremental.output
	assert incremental.output.contains('check (incremental)'), incremental.output
	assert incremental.output.contains('cgen (incremental)'), incremental.output
	assert run_module_cache_binary(incremental_output) == 'after real logic'
}

fn test_incremental_program_cache_resolves_compile_signature_artifacts() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_compile_signature_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'cached/cached.v', 'module cached

pub fn value() int {
	return 40
}
')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import cached

fn adjusted_value() int {
	return cached.value() + 1
}

fn main() {
	println(adjusted_value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '41'
	// Warm once so the snapshot uses the stable cached-module source set.
	write_module_cache_file(root, 'main.v', 'module main

import cached

fn adjusted_value() int {
	return cached.value() + 2
}

fn main() {
	println(adjusted_value())
}
')
	baseline_output := os.join_path(root, 'baseline')
	compile_module_cache_project(v3_bin, cache_dir, main_file, baseline_output)
	assert run_module_cache_binary(baseline_output) == '42'

	// Populate strict module objects without replacing main.v's non-strict snapshot.
	strict_seed_file := os.join_path(root, 'strict_seed.v')
	write_module_cache_file(root, 'strict_seed.v', 'module main

import cached

fn main() {
	println(cached.value())
}
')
	strict_seed_output := os.join_path(root, 'strict_seed')
	strict_seed :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -strict -o ${os.quoted_path(strict_seed_output)} ${os.quoted_path(strict_seed_file)}')
	assert strict_seed.exit_code == 0, strict_seed.output
	assert run_module_cache_binary(strict_seed_output) == '40'

	write_module_cache_file(root, 'main.v', 'module main

import cached

fn adjusted_value() int {
	return cached.value() + 3
}

fn main() {
	println(adjusted_value())
}
')
	strict_output := os.join_path(root, 'strict')
	strict :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -strict -o ${os.quoted_path(strict_output)} ${os.quoted_path(main_file)}')
	assert strict.exit_code == 0, strict.output
	assert strict.output.contains('check (incremental)'), strict.output
	assert strict.output.contains('cgen (incremental)'), strict.output
	assert run_module_cache_binary(strict_output) == '43'
}

fn test_incremental_program_cache_emits_new_body_support_typedefs() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_body_support_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

fn add_one(value int) int {
	return value + 1
}

fn calculated_value() int {
	return add_one(0)
}

fn main() {
	println(calculated_value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '1'

	write_module_cache_file(root, 'main.v', 'module main

fn add_one(value int) int {
	return value + 1
}

fn calculated_value() int {
	return add_one(1)
}

fn main() {
	println(calculated_value())
}
')
	baseline_output := os.join_path(root, 'baseline')
	compile_module_cache_project(v3_bin, cache_dir, main_file, baseline_output)
	assert run_module_cache_binary(baseline_output) == '2'

	write_module_cache_file(root, 'main.v', 'module main

fn add_one(value int) int {
	return value + 1
}

fn calculated_value() int {
	values := [2]int{init: 40}
	return add_one(values[0] + 1)
}

fn main() {
	println(calculated_value())
}
')
	fixed_array_output := os.join_path(root, 'fixed_array')
	fixed_array :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(fixed_array_output)} ${os.quoted_path(main_file)}')
	assert fixed_array.exit_code == 0, fixed_array.output
	assert fixed_array.output.contains('cgen (incremental)'), fixed_array.output
	assert run_module_cache_binary(fixed_array_output) == '42'

	write_module_cache_file(root, 'main.v', 'module main

fn add_one(value int) int {
	return value + 1
}

fn calculated_value() int {
	values := [2]int{init: 41}
	callback := add_one
	return callback(values[0])
}

fn main() {
	println(calculated_value())
}
')
	fn_pointer_output := os.join_path(root, 'fn_pointer')
	fn_pointer :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(fn_pointer_output)} ${os.quoted_path(main_file)}')
	assert fn_pointer.exit_code == 0, fn_pointer.output
	assert fn_pointer.output.contains('cgen (incremental)'), fn_pointer.output
	assert run_module_cache_binary(fn_pointer_output) == '42'
}

fn test_incremental_program_cache_falls_back_for_new_generic_type_argument() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_generic_type_arg_${os.getpid()}')
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
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

import generic

fn value() int {
	return generic.identity[int](40)
}

fn main() {
	println(value())
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '40'

	write_module_cache_file(root, 'main.v', 'module main

import generic

fn value() int {
	return generic.identity[int](41)
}

fn main() {
	println(value())
}
')
	baseline_output := os.join_path(root, 'baseline')
	compile_module_cache_project(v3_bin, cache_dir, main_file, baseline_output)
	assert run_module_cache_binary(baseline_output) == '41'

	write_module_cache_file(root, 'main.v', 'module main

import generic

fn value() int {
	return int(generic.identity[i64](42))
}

fn main() {
	println(value())
}
')
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('check (incremental)'), second.output
	assert !second.output.contains('monomorphize (incremental)'), second.output
	assert !second.output.contains('cgen (incremental)'), second.output
	assert run_module_cache_binary(second_output) == '42'
}

fn test_incremental_program_cache_falls_back_for_newly_reachable_function() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_program_reachability_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', "module main

fn newly_reachable() string {
	return 'called'
}

fn main() {
	println('before')
}
")
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == 'before'

	write_module_cache_file(root, 'main.v', "module main

fn newly_reachable() string {
	return 'called'
}

fn main() {
	println(newly_reachable())
}
")
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (incremental)'), second.output
	assert run_module_cache_binary(second_output) == 'called'
}

fn test_incremental_program_cache_invalidates_for_top_level_statement_change() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_top_level_stmt_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'println(1)\n')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '1'

	write_module_cache_file(root, 'main.v', 'println(2)\n')
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (cached)'), second.output
	assert run_module_cache_binary(second_output) == '2'
}

fn test_incremental_program_cache_preserves_top_level_statement_order() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_top_level_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', "println('cold first')\nprintln('cold second')\n")
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == 'cold first\ncold second'

	write_module_cache_file(root, 'main.v', "println('first')\nprintln('second')\n")
	baseline_output := os.join_path(root, 'baseline')
	compile_module_cache_project(v3_bin, cache_dir, main_file, baseline_output)
	assert run_module_cache_binary(baseline_output) == 'first\nsecond'

	write_module_cache_file(root, 'main.v', "println('second')\nprintln('first')\n")
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (cached)'), second.output
	assert run_module_cache_binary(second_output) == 'second\nfirst'
}

fn test_incremental_program_cache_invalidates_for_function_attribute_change() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_function_attribute_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

@[noreturn]
fn stop() {
	exit(0)
}

fn value() int {
	stop()
}

fn main() {}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)

	write_module_cache_file(root, 'main.v', 'module main

fn stop() {
	exit(0)
}

fn value() int {
	stop()
}

fn main() {}
')
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code != 0, second.output
	assert second.output.contains('missing return'), second.output
	assert !second.output.contains('check (incremental)'), second.output
}

fn test_incremental_program_cache_invalidates_for_reflected_declaration_attribute_change() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_reflected_attribute_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

@[first]
struct Tagged {}

fn main() {
	$for attr in Tagged.attributes {
		println(attr.name)
	}
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == 'first'

	write_module_cache_file(root, 'main.v', 'module main

@[second]
struct Tagged {}

fn main() {
	$for attr in Tagged.attributes {
		println(attr.name)
	}
}
')
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('check (incremental)'), second.output
	assert run_module_cache_binary(second_output) == 'second'
}

fn test_incremental_program_cache_preserves_global_initializer_order() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_incremental_global_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

__global trace int
__global first = record(1)
__global second = record(2)

fn record(value int) int {
	trace = trace * 10 + value
	return value
}

fn main() {
	println(trace)
}
')
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '12'

	write_module_cache_file(root, 'main.v', 'module main

__global trace int
__global second = record(2)
__global first = record(1)

fn record(value int) int {
	trace = trace * 10 + value
	return value
}

fn main() {
	println(trace)
}
')
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (cached)'), second.output
	assert run_module_cache_binary(second_output) == '21'
}

fn test_cgen_cache_invalidates_for_resolved_dynamic_flag_change() {
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_dynamic_c_flag_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'old.c', 'int v3_dynamic_flag_value(void) { return 1; }\n')
	old_object := os.join_path(root, 'old.o')
	old_cc := os.execute('cc -c -o ${os.quoted_path(old_object)} ${os.quoted_path(os.join_path(root,
		'old.c'))}')
	assert old_cc.exit_code == 0, old_cc.output
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', "module main

#flag \$first_existing('@DIR/new.o', '@DIR/old.o')

fn C.v3_dynamic_flag_value() int

fn main() {
	println(C.v3_dynamic_flag_value())
}
")
	cache_dir := os.join_path(root, 'cache')
	first_output := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_output)
	assert run_module_cache_binary(first_output) == '1'

	write_module_cache_file(root, 'new.c', 'int v3_dynamic_flag_value(void) { return 2; }\n')
	new_object := os.join_path(root, 'new.o')
	new_cc := os.execute('cc -c -o ${os.quoted_path(new_object)} ${os.quoted_path(os.join_path(root,
		'new.c'))}')
	assert new_cc.exit_code == 0, new_cc.output
	second_output := os.join_path(root, 'second')
	second :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(second_output)} ${os.quoted_path(main_file)}')
	assert second.exit_code == 0, second.output
	assert !second.output.contains('cgen (cached)'), second.output
	assert run_module_cache_binary(second_output) == '2'
}

fn test_cached_dev_tcc_bypasses_native_source_link_flags() {
	$if !macos {
		return
	}
	v3_bin := build_module_cache_v3()
	root := os.join_path(os.temp_dir(), 'v3_cached_dev_native_link_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_module_cache_file(root, 'v.mod', "Module { name: 'cached_dev_native_link' }\n")
	write_module_cache_file(root, 'native.m',
		'int v3_cached_dev_native_value(void) { return 73; }\n')
	main_file := os.join_path(root, 'main.v')
	write_module_cache_file(root, 'main.v', 'module main

#flag @VMODROOT/native.m

fn C.v3_cached_dev_native_value() int

fn main() {
	println(C.v3_cached_dev_native_value())
}
')
	cache_dir := os.join_path(root, 'cache')
	output := os.join_path(root, 'output')
	compile :=
		os.execute('V3CACHE=${os.quoted_path(cache_dir)} ${os.quoted_path(v3_bin)} -o ${os.quoted_path(output)} ${os.quoted_path(main_file)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('tcc.exe'), compile.output
	assert run_module_cache_binary(output) == '73'
}
