import os
import v3.modulecache

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

fn test_module_cache_declaration_header_preserves_preprocessor_after_comment() {
	prefix := '/* embedded header */\n#ifndef CACHED_HEADER\n#define CACHED_HEADER\ntypedef struct Cached { int value; } Cached;\n#endif\n'
	header := modulecache.declaration_header(prefix)
	assert header.contains('/* embedded header */\n#ifndef CACHED_HEADER')
	assert !header.contains('extern #ifndef')
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
	println(genericmod.identity[int](17))
	println(genericmod.internal_value())
	println(sumcache.equal_ints())
}
')

	cache_dir := os.join_path(root, 'cache')
	first_bin := os.join_path(root, 'first')
	compile_module_cache_project(v3_bin, cache_dir, main_file, first_bin)
	assert run_module_cache_binary(first_bin) == '42\n17\n23\ntrue'

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
	assert run_module_cache_binary(second_bin) == '44\n17\n23\ntrue'
	second_hashes := module_cache_object_hashes(cache_dir)
	changed_after_foo := changed_module_cache_objects(first_hashes, second_hashes)
	assert changed_after_foo.len == 1, changed_after_foo.str()
	assert changed_after_foo[0].starts_with('foo_'), changed_after_foo.str()

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
	assert changed_module_cache_objects(second_hashes, alternate_hashes).len == 0

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
