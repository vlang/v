import os
import v3.markused
import v3.parser
import v3.pref
import v3.types

const typed_receiver_vexe = @VEXE
const typed_receiver_tests_dir = os.dir(@FILE)
const typed_receiver_v3_dir = os.dir(typed_receiver_tests_dir)
const typed_receiver_vlib_dir = os.dir(typed_receiver_v3_dir)
const typed_receiver_v3_src = os.join_path(typed_receiver_v3_dir, 'v3.v')

fn typed_receiver_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_markused_typed_receiver_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${typed_receiver_vexe} -gc none -path "${typed_receiver_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${typed_receiver_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn typed_receiver_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_markused_typed_receiver_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'seriesmod')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'othermod')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'typed_receiver_markused' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'seriesmod/series.v'), 'module seriesmod

pub struct Series {
pub:
	factor int
}

pub struct LocalSeries {
pub:
	factor int
}

pub struct AlternateSeries {
pub:
	value int
}

pub const default_series = Series{
	factor: 4
}

const shadow_series = Series{
	factor: 9
}

pub fn run(x int) int {
	return default_series.measure(x)
}

pub fn local_shadow() int {
	shadow_series := LocalSeries{
		factor: 11
	}
	return shadow_series.leak(1)
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'seriesmod/methods.v'), 'module seriesmod

pub fn (s Series) measure(x int) int {
	return s.factor + x
}

fn (s Series) unused(x int) int {
	return s.factor - x
}

fn (s Series) leak(x int) int {
	return s.factor + x + 1000
}

fn (s AlternateSeries) measure(x int) int {
	return s.value + x + 2000
}

fn (s LocalSeries) leak(x int) int {
	return s.factor + x
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'othermod/other.v'), 'module othermod

pub struct Series {
pub:
	value int
}

pub fn touch() int {
	return 5
}

pub fn (s Series) measure(x int) int {
	return s.value + x + 100
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import othermod
import seriesmod

fn main() {
	assert seriesmod.run(3) == 7
	assert seriesmod.local_shadow() == 12
	assert othermod.touch() == 5
	println(int_str(seriesmod.run(1) + seriesmod.local_shadow() + othermod.touch()))
}
') or {
		panic(err)
	}
	return os.join_path(root, 'main.v')
}

fn typed_receiver_mark_used_project(name string, files map[string]string, main_file string) map[string]bool {
	root := os.join_path(os.temp_dir(), 'v3_markused_typed_receiver_used_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	mut paths := []string{}
	main_path := os.join_path(root, main_file)
	mut rel_paths := files.keys()
	rel_paths.sort()
	if main_file in files {
		os.mkdir_all(os.dir(main_path)) or { panic(err) }
		os.write_file(main_path, files[main_file]) or { panic(err) }
		paths << main_path
	}
	for rel in rel_paths {
		if rel == main_file {
			continue
		}
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, files[rel]) or { panic(err) }
		paths << path
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	for path in paths {
		tc.diagnostic_files[path] = true
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return markused.mark_used(a, tc)
}

fn test_builtin_map_receiver_methods_keep_plain_map_lookup() {
	used := typed_receiver_mark_used_project('builtin_map_methods', {
		'main.v': 'module main

fn main() {
	mut m := map[int]int{}
	m[1] = 1
	keys := m.keys()
	values := m.values()
	assert keys.len == 1
	assert values.len == 1
	m.clear()
	assert m.len == 0
}
'
	}, 'main.v')
	assert used['map.clear'], used.keys().str()
	assert !used['map[int]int.clear'], used.keys().str()
}

fn test_markused_used_map_does_not_traverse_unused_homonym_method_body() {
	used := typed_receiver_mark_used_project('homonym_secret', {
		'main.v':             'module main

import othermod
import seriesmod

fn main() {
	_ := seriesmod.run(3)
	_ := othermod.touch()
}
'
		'seriesmod/series.v': 'module seriesmod

pub struct Series {
pub:
	factor int
}

pub const default_series = Series{
	factor: 4
}

pub fn run(x int) int {
	return default_series.measure(x)
}

pub fn (s Series) measure(x int) int {
	return s.factor + x
}
'
		'othermod/other.v':   'module othermod

pub struct Series {
pub:
	value int
}

pub fn touch() int {
	return 5
}

struct Helper {}

fn (h Helper) secret() int {
	return 99
}

pub fn (s Series) measure(x int) int {
	return Helper{}.secret() + s.value + x
}
'
	}, 'main.v')
	assert used['seriesmod.Series.measure'] || used['seriesmod__Series__measure'], used.str()
	assert !used['Series.measure'], used.str()
	assert !used['othermod.Series.measure'], used.str()
	assert !used['othermod__Series__measure'], used.str()
	assert !used['othermod.secret'], used.str()
	assert !used['secret'], used.str()
	assert !used['othermod.Helper.secret'], used.str()
	assert !used['Helper.secret'], used.str()
	assert !used['othermod__Helper__secret'], used.str()
}

fn test_markused_prefers_scoped_node_type_for_block_receiver_locals() {
	used := typed_receiver_mark_used_project('block_receiver_local_type', {
		'main.v': 'module main

struct Outer {}
struct Inner {}
struct Unused {}

fn (o Outer) score() int {
	return 1
}

fn (i Inner) score() int {
	return 2
}

fn (u Unused) score() int {
	return 99
}

fn run() int {
	outer := Outer{}
	mut total := outer.score()
	if total > 0 {
		inner := Inner{}
		total += inner.score()
	}
	return total
}

fn main() {
	assert run() == 3
}
'
	}, 'main.v')
	assert used['Outer.score'] || used['main.Outer.score'] || used['main__Outer__score'], used.str()
	assert used['Inner.score'] || used['main.Inner.score'] || used['main__Inner__score'], used.str()
	assert !used['Unused.score'] && !used['main.Unused.score'] && !used['main__Unused__score'], used.str()
}

fn test_markused_roots_exact_typed_const_receiver_method() {
	v3_bin := typed_receiver_build_v3()
	main_path := typed_receiver_write_project()
	out := os.join_path(os.temp_dir(), 'v3_markused_typed_receiver_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '22'
	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('seriesmod__Series__measure'), generated
	assert generated.contains('seriesmod__LocalSeries__leak'), generated
	assert !generated.contains('seriesmod__Series__unused'), generated
	assert !generated.contains('seriesmod__Series__leak'), generated
	assert !generated.contains('seriesmod__AlternateSeries__measure'), generated
}
