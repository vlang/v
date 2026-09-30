import os

const copy_ownership_vexe = @VEXE
const copy_ownership_vroot = os.dir(os.dir(os.dir(os.dir(@FILE))))
const copy_ownership_tmp_dir = os.join_path(os.vtmp_dir(), 'copy_ownership_${os.getpid()}')
const copy_ownership_v3 = os.join_path(copy_ownership_tmp_dir, 'v3_ownership')

const copy_ownership_drop_decls = 'interface Drop {
mut:
	drop()
}

struct Res implements IClone, Drop {
	id int
}

fn (mut r Res) drop() {
	println("drop \${r.id}")
}

fn (r &Res) clone() Res {
	return Res{r.id + 100}
}
'

fn testsuite_begin() {
	os.mkdir_all(copy_ownership_tmp_dir) or { panic(err) }
	cmd_v := os.join_path(copy_ownership_vroot, 'cmd', 'v')
	vlib := os.join_path(copy_ownership_vroot, 'vlib')
	build := os.execute('${os.quoted_path(copy_ownership_vexe)} -gc none -d ownership -path "${vlib}|@vlib|@vmodules" -o ${os.quoted_path(copy_ownership_v3)} ${os.quoted_path(cmd_v)}')
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(copy_ownership_tmp_dir) or {}
}

fn copy_ownership_compile(name string, src string) os.Result {
	path := os.join_path(copy_ownership_tmp_dir, '${name}.v')
	os.write_file(path, src) or { panic(err) }
	out := os.join_path(copy_ownership_tmp_dir, name)
	return os.execute('${os.quoted_path(copy_ownership_v3)} -ownership -d ownership -no-parallel -o ${os.quoted_path(out)} ${os.quoted_path(path)}')
}

fn copy_ownership_run(name string, src string) string {
	build := copy_ownership_compile(name, src)
	assert build.exit_code == 0, build.output
	run := os.execute(os.quoted_path(os.join_path(copy_ownership_tmp_dir, name)))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_copy_clones_owned_elements_and_drops_replaced_ones() {
	output := copy_ownership_run('drops', copy_ownership_drop_decls + '
fn make() []Res {
	return [Res{7}, Res{8}]
}

fn main() {
	{
		mut dst := [Res{1}, Res{2}]
		src := [Res{10}, Res{20}, Res{30}]
		println("n=\${copy(mut dst, src)}")
		println("src \${src[2].id}, dst \${dst[0].id} \${dst[1].id}")
	}
	println("--- temporary")
	{
		mut dst := [Res{3}]
		println("n=\${copy(mut dst, make())}")
		println("dst \${dst[0].id}")
	}
	println("end")
}
')
	assert output.split_into_lines() == [
		'drop 1',
		'drop 2',
		'n=2',
		'src 30, dst 110 120',
		'drop 10',
		'drop 20',
		'drop 30',
		'drop 110',
		'drop 120',
		'--- temporary',
		'drop 3',
		'drop 7',
		'drop 8',
		'n=1',
		'dst 107',
		'drop 107',
		'end',
	]
}

fn test_copy_clones_strings_and_nested_arrays() {
	output := copy_ownership_run('strings', '
fn words() []string {
	return ["t".repeat(2), "u".repeat(2)]
}

fn main() {
	mut dst := ["a".repeat(3), "b".repeat(3)]
	src := ["x".repeat(3), "y".repeat(3), "z".repeat(3)]
	assert copy(mut dst, src) == 2
	assert dst == ["xxx", "yyy"]
	assert src == ["xxx", "yyy", "zzz"]
	assert copy(mut dst, words()) == 2
	assert dst == ["tt", "uu"]
	assert copy(mut dst[1..], src[2..]) == 1
	assert dst == ["tt", "zzz"]
	mut fixed := ["p".repeat(2), "q".repeat(2), "r".repeat(2)]!
	assert copy(mut fixed[1..], src) == 2
	assert fixed == ["pp", "xxx", "yyy"]!
	mut nested := [[1, 2], [3]]
	inner := [[7, 8, 9]]
	assert copy(mut nested, inner) == 1
	assert nested == [[7, 8, 9], [3]]
	assert inner == [[7, 8, 9]]
	println("ok")
}
')
	assert output == 'ok'
}

fn test_copy_rejects_owned_elements_without_clone() {
	build := copy_ownership_compile('uncloneable', 'interface Drop {
mut:
	drop()
}

struct Handle implements Drop {
	fd int
}

fn (mut h Handle) drop() {}

fn main() {
	mut dst := [Handle{1}]
	src := [Handle{2}]
	copy(mut dst, src)
}
')
	assert build.exit_code != 0
	assert build.output.contains('cannot copy `Handle` elements: `Handle` requires ownership destruction but has no compatible `clone()` method'), build.output
}

fn test_mut_fixed_array_views_preserve_independent_element_owners() {
	output := copy_ownership_run('fixed_array_owners', '@[has_globals]
module main

__global next_owned_id = 0
__global dropped_ids = map[int]bool{}

interface Drop {
mut:
	drop()
}

struct Tracked implements IClone, Drop {
	id int
}

fn fresh() Tracked {
	next_owned_id++
	return Tracked{next_owned_id}
}

fn (r &Tracked) clone() Tracked {
	return fresh()
}

fn (mut r Tracked) drop() {
	assert !dropped_ids[r.id], "owner dropped twice"
	dropped_ids[r.id] = true
}

fn keep(mut values []Tracked) []Tracked {
	return values
}

fn keep_strings(mut values []string) []string {
	return values
}

struct Wrapper {
	items []Tracked
}

fn keep_wrappers(mut values []Wrapper) []Wrapper {
	return values
}

fn main() {
	mut fixed := [fresh()]!
	kept := keep(mut fixed)
	drop_owned(kept)
	drop_owned(fixed)
	mut wrappers := [Wrapper{[fresh()]}]!
	kept_wrappers := keep_wrappers(mut wrappers)
	drop_owned(kept_wrappers)
	drop_owned(wrappers)
	assert dropped_ids.len == next_owned_id
	mut words := ["first".repeat(3), "second".repeat(3)]!
	kept_words := keep_strings(mut words)
	drop_owned(words)
	assert kept_words[0] == "firstfirstfirst"
	assert kept_words[1] == "secondsecondsecond"
	drop_owned(kept_words)
	println("ok")
}
')
	assert output == 'ok'
}

fn test_mut_fixed_array_views_reject_elements_without_clone() {
	for argument in ['values', 'values[0..1]'] {
		build := copy_ownership_compile('uncloneable_fixed_${argument.len}', 'interface Drop {
mut:
	drop()
}

struct Handle implements Drop {
	fd int
}

fn (mut h Handle) drop() {}

fn keep(mut values []Handle) []Handle {
	return values
}

fn main() {
	mut values := [Handle{1}]!
	keep(mut ${argument})
}
')
		assert build.exit_code != 0
		assert build.output.contains('requires ownership destruction but has no compatible `clone()` method'), build.output
	}
	build := copy_ownership_compile('uncloneable_fixed_reference', 'interface Drop {
mut:
	drop()
}

struct Handle implements Drop {
	fd int
}

fn (mut h Handle) drop() {}

fn inspect(values &[]Handle) {
	assert values[0].fd == 1
}

fn main() {
	values := [Handle{1}]!
	inspect(values)
}
')
	assert build.exit_code != 0
	assert build.output.contains('requires ownership destruction but has no compatible `clone()` method'), build.output
}

fn test_nonownership_fixed_array_views_do_not_root_unused_destructors() {
	path := os.join_path(copy_ownership_tmp_dir, 'unused_fixed_drop.c.v')
	os.write_file(path, 'fn C.unreachable_fixed_array_drop()

struct Storage {
	values []int
}

fn (mut s Storage) drop() {
	C.unreachable_fixed_array_drop()
}

fn keep(mut values []Storage) []Storage {
	return values
}

fn main() {
	mut values := [Storage{[1]}]!
	kept := keep(mut values)
	assert kept[0].values == [1]
	println("ok")
}
') or { panic(err) }
	output := os.execute('${os.quoted_path(copy_ownership_vexe)} run ${os.quoted_path(path)}')
	assert output.exit_code == 0, output.output
	assert output.output.trim_space() == 'ok'
}
