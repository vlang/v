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
