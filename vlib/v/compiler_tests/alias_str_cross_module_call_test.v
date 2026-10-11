// A local initialized from a call into another module took the return type as the
// declaring module spelled it: `ID` rather than `m.ID`. In `main` that bare name has
// no `str` method, so printing the local fell back to the generated `m.ID([0, ...])`
// even though `m.ID` declares its own `str`. Locals unwrapped from an `or` block and
// array elements already used the qualified name.
import os

const vexe = @VEXE

const alias_module = 'module m

pub type ID = [16]u8

pub type Count = int

pub type Names = []string

pub fn (u ID) str() string {
	return "id"
}

pub fn (c Count) str() string {
	return "count"
}

pub fn (n Names) str() string {
	return "names"
}

pub fn make_id() ID {
	return ID{}
}

pub fn make_id_or() !ID {
	return ID{}
}

pub fn make_count() Count {
	return 1
}

pub fn make_names() Names {
	return ["a"]
}
'

const alias_main = 'import m

fn main() {
	a := m.make_id()
	println(a)
	println("\${a}")
	println(a.str())
	b := m.make_id_or() or { panic(err) }
	println(b)
	ids := [m.make_id()]
	println(ids)
	c := m.make_count()
	println(c)
	println("\${c}")
	n := m.make_names()
	println(n)
	println("\${n}")
}
'

fn test_alias_str_method_is_used_for_a_local_from_another_module() {
	dir := os.join_path(os.vtmp_dir(), 'v3_alias_str_cross_module_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'm')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'm', 'm.v'), alias_module) or { panic(err) }
	src := os.join_path(dir, 'main.v')
	os.write_file(src, alias_main) or { panic(err) }
	exe := os.join_path(dir, 'main.exe')
	build := os.exec([vexe, '-new-compiler', '-o', exe, src])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['id', 'id', 'id', 'id', '[id]', 'count', 'count',
		'names', 'names']
}
