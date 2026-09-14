import os

// A builtin that builds a new collection is not a window onto the one it was called
// on. The alias analysis could not read those declarations -- they live in `builtin`
// -- and an unreadable body is otherwise assumed to pass every argument through, so
// an array built by `map` out of an immutable struct's field was reported as aliasing
// that field and writing to it was rejected.

const fresh_builtin_vexe = @VEXE
const fresh_builtin_tests_dir = os.dir(@FILE)
const fresh_builtin_v3_dir = os.dir(fresh_builtin_tests_dir)
const fresh_builtin_vlib_dir = os.dir(fresh_builtin_v3_dir)
const fresh_builtin_v3_src = os.join_path(fresh_builtin_v3_dir, 'v.v')

fn fresh_builtin_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fresh_builtin_compiler_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${fresh_builtin_vexe} -gc none -path "${fresh_builtin_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fresh_builtin_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn fresh_builtin_compile(v3_bin string, root string, source string) os.Result {
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	main_v := os.join_path(root, 'main.v')
	os.write_file(main_v, source) or { panic(err) }
	exe := os.join_path(root, 'prog')
	compile := os.execute('${v3_bin} -nocache ${main_v} -b c -o ${exe}')
	if compile.exit_code != 0 {
		return compile
	}
	return os.execute(exe)
}

fn test_an_array_built_by_a_builtin_is_not_an_alias_of_its_source() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Table {
	widths []int
}

fn mapped(t Table) []int {
	return t.widths.map(it + 0)
}

fn filtered(t Table) []int {
	return t.widths.filter(it > 0)
}

fn reversed(t Table) []int {
	return t.widths.reverse()
}

fn cloned(t Table) []int {
	return t.widths.clone()
}

fn main() {
	t := Table{ widths: [1, 2, 3] }
	mut a := mapped(t)
	a[0] = 9
	mut b := filtered(t)
	b[0] = 9
	mut c := reversed(t)
	c[0] = 9
	mut d := cloned(t)
	d[0] = 9
	println(t.widths)
	println(a[0])
	println(b[0])
	println(c[0])
	println(d[0])
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['[1, 2, 3]', '9', '9', '9',
		'9'], res.output
}

// The real thing is still caught: a function that hands back the field itself gives
// out a window onto it, and writing through that writes into the immutable value.
fn test_returning_the_field_itself_is_still_an_alias() {
	v3_bin := fresh_builtin_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_fresh_builtin_real_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := fresh_builtin_compile(v3_bin, root, 'struct Table {
	widths []int
}

fn borrowed(t Table) []int {
	if t.widths.len > 0 {
		return t.widths
	}
	return []int{}
}

fn main() {
	t := Table{ widths: [1, 2, 3] }
	mut a := borrowed(t)
	a[0] = 9
	println(t.widths)
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('aliases mutable data from an immutable value'), res.output
}
