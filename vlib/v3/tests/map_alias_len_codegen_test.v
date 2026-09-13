import os

// The map header no longer carries a `len` field -- the count lives in
// `data->count` -- so C generation has to rewrite every `.len` on a map. It
// matched the bare map type only, and an *alias* of a map fell through to a
// plain field access, so the generated C did not compile:
// `error: no member named 'len' in 'struct map'`.

const map_alias_len_vexe = @VEXE
const map_alias_len_tests_dir = os.dir(@FILE)
const map_alias_len_v3_dir = os.dir(map_alias_len_tests_dir)
const map_alias_len_vlib_dir = os.dir(map_alias_len_v3_dir)
const map_alias_len_v3_src = os.join_path(map_alias_len_v3_dir, 'v3.v')

// The fixture has to be compiled by the V3 compiler built from this checkout.
// Building `vlib/v3/v3.v` is also what lets the suite's unit-test wrapper hand
// back its shared V3 binary; any other invocation is forwarded to the host
// compiler instead, which would not exercise this code path at all.
fn map_alias_len_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_map_alias_len_compiler_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${map_alias_len_vexe} -gc none -path "${map_alias_len_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${map_alias_len_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn map_alias_len_build_and_run(v3_bin string, root string, source string) os.Result {
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

fn test_len_through_a_map_alias() {
	v3_bin := map_alias_len_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_map_alias_len_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := map_alias_len_build_and_run(v3_bin, root, 'type Ints = []int
type Simple = map[string]int

fn take(m Simple) int {
	return m.len
}

fn main() {
	a := Ints([1, 2, 3])
	println(a.len)
	mut m := Simple{}
	m["x"] = 1
	m["y"] = 2
	println(m.len)
	println(take(m))
	pm := &m
	println(pm.len)
	s := "abc"
	println(s.len)
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['3', '2', '2', '2', '3'], res.output
}

// A map alias whose value type mentions the alias itself has to resolve through
// the same path without recursing forever.
fn test_len_through_a_recursive_map_alias() {
	v3_bin := map_alias_len_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_map_alias_len_rec_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := map_alias_len_build_and_run(v3_bin, root, 'type Handlers = map[string]fn (Handlers)

fn main() {
	handlers := Handlers{}
	println(handlers.len)
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '0', res.output
}
