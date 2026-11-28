import os
import v.tempname

// The map header no longer carries a `len` field -- the count lives in
// `data->count` -- so C generation has to rewrite every `.len` on a map. It
// matched the bare map type only, and an *alias* of a map fell through to a
// plain field access, so the generated C did not compile:
// `error: no member named 'len' in 'struct map'`.

fn map_alias_len_build_and_run(name string, source string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'v3_map_alias_len_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	main_v := os.join_path(root, 'main.v')
	os.write_file(main_v, source) or { panic(err) }
	exe := os.join_path(root, 'prog')
	compile := os.exec([@VEXE, '-nocache', main_v, '-b', 'c', '-o', exe])
	if compile.exit_code != 0 {
		return compile
	}
	return os.exec([exe])
}

fn test_len_through_a_map_alias() {
	res := map_alias_len_build_and_run('direct', 'type Ints = []int
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
	res := map_alias_len_build_and_run('recursive', 'type Handlers = map[string]fn (Handlers)

fn main() {
	handlers := Handlers{}
	println(handlers.len)
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '0', res.output
}
