import os

const array_accessor_borrow_vexe = @VEXE
const array_accessor_borrow_tests_dir = os.dir(@FILE)
const array_accessor_borrow_v3_dir = os.dir(array_accessor_borrow_tests_dir)
const array_accessor_borrow_vlib_dir = os.dir(array_accessor_borrow_v3_dir)
const array_accessor_borrow_v3_src = os.join_path(array_accessor_borrow_v3_dir, 'v3.v')

fn tmp_array_accessor_borrow_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
}

fn build_v3_array_accessor_borrow() string {
	v3_bin := tmp_array_accessor_borrow_path('array_accessor_borrow')
	build :=
		os.execute('${os.quoted_path(array_accessor_borrow_vexe)} -gc none -path "${array_accessor_borrow_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(array_accessor_borrow_v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn assert_clean_borrow_build(compile os.Result) {
	assert compile.exit_code == 0, compile.output
	// The regressed transform lowered `arr.last().field` to an empty placeholder that
	// the C backend printed as `(0)`, so the borrow read reached cc as `(0).field`
	// ("member reference base type 'int' is not a structure or union"). In a directly
	// compiled module the ownership checker instead rejected the accessor outright.
	assert !compile.output.contains('unsupported node kind'), compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn run_v3_array_accessor_borrow_program(v3_bin string, name string, src string) string {
	src_path := '${tmp_array_accessor_borrow_path(name)}.v'
	bin_path := tmp_array_accessor_borrow_path('${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert_clean_borrow_build(compile)
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn run_v3_array_accessor_borrow_module(name string, module_src string, main_src string) string {
	proj_dir := tmp_array_accessor_borrow_path(name)
	mod_dir := os.join_path(proj_dir, 'mymod')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'mymod.v'), module_src) or { panic(err) }
	os.write_file(os.join_path(proj_dir, 'main.v'), main_src) or { panic(err) }
	bin_path := tmp_array_accessor_borrow_path('${name}_bin')
	v3_bin := build_v3_array_accessor_borrow()
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(proj_dir)} -b c -o ${os.quoted_path(bin_path)}')
	assert_clean_borrow_build(compile)
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// Reading a field of `arr.first()`/`arr.last()` is a borrow of the stored element,
// not an independent copy, so it must lower to an in-place `arr[..]` access even when
// the element type owns heap data and has no `clone()` method. This mirrors the HPACK
// dynamic table (`net.http` / `net.quic`) that `v new --web` pulls in via `veb`.
fn test_first_last_field_borrow_on_owned_elements() {
	v3_bin := build_v3_array_accessor_borrow()

	// The exact evict-loop shape: read the last element's scalar field, then remove it.
	last_field := run_v3_array_accessor_borrow_program(v3_bin, 'last_field',
		"struct Entry {\n\tname  string\n\tvalue string\n\tsize  int\n}\n\nstruct Tbl {\nmut:\n\tentries []Entry\n\ttotal   int\n}\n\nfn (mut t Tbl) shrink() {\n\tfor t.entries.len > 0 {\n\t\tt.total -= t.entries.last().size\n\t\tt.entries.delete_last()\n\t}\n}\n\nfn main() {\n\tmut t := Tbl{}\n\tt.entries << Entry{ name: 'a', value: 'b', size: 5 }\n\tt.entries << Entry{ name: 'c', value: 'd', size: 7 }\n\tt.total = 12\n\tt.shrink()\n\tprintln(int_str(t.total))\n}\n")
	assert last_field == '0'

	// first()/last() field reads through both value and ref receivers, incl. a chained
	// field access (`last().name.len`).
	mixed := run_v3_array_accessor_borrow_program(v3_bin, 'mixed_field',
		"struct Entry {\n\tname string\n\tn    int\n}\n\nstruct Box {\nmut:\n\tentries []Entry\n}\n\nfn (b &Box) probe() int {\n\treturn b.entries.first().n + b.entries.last().n + b.entries.last().name.len\n}\n\nfn main() {\n\tmut b := Box{}\n\tb.entries << Entry{ name: 'ab', n: 10 }\n\tb.entries << Entry{ name: 'cdef', n: 20 }\n\tprintln(int_str(b.probe()))\n}\n")
	assert mixed == '34'
}

// The same borrow read inside an imported module: the ownership checker does not
// diagnose non-user modules, so the regressed transform silently produced `(0)` there,
// which is precisely why `v .` on a fresh `--web` project failed to compile.
fn test_first_last_field_borrow_in_imported_module() {
	out := run_v3_array_accessor_borrow_module('borrow_mod',
		"module mymod\n\nstruct Entry {\n\tname  string\n\tvalue string\n\tsize  int\n}\n\npub struct Tbl {\nmut:\n\tentries []Entry\n\ttotal   int\n}\n\npub fn (mut t Tbl) add(name string, value string) {\n\tsz := name.len + value.len + 32\n\tfor t.entries.len > 0 && t.total + sz > 100 {\n\t\tt.total -= t.entries.last().size\n\t\tt.entries.delete_last()\n\t}\n\tt.entries.insert(0, Entry{ name: name, value: value, size: sz })\n\tt.total += sz\n}\n\npub fn (t &Tbl) total() int {\n\treturn t.total\n}\n",
		"module main\n\nimport mymod\n\nfn main() {\n\tmut t := mymod.Tbl{}\n\tt.add('a', 'b')\n\tt.add('c', 'd')\n\tprintln(int_str(t.total()))\n}\n")
	assert out == '68'
}
