import os

const vexe = @VEXE

// Regression test for https://github.com/vlang/v/issues/27330
//
// With -usecache, modules like `builtin` are compiled separately in
// build_module mode, where the interface type-table index is emitted as
// `extern const u32 ..._index;` and referenced. The main program must therefore
// provide a real, externally-linked `const u32 ..._index = N;` definition - a
// compile-time `enum` constant has no linker symbol, so the reference would be
// undefined at link time (e.g. `undefined symbol: _IError_None___index` on
// FreeBSD/clang).
fn test_usecache_interface_index_is_real_symbol() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27330')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'issue_27330.v')
	os.write_file(source_path, "fn main() {\n\tprintln('hello world')\n}\n") or { panic(err) }

	// -o - dumps the generated C of the main program to stdout.
	res := os.execute('${os.quoted_path(vexe)} -usecache -o - ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
	// The index must be a real (externally-linked) definition, not a bare enum.
	// The separate enum keeps an integer constant expression available for C
	// contexts like switch case labels.
	assert res.output.contains('enum { _IError_None___index_enum =')
	assert res.output.contains('const u32 _IError_None___index = _IError_None___index_enum;')
	assert !res.output.contains('enum { _IError_None___index =')

	// Sanity check: without -usecache the compile-time enum form is kept (it is
	// the tcc-friendly form and needs no external symbol in a single build).
	res2 := os.execute('${os.quoted_path(vexe)} -o - ${os.quoted_path(source_path)}')
	if res2.exit_code != 0 {
		panic(res2.output)
	}
	assert res2.output.contains('enum { _IError_None___index =')
}

fn test_usecache_shared_interface_lock_uses_enum_index_in_case_labels() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27330_shared')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'shared_interface.v')
	os.write_file(source_path, '
interface MyInterface {
	foo() string
}

struct MyStruct {
pub mut:
	fooer shared MyInterface
}

struct MyImplementor {
mut:
	num int
}

fn (m MyImplementor) foo() string {
	return "Hello World!"
}

fn main() {
	shared imp := MyImplementor{
		num: 1
	}
	s := MyStruct{
		fooer: imp
	}
	lock s.fooer {
		println(s.fooer.foo())
	}
}
') or {
		panic(err)
	}

	res := os.execute('${os.quoted_path(vexe)} -usecache -o - ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('enum { _main__MyInterface_main__MyImplementor_index_enum =')
	assert res.output.contains('const u32 _main__MyInterface_main__MyImplementor_index = _main__MyInterface_main__MyImplementor_index_enum;')
	assert res.output.contains('case _main__MyInterface_main__MyImplementor_index_enum:')
	assert !res.output.contains('case _main__MyInterface_main__MyImplementor_index:')
}
