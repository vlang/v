// vtest build: !windows
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

fn test_usecache_build_module_shared_interface_lock_uses_canonical_index_symbol() {
	root := os.join_path(os.vtmp_dir(), 'v_issue_27330_shared_module_${os.getpid()}')
	cache_dir := os.join_path(root, '.cache')
	vtmp_dir := os.join_path(root, '.vtmp')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'maker'))!
	os.mkdir_all(vtmp_dir)!
	os.write_file(os.join_path(root, 'v.mod'),
		"Module {\n\tname: 'v_issue_27330_shared_module'\n}\n") or { panic(err) }
	os.write_file(os.join_path(root, 'maker', 'maker.v'), '
module maker

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

pub fn exercise() {
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

	old_vcache := os.getenv_opt('VCACHE') or { '' }
	old_vtmp := os.getenv_opt('VTMP') or { '' }
	os.setenv('VCACHE', cache_dir, true)
	os.setenv('VTMP', vtmp_dir, true)
	defer {
		if old_vcache.len == 0 {
			os.unsetenv('VCACHE')
		} else {
			os.setenv('VCACHE', old_vcache, true)
		}
		if old_vtmp.len == 0 {
			os.unsetenv('VTMP')
		} else {
			os.setenv('VTMP', old_vtmp, true)
		}
	}
	mut p := os.new_process(vexe)
	p.set_work_folder(root)
	p.set_args(['-keepc', 'build-module', 'maker'])
	p.set_redirect_stdio()
	p.wait()
	stdout := p.stdout_slurp()
	stderr := p.stderr_slurp()
	exit_code := p.code
	p.close()
	assert exit_code == 0, '${stdout}${stderr}'
	generated_c_path := os.join_path(vtmp_dir, 'maker.tmp.c')
	assert os.exists(generated_c_path)
	generated_c := os.read_file(generated_c_path)!
	assert generated_c.contains('extern const u32 _maker__MyInterface_maker__MyImplementor_index;')
	assert generated_c.contains('if (x->val._typ == _maker__MyInterface_maker__MyImplementor_index) {')
	assert !generated_c.contains('enum { _maker__MyInterface_maker__MyImplementor_index_enum =')
	assert !generated_c.contains('case _maker__MyInterface_maker__MyImplementor_index_enum:')
}
