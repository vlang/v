// vtest build: !windows
import os

const vexe = @VEXE

// Regression test for https://github.com/vlang/v/issues/27330
//
// `-usecache` may compile a module separately from the program that uses it.
// The interface type-table index then has to survive as a real, externally
// linked symbol: a compile-time constant has no linker symbol, so the module's
// reference to it failed to link (`undefined symbol: _IError_None___index` on
// FreeBSD/clang). How the index is spelled in the generated C is a backend
// detail; that the program links and runs is not, so that is what is asserted.
fn build_and_run_with_usecache(name string, source string) string {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_usecache_${name}_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, '${name}.v')
	binary_path := os.join_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	build :=
		os.execute('${os.quoted_path(vexe)} -usecache -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}')
	assert build.exit_code == 0, build.output
	assert os.is_file(binary_path)
	run := os.execute(os.quoted_path(binary_path))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_usecache_program_links_and_runs() {
	output := build_and_run_with_usecache('issue_27330', "fn main() {\n\tprintln('hello world')\n}\n")
	assert output == 'hello world', output
}

fn test_usecache_shared_interface_lock_links_and_runs() {
	// The index of an implementor inside a `shared` interface is read both as a
	// value and as a `switch` case label. Both readings must resolve to the same
	// definition, or the program does not link.
	output := build_and_run_with_usecache('issue_27330_shared', 'interface MyInterface {
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
')
	assert output == 'Hello World!', output
}
