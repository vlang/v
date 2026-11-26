import os

fn test_naked_assembly_owns_integer_pointer_and_stack_argument_returns() ! {
	$if !amd64 || !(linux || macos) {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'naked_asm_returns_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VEXEROOT, 'vlib/v/compiler_tests/testdata/naked_asm_returns/main.v')
	for prod in [false, true] {
		program := os.join_path(root, 'native_${prod}')
		mut command := [@VEXE, '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc', clang]
		if prod { command << '-prod' }
		command << ['-o', program, fixture]
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([program])
		assert ran.exit_code == 0 && ran.output.trim_space() == 'naked ABI returns passed', ran.output
	}
}

fn test_ordinary_functions_still_require_a_v_return_and_naked_returns_are_type_checked() ! {
	root := os.join_path(os.vtmp_dir(), 'naked_asm_diagnostics_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for name, source in {
		'ordinary':   'fn work() int {}
fn main() { println(work()) }
'
		'wrong_type': '@[_naked]
fn work() int { return "wrong" }
fn main() { println(work()) }
'
	} {
		path := os.join_path(root, '${name}.v')
		os.write_file(path, source)!
		checked := os.exec([@VEXE, '-check', path])
		assert checked.exit_code != 0, checked.output
		if name == 'ordinary' {
			assert checked.output.contains('missing return at end of function `work`'), checked.output
		} else {
			assert checked.output.contains('return') && checked.output.contains('string'), checked.output
		}
	}
}
