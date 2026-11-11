module amd64

import os

const amd64_test_compiler = os.join_path(os.vtmp_dir(), 'amd64_run_compiler_${os.getpid()}')

fn testsuite_end() {
	os.rm(amd64_test_compiler) or {}
}

fn amd64_test_compiler_path() string {
	if !os.exists(amd64_test_compiler) {
		result := os.exec([@VEXE, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend', 'amd64',
			'-o', amd64_test_compiler, os.join_path(@VEXEROOT, 'cmd', 'v')])
		assert result.exit_code == 0, result.output
	}
	return amd64_test_compiler
}

fn test_amd64_backend_compiles_and_runs_a_program() {
	$if linux && amd64 {
		compiler := amd64_test_compiler_path()
		root := os.join_path(os.vtmp_dir(), 'amd64_native_${os.getpid()}')
		os.mkdir_all(root)!
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(source, "fn main() {\n\tprintln('amd64 backend works')\n}\n")!
		compiled := os.exec([compiler, '-gc', 'none', '-b', 'amd64', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'amd64 backend works', ran.output
	}
}
