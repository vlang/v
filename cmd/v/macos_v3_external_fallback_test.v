module main

import os

fn run_external_fallback_test_process(executable string, args []string, work_dir string, overrides map[string]string) os.Result {
	mut environment := os.environ()
	environment['VFLAGS'] = ''
	environment['VOSARGS'] = ''
	for name, value in overrides {
		environment[name] = value
	}
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_work_folder(work_dir)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	result := os.Result{
		exit_code: process.code
		output: output
	}
	process.close()
	return result
}

fn external_fallback_inline_asm_source() string {
	$if amd64 {
		return 'fn main() {\n\ta := i64(10)\n\tmut b := i64(0)\n\tasm amd64 {\n\t\tmov rax, a\n\t\tmov b, rax\n\t\t; +r (b)\n\t\t; r (a)\n\t\t; rax\n\t}\n\tprintln(b)\n}\n'
	} $else $if arm64 {
		return 'fn main() {\n\ta := 10\n\tmut b := 0\n\tasm arm64 {\n\t\tmov x0, a\n\t\tmov b, x0\n\t\t; +r (b)\n\t\t; r (a)\n\t\t; x0\n\t}\n\tprintln(b)\n}\n'
	} $else {
		return ''
	}
}

fn test_macos_v3_uses_external_v1_fallback_for_unsupported_programs() {
	$if macos || linux {
		source_text := external_fallback_inline_asm_source()
		if source_text == '' {
			return
		}
		vroot := os.dir(@VEXE)
		fallback := os.join_path(vroot, macos_v3_v1_fallback_binary)
		// Developer/compiler-only test builds do not necessarily come through make.
		// The make/bootstrap jobs exercise this path with the fallback present.
		if !os.is_executable(fallback) {
			return
		}
		root := os.join_path(os.vtmp_dir(), 'v3_external_v1_fallback_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, source_text)!
		output := os.join_path(root, 'main')
		result := run_external_fallback_test_process(@VEXE, ['-silent', '-o', output, source],
			vroot, {
			'V_MACOS_V3_NO_FALLBACK': ''
		})
		assert result.exit_code == 0, result.output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '10', run.output

		strict_output := os.join_path(root, 'strict')
		strict := run_external_fallback_test_process(@VEXE, ['-silent', '-o', strict_output,
			source], vroot, {
			'V_MACOS_V3_NO_FALLBACK': '1'
		})
		assert strict.exit_code != 0, strict.output
		assert !os.exists(strict_output)
	}
}
