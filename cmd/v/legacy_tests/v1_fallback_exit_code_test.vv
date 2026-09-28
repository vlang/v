import os

const test_vexe = os.quoted_path(@VEXE)

// `v -old-compiler ...` hands the build to the separately built V1 compatibility
// compiler. On platforms with exec that replaces this process, so the compiler's
// status is the one the caller sees; Windows has no exec, and the CRT's
// `_execvp` used to start the child and then exit this process with status 0 -
// a failed compilation reported its error and still looked like a success.
fn run_old_compiler(src string, name string) os.Result {
	wrkdir := os.join_path(os.vtmp_dir(), 'v1_fallback_exit_code')
	os.mkdir_all(wrkdir) or { panic(err) }
	source_path := os.join_path(wrkdir, '${name}.v')
	os.write_file(source_path, src) or { panic(err) }
	defer {
		os.rm(source_path) or {}
	}
	output_path := os.join_path(wrkdir, '${name}.exe')
	defer {
		os.rm(output_path) or {}
	}
	return os.execute('${test_vexe} -old-compiler -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
}

fn test_a_failed_v1_fallback_build_reports_a_failing_exit_code() {
	res := run_old_compiler('fn main() {\n\tprintln(undefined_name_here)\n}\n', 'broken')
	assert res.exit_code != 0, 'a failing build must not report success: ${res.output}'
	assert res.output.contains('undefined_name_here'), res.output
}

fn test_a_successful_v1_fallback_build_reports_success() {
	res := run_old_compiler('fn main() {\n\tprintln(1 + 1)\n}\n', 'fine')
	assert res.exit_code == 0, res.output
}
