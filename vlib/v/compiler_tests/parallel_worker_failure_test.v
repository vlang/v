import os
import strings
import v.cmdexec
import v.tempname

fn write_parallel_failure_source(path string) {
	mut src := strings.new_builder(320_000)
	src.writeln('module main')
	src.writeln('')
	src.writeln('struct SharedHost {}')
	// Reusing the same shared parameter name across independently checked
	// functions exposes accidental sharing of per-function checker maps.
	// Exceed mark-used's eager-precollection threshold as well as the checker's
	// parallel threshold, so failure injection reaches both worker pools.
	for i in 0 .. 3100 {
		src.writeln('fn (_host SharedHost) shared_helper_${i}(shared _value int) int {')
		src.writeln('\treturn ${i}')
		src.writeln('}')
	}
	for i in 0 .. 1050 {
		src.writeln('fn helper_${i}() int {')
		src.writeln('\treturn ${i}')
		src.writeln('}')
	}
	src.writeln('fn main() {')
	src.writeln('\tmut total := 0')
	for i in 0 .. 1050 {
		src.writeln('\ttotal += helper_${i}()')
	}
	src.writeln('\tprintln(int_str(total))')
	src.writeln('}')
	os.write_file(path, src.str()) or { panic(err) }
}

fn generate_parallel_failure_c(source string, stage string) string {
	out := os.join_path(os.dir(source), 'generated_${stage}.c')
	result := cmdexec.run_in_merged_with_env(@VEXE, ['-silent', source, '-o', out], '', {
		'VJOBS':                       '4'
		'V3_TEST_PTHREAD_CREATE_FAIL': if stage.len > 0 { '${stage}:all' } else { '' }
	})
	assert result.exit_code == 0, '${stage}: ${result.output}'
	return os.read_file(out) or { panic(err) }
}

// test_parallel_worker_launch_failures_fall_back_synchronously verifies that
// every compiler phase produces the same output when all helper launches fail.
fn test_parallel_worker_launch_failures_fall_back_synchronously() {
	root := os.join_path(os.vtmp_dir(), 'v3_parallel_failure_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	write_parallel_failure_source(source)
	baseline := generate_parallel_failure_c(source, '')
	for stage in ['checker', 'transform', 'markused', 'cgen'] {
		got := generate_parallel_failure_c(source, stage)
		assert got == baseline, '${stage} fallback changed generated C'
	}
}
