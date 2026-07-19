import os
import rand
import strings

const pwf_tests_dir = os.dir(@FILE)
const pwf_v3_dir = os.dir(pwf_tests_dir)
const pwf_vlib_dir = os.dir(pwf_v3_dir)
const pwf_v3_src = os.join_path(pwf_v3_dir, 'v3.v')

fn build_parallel_failure_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_parallel_failure_${os.getpid()}_${rand.ulid()}')
	build :=
		os.execute('${os.quoted_path(@VEXE)} -gc none -path "${pwf_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(pwf_v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn write_parallel_failure_source() string {
	path := os.join_path(os.temp_dir(), 'v3_parallel_failure_${os.getpid()}_${rand.ulid()}.v')
	mut src := strings.new_builder(320_000)
	src.writeln('module main')
	src.writeln('')
	// Reusing the same shared parameter name across independently checked
	// functions exposes accidental sharing of per-function checker maps.
	// Exceed mark-used's eager-precollection threshold as well as the checker's
	// parallel threshold, so failure injection reaches both worker pools.
	for i in 0 .. 3100 {
		src.writeln('fn shared_helper_${i}(shared value int) int {')
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
	return path
}

fn generate_parallel_failure_c(v3_bin string, source string, stage string) string {
	out := os.join_path(os.temp_dir(),
		'v3_parallel_failure_${stage}_${os.getpid()}_${rand.ulid()}.c')
	env := if stage.len > 0 { 'V3_TEST_PTHREAD_CREATE_FAIL=${stage}:all ' } else { '' }
	result :=
		os.execute('${env}VJOBS=4 ${os.quoted_path(v3_bin)} ${os.quoted_path(source)} -o ${os.quoted_path(out)}')
	assert result.exit_code == 0, '${stage}: ${result.output}'
	return os.read_file(out) or { panic(err) }
}

// test_parallel_worker_launch_failures_fall_back_synchronously verifies that
// every compiler phase produces the same output when all helper launches fail.
fn test_parallel_worker_launch_failures_fall_back_synchronously() {
	$if !windows {
		v3_bin := build_parallel_failure_v3()
		source := write_parallel_failure_source()
		baseline := generate_parallel_failure_c(v3_bin, source, '')
		for stage in ['checker', 'transform', 'markused', 'cgen'] {
			got := generate_parallel_failure_c(v3_bin, source, stage)
			assert got == baseline, '${stage} fallback changed generated C'
		}
	}
}
