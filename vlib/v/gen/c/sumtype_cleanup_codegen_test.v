module c

import os
import v.cmdexec

fn test_sum_storage_acquires_only_active_literal_and_shares_cleanup() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'sum_storage_cleanup_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	compiler := os.join_path(dir, 'v-ownership')
	vlib := os.join_path(@VEXEROOT, 'vlib')
	build := os.exec([@VEXE, '-new-compiler', '-cc', cc, '-gc', 'none', '-d', 'ownership', '-path',
		vlib, '-o', compiler, os.join_path(vlib, 'v', 'v.v')])
	assert build.exit_code == 0, build.output
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'type Choice = int | string | []int | []string | Box[[2]string]
type Nested = Choice | bool
type Box[T] = T | int
fn boxed() Box[[2]string] { return 17 }
fn scalar() Choice { return Choice(17) }
fn slice_value() Choice {
	values := [1, 2, 3]
	return Choice(values[1..])
}
fn nested_slice() Nested {
	values := ["first", "second"]
	return Nested(Choice(values[1..]))
}
fn unknown(value Choice) Choice { return value }
fn acquired() Choice { return unknown(Choice([4, 5])) }
fn unknown_nested(value Nested) Nested { return value }
fn acquired_nested() Nested { return unknown_nested(nested_slice()) }
fn main() {
	assert (boxed() as int) == 17
	assert (scalar() as int) == 17
	assert (slice_value() as []int) == [2, 3]
	nested := nested_slice() as Choice
	assert (nested as []string) == ["second"]
	assert (acquired() as []int) == [4, 5]
	dynamic := acquired_nested() as Choice
	assert (dynamic as []string) == ["second"]
	assert (unknown(Choice("text")) as string) == "text"
}')!
	for jobs in ['1', '2'] {
		for mode in ['-ownership', '-autofree'] {
			generated := os.join_path(dir, 'program.c')
			result := cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-gc', 'none', mode,
				'-o', generated, source], dir, {
				'VJOBS': jobs
			})
			assert result.exit_code == 0, 'VJOBS=${jobs} ${mode}: ${result.output}'
			csrc := os.read_file(generated)!
			// The known scalar payload must bypass unrelated array variants entirely.
			assert csrc.contains('scalar(void) {')
			scalar := csrc.all_after('scalar(void) {').all_before('\n}')
			assert scalar.len > 0
			assert !scalar.contains('owned_sum_array_escape'), scalar
			assert !scalar.contains('is_slice_view'), scalar
			// Serial and parallel support lanes must both enroll transformed cleanup sites.
			helpers := csrc.split_into_lines().filter(it.starts_with('static void __v3_ownership_drop_'))
			assert helpers.filter(it.contains('Choice(')).len == 2, 'VJOBS=${jobs} ${mode}: ${helpers}'
			assert helpers.filter(it.contains('Nested(')).len == 2, 'VJOBS=${jobs} ${mode}: ${helpers}'
			sum_header := csrc.split_into_lines().filter(it.starts_with('struct Box'))
			assert sum_header.len == 1, sum_header.str()
			sum_c_type := sum_header[0].all_after('struct ').all_before(' {')
			assert helpers.filter(it.contains('(${sum_c_type}* _value)')).len == 2, 'VJOBS=${jobs} ${mode}: ${helpers}'
			run := cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-gc', 'none', '-cc',
				cc, mode, 'run', source], dir, {
				'VJOBS': jobs
			})
			assert run.exit_code == 0, 'VJOBS=${jobs} ${mode}: ${run.output}'
		}
	}
}
