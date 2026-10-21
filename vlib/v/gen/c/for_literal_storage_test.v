module c

import os

fn test_literal_iteration_uses_fixed_storage() {
	path := os.join_path(os.vtmp_dir(), 'literal_loop_${os.getpid()}.v')
	output := path + '.c'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	source := '@[noinline]
fn sum_pair(first int, second int) int {
 mut sum := 0
 for value in [first, second] { sum += value }
 return sum
}
fn main() { assert sum_pair(12, 30) == 42 }'
	os.write_file(path, source)!
	command := '${os.quoted_path(@VEXE)} -new-compiler -o ${os.quoted_path(output)} ${os.quoted_path(path)}'
	result := os.execute(command)
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	body := generated.all_after('__attribute__((noinline)) i64 sum_pair(')
		.all_before('\n}')
	assert body.contains('first') && body.contains('second')
	assert !body.contains('new_array')
	assert !body.contains('array_get')
	assert !body.contains('malloc')
}
