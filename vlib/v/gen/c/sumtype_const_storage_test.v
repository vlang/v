module c

import os
import v.cmdexec

fn test_sumtype_aggregate_const_uses_runtime_storage() ! {
	root := os.join_path(os.vtmp_dir(), 'sumtype_const_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'struct Empty {}
struct Payload { value int }
type Value = Empty | Payload | int
const empty_value = Value(Empty{})
const payload_value = Value(Payload{42})
fn main() {
 assert empty_value is Empty
 assert (payload_value as Payload).value == 42
}')!
	generated_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-o', output, source])
	assert generated_result.exit_code == 0, generated_result.output
	generated := os.read_file(output)!
	assert generated.contains('Value main__empty_value;')
	assert generated.contains('Value main__payload_value;')
	assert !generated.contains('const Value main__empty_value = ')
	assert !generated.contains('const Value main__payload_value = ')
	run_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', 'run', source])
	assert run_result.exit_code == 0, run_result.output
}
