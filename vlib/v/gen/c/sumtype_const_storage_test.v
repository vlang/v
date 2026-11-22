module main

import os
import v.cmdexec

fn test_sumtype_aggregate_const_uses_runtime_storage() ! {
	root := os.join_path(os.vtmp_dir(), 'sumtype_const_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	binary := os.join_path(root, 'main')
	os.write_file(source, 'struct Empty {}
struct Payload { value int }
type Value = Empty | Payload | int
const empty_value = Value(Empty{})
const payload_value = Value(Payload{42})
fn main() {
 assert empty_value is Empty
 assert (payload_value as Payload).value == 42
}')!
	run := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-b', 'c', '-o', binary, 'run', source])
	assert run.exit_code == 0, run.output
	generated := os.read_file('${binary}.c')!
	assert generated.contains('Value main__empty_value;')
	assert generated.contains('Value main__payload_value;')
	assert !generated.contains('const Value main__empty_value = ')
	assert !generated.contains('const Value main__payload_value = ')
}
