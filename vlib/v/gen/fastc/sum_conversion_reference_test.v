module fastc

import os
import v.cmdexec

fn test_references_to_sum_conversions_keep_the_carrier_and_variant_alive() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_sum_conversion_reference_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'values'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'sum_conversion_reference' }")!
	os.write_file(os.join_path(root, 'values', 'values.v'), r'module values
pub struct Number { pub: value int }
pub enum Flag { no yes }
pub type Value = Number | Flag | string
pub fn boxed(value Value) &Value {
    return match value {
        Number { &Value(value) }
        Flag { &Value(value) }
        string { &Value(value) }
    }
}
')!
	os.write_file(os.join_path(root, 'main.v'), r'module main
import values
fn copied(value values.Value) &values.Value { return &values.Value(value) }
fn direct() &values.Value { return &values.Value(values.Number{value: 42}) }
fn main() {
    number := values.boxed(values.Number{value: 21})
    flag := values.boxed(values.Flag.yes)
    text := values.boxed("saved")
    copied_value := copied(values.Number{value: 7})
    original := direct()
    for i in 0 .. 100 {
        overwritten := values.boxed(values.Number{value: i})
        assert ((*overwritten) as values.Number).value == i
    }
    assert ((*number) as values.Number).value == 21
    assert ((*flag) as values.Flag) == .yes
    assert ((*text) as string) == "saved"
    assert ((*copied_value) as values.Number).value == 7
    assert ((*original) as values.Number).value == 42
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
	}
}
