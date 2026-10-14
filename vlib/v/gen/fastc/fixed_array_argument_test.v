module fastc

import os
import v.cmdexec

fn test_fastc_fixed_arrays_convert_to_dynamic_call_parameters() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_fixed_array_arguments_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
const names = ["alpha", "beta"]!
__global numbers [3]u16
struct Record { names [2]string }
struct Receiver {}
fn first(values []string) string { return values[0] }
fn sum(values []u16) u16 {
	mut total := u16(0)
	for value in values { total += value }
	return total
}
fn (receiver Receiver) first(values []string) string { return first(values) }
fn from_reference(mut values [2]string) string { return first(values) }
fn make_names() [2]string { return ["gamma", "delta"]! }
fn main() {
	assert first(names) == "alpha"
	assert Receiver{}.first(names) == "alpha"
	assert first(make_names()) == "gamma"
	mut local := ["local", "array"]!
	assert from_reference(mut local) == "local"
	mut record := Record{}
	record.names[0] = "field"
	record.names[1] = "array"
	assert first(record.names) == "field"
	assert Receiver{}.first(record.names) == "field"
	mut field_copy := record.names
	field_copy[0] = "copy"
	assert field_copy[0] == "copy"
	assert record.names[0] == "field"
	numbers[0] = 100
	numbers[1] = 200
	numbers[2] = 300
	assert sum(numbers) == 600
	assert sum((numbers)) == 600
	mut number_copy := (numbers)
	number_copy[1] = 9
	assert number_copy[1] == 9
	assert numbers[1] == 200
	println("fixed arguments work")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], vroot, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'fixed arguments work', run.output
}
