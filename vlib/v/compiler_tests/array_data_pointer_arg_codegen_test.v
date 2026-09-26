import os

fn test_dynamic_array_data_passes_its_buffer_to_pointer_param() {
	vexe := @VEXE
	vlib := os.dir(os.dir(os.dir(@FILE)))
	v3_source := os.join_path(vlib, 'v', 'v.v')
	root := os.join_path(os.temp_dir(), 'v3_array_data_pointer_arg_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	v3_bin := os.join_path(root, 'v3')
	build := os.execute('${vexe} -gc none -path "${vlib}|@vlib|@vmodules" -o ${v3_bin} ${v3_source}')
	assert build.exit_code == 0, build.output
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

struct Slot {
mut:
	data voidptr
}

fn first(values &voidptr) voidptr {
	return unsafe { values[0] }
}

fn set(value &voidptr) {
	unsafe { *value = voidptr(usize(0x5678)) }
}

fn main() {
	mut values := unsafe { []voidptr{len: 2} }
	values[0] = voidptr(usize(0x1234))
	assert first(values.data) == voidptr(usize(0x1234))
	mut slot := Slot{}
	set(slot.data)
	assert slot.data == voidptr(usize(0x5678))
}
') or { panic(err) }
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} -gc none -o ${bin} ${source}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
}
