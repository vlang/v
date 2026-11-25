import os
import v.tempname

fn test_dynamic_array_data_passes_its_buffer_to_pointer_param() {
	root := os.join_path(os.vtmp_dir(), 'v3 array data pointer arg ${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	exe_suffix := $if windows { '.exe' } $else { '' }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

type Values = []voidptr

struct Slot {
mut:
	data voidptr
}

fn first(values &voidptr) voidptr {
	return unsafe { values[0] }
}

fn set(value &voidptr) {
	unsafe { *value = voidptr(uint(0x5678)) }
}

fn through_pointer(values &[]voidptr) {
	assert first(values.data) == voidptr(uint(0x1234))
	assert first((values.data)) == voidptr(uint(0x1234))
	assert first(((values.data))) == voidptr(uint(0x1234))
}

fn through_mut(mut values []voidptr) {
	assert first(values.data) == voidptr(uint(0x1234))
	assert first((values.data)) == voidptr(uint(0x1234))
	assert first(((values.data))) == voidptr(uint(0x1234))
}

fn through_alias(values &Values) {
	assert first(values.data) == voidptr(uint(0x1234))
	assert first((values.data)) == voidptr(uint(0x1234))
	assert first(((values.data))) == voidptr(uint(0x1234))
}

fn main() {
	mut values := unsafe { []voidptr{len: 2} }
	values[0] = voidptr(uint(0x1234))
	assert first(values.data) == voidptr(uint(0x1234))
	assert first((values.data)) == voidptr(uint(0x1234))
	assert first(((values.data))) == voidptr(uint(0x1234))
	through_pointer(&values)
	through_mut(mut values)
	aliased := Values(values)
	through_alias(&aliased)
	mut slot := Slot{}
	set(slot.data)
	assert slot.data == voidptr(uint(0x5678))
	slot.data = unsafe { nil }
	set((slot.data))
	assert slot.data == voidptr(uint(0x5678))
}
') or { panic(err) }
	bin := os.join_path(root, 'out${exe_suffix}')
	compile := os.exec([@VEXE, '-gc', 'none', '-o', bin, source])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}
