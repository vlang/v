module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_consecutive_pointer_stores_keep_statement_boundaries() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_raw_memory_store_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Slots {
mut:
	values [2]u32
}
fn main() {
	mut memory := [u64(0), u64(0), u64(0)]!
	address := unsafe { u64(&memory[0]) }
	mut count := u16(9)
	unsafe {
		*&u64(address) = u64(0x1122334455667788)
		*&u32(address + 8) = u32(0xaabbccdd)
		*&u16(address + 12) = count++
		*&u16(address + 14) = u16(17)
		*&u16(address + 16 + 4) = u16(19)
	}
	unsafe {
		assert *&u64(address) == u64(0x1122334455667788)
		assert *&u32(address + 8) == u32(0xaabbccdd)
		assert *&u16(address + 12) == 9 && count == 10
		assert *&u16(address + 14) == 17
		assert *&u16(address + 20) == 19
	}
	mut first := u32(0)
	mut second := u32(0)
	first_pointer := &first
	second_pointer := &second
	mut slots := Slots{}
	pointers := [first_pointer, second_pointer]
	unsafe {
		*first_pointer = u32(21)
		*second_pointer = u32(22)
		*pointers[0] = u32(31)
		*&slots.values[0] = u32(41)
		*&slots.values[1] = u32(42)
	}
	assert first == 31 && second == 22
	assert slots.values == [u32(41), u32(42)]!
	mut arithmetic := 0
	arithmetic = 6
		* 7
		+ 1
	assert arithmetic == 43
	mut multiplier := 7
	arithmetic = 6
		* multiplier++
	assert arithmetic == 42 && multiplier == 8
	masked := u32(255)
		& u32(42)
	assert masked == 42
	mut previous := u16(0)
	previous = count--
	assert previous == 10 && count == 9
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
