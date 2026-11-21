module fastc

import os
import v.cmdexec

// A `c''` struct field default stays a C string; its source slice keeps the `c` prefix.
fn test_fastc_c_string_field_default_keeps_prefix() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_c_string_field_default_keeps_prefix_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

pub struct GString {
pub:
	ptr &u8 = unsafe { nil }
	len u32
}

pub fn GString.empty() GString {
	return GString{}
}

pub fn GString.from_slice(b []u8) GString {
	if b.len == 0 {
		return GString.empty()
	}
	return GString{
		ptr: b.data
		len: u32(b.len)
	}
}

pub struct Diag {
pub:
	message &char = c''
}

fn open_path(fail bool) !string {
	if fail {
		return error('nope')
	}
	return 'path'
}

pub fn open(fail bool) GString {
	path := open_path(fail) or {
		println('error \${err}')
		return GString.empty()
	}
	return GString.from_slice(path.bytes())
}

fn main() {
	println(open(false).len)
	println(open(true).len)
	d := Diag{}
	println(unsafe { cstring_to_vstring(d.message) }.len)
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '4\nerror nope\n0\n0', run.output
}
