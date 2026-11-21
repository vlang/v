module fastc

import os
import v.cmdexec

// Explicit calls of generic methods with several type arguments are specialized, also when
// the method lists its struct type parameter among its own (`read_bits[E, T]` on `BitReader[E]`)
// and when its result is a tuple of type parameters.
fn test_fastc_generic_methods_with_several_type_arguments() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_methods_with_several_type_argume_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bits')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'bits', 'bits.v'), "module bits

pub struct BigEndian {}

pub struct BitReader[E] {
pub mut:
	data  []u8
	pos   int
}

pub fn (mut r BitReader[E]) read_tuple[E, T](num u16) !(T, u16) {
	if r.pos >= r.data.len {
		return error('EndOfStream')
	}
	value := r.data[r.pos]
	r.pos++
	return T(value), num
}

pub fn (mut r BitReader[E]) read_bits[E, T](num u16, out_bits &u16) !T {
	value, count := r.read_tuple[E, T](num)!
	unsafe {
		*out_bits = count
	}
	return value
}

pub fn decode(data []u8) !int {
	mut reader := BitReader[BigEndian]{
		data: data
	}
	mut bits_read := u16(0)
	mut code := u16(0)
	code |= reader.read_bits[BigEndian, u16](1, &bits_read)!
	small := reader.read_bits[BigEndian, u8](8, &bits_read)!
	return int(code) * 1000 + int(small) * 10 + int(bits_read)
}
") or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import bits

fn main() {
	println(bits.decode([u8(3), 7])!)
}
') or { panic(err) }
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
	assert run.output.trim_space() == '3078', run.output
}
