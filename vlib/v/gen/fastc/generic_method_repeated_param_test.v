module fastc

import os
import v.cmdexec

// A method that repeats its generic receiver's parameter (`fn (d Decoder[E]) decode[E]()`) is
// specialized in the instance copy; calls may still pass the type (`d.decode[BigEndian](x)`), also
// through an alias of the instance, and a method with further parameters (`read[E, T]`) stays
// generic.
fn test_fastc_specialized_method_accepts_repeated_type_arguments() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_specialized_method_accepts_repeated_type_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'codec')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'codec', 'codec.v'), 'module codec

pub struct BigEndian {}

pub struct LittleEndian {}

pub struct Decoder[E] {
pub mut:
	count int
}

pub fn (mut d Decoder[E]) decode[E](data []u8) int {
	d.count++
	$if E is BigEndian {
		return int(data[0]) << 8 | int(data[1])
	} $else {
		return int(data[1]) << 8 | int(data[0])
	}
}

pub fn (mut d Decoder[E]) read[E, T](data []u8) T {
	return T(d.decode[E](data))
}

pub fn (d Decoder[E]) deinit[E]() int {
	return d.count
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import codec

type BigDecoder = codec.Decoder[codec.BigEndian]

struct Reader {
mut:
	big    codec.Decoder[codec.BigEndian]
	little codec.Decoder[codec.LittleEndian]
}

fn main() {
	mut r := Reader{}
	data := [u8(1), 2]
	println(r.big.decode[codec.BigEndian](data))
	println(r.little.decode(data))
	println(r.big.read[codec.BigEndian, u16](data))
	println(r.big.deinit[codec.BigEndian]())
	println(r.little.deinit())
	mut alias := BigDecoder{}
	println(alias.decode[codec.BigEndian](data))
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
	assert run.output.trim_space() == '258\n513\n258\n2\n1\n258', run.output
}
