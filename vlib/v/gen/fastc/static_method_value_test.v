module fastc

import os
import v.cmdexec

// A static method used as a value (`Codec.new`) is a function address with the
// method's signature, in constants, locals, arrays and arguments.
fn test_fastc_static_method_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_static_method_values_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'img')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'img', 'img.v'), 'module img

pub interface Codec {
	name() string
}

struct Bmp {}

fn (b Bmp) name() string {
	return "bmp"
}

struct Png {}

fn (p Png) name() string {
	return "png"
}

pub fn Bmp.codec() Codec {
	return Bmp{}
}

pub fn Png.codec() Codec {
	return Png{}
}

const factories = [
	Bmp.codec,
	Png.codec,
]!

pub fn codec_name(i int) string {
	return factories[i]().name()
}

pub fn all_names() []string {
	mut out := []string{}
	for factory in factories {
		out << factory().name()
	}
	return out
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import img

struct Bmp {
	n int
}

fn Bmp.make(n int) Bmp {
	return Bmp{n}
}

fn apply(f fn (int) Bmp) int {
	return f(4).n
}

fn main() {
	println(img.codec_name(1))
	println(img.all_names())
	f := Bmp.make
	println(f(3).n)
	println(apply(Bmp.make))
	makers := [Bmp.make]
	made := makers[0](5)
	println(made.n)
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
	assert run.output.trim_space() == "png\n['bmp', 'png']\n3\n4\n5", run.output
}
