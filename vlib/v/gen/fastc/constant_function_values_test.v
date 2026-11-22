module fastc

import os
import v.cmdexec

// A constant stores function values erased to voidptr, alone or in arrays. Calls through
// it use the signature recorded with the constant type.
fn test_fastc_constants_keep_stored_function_signatures() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_constant_function_values_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'codecs')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'codecs', 'codecs.v'), 'module codecs

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

fn bmp_codec() Codec {
	return Bmp{}
}

fn png_codec() Codec {
	return Png{}
}

fn twice(x int) int {
	return x * 2
}

const factories = [
	bmp_codec,
	png_codec,
]!

const growing = [bmp_codec, png_codec, bmp_codec]

pub const doubler = twice

pub fn codec_name(i int) string {
	return factories[i]().name()
}

pub fn all_names() []string {
	mut out := []string{}
	for factory in growing {
		out << factory().name()
	}
	return out
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import codecs

fn main() {
	println(codecs.codec_name(1))
	println(codecs.all_names())
	println(codecs.doubler(21))
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
	assert run.output.trim_space() == "png\n['bmp', 'png', 'bmp']\n42", run.output
}
