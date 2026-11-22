module fastc

import os
import v.cmdexec

// A struct field typed by an alias of a fixed array (`type Mat = [4]f32`) is stored as the
// fixed-array struct, so copying it into a raw `[4]f32` field reads its `.data`.
fn test_fastc_alias_fixed_array_field_is_array_struct() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_alias_fixed_array_field_is_array_struct_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

type Mat = [4]f32

struct Uniforms {
	projection Mat
	size       [2]f32
	scale      f32
}

struct GpuUniforms {
	projection [4]f32
	size       [2]f32
	scale      f32
}

fn convert(value Uniforms) GpuUniforms {
	return GpuUniforms{
		projection: value.projection
		size:       value.size
		scale:      value.scale
	}
}

fn copy_alias(value Uniforms) Uniforms {
	return Uniforms{
		projection: value.projection
		size:       value.size
	}
}

fn main() {
	u := Uniforms{
		projection: Mat([f32(1), 2, 3, 4]!)
		size:       [f32(5), 6]!
		scale:      7
	}
	g := convert(u)
	println(g.projection[3])
	println(g.size[1])
	println(g.scale)
	c := copy_alias(u)
	println(c.projection[2])
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
	assert run.output.trim_space() == '4.0\n6.0\n7.0\n3.0', run.output
}
