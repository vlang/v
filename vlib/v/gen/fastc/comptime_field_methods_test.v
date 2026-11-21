module fastc

import os
import v.cmdexec

// `$for method in field.methods` inside a `$for field in T.fields` loop iterates the methods
// of the field type, also when that type comes from another module.
fn test_fastc_comptime_loops_over_field_type_methods() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_loops_over_field_type_methods_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opts')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'opts', 'opts.v'), "module opts

pub struct Color {
pub mut:
	r int
}

pub fn (mut c Color) init() ! {
	c.r = 255
}

pub struct Font {
pub mut:
	size int
}

pub struct Options {
pub mut:
	color Color
	font  Font
	count int
}

pub fn reset_field[T](mut dst T, key string) ! {
	$for field in T.fields {
		if field.name == key {
			defaults := T{}
			$if field.is_struct {
				$for method in field.methods {
					$if method.name == 'init' {
						mut initialized := defaults.$(field.name)
						initialized.$method() or { return error('invalid value') }
						dst.$(field.name) = initialized
						return
					}
				}
			}
			dst.$(field.name) = defaults.$(field.name)
			return
		}
	}
}
") or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import opts

fn main() {
	mut o := opts.Options{
		color: opts.Color{1}
		font:  opts.Font{2}
		count: 3
	}
	opts.reset_field(mut o, 'color') or { panic(err) }
	opts.reset_field(mut o, 'font') or { panic(err) }
	opts.reset_field(mut o, 'count') or { panic(err) }
	println('\${o.color.r} \${o.font.size} \${o.count}')
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
	assert run.output.trim_space() == '255 0 0', run.output
}
