module fastc

import os
import v.cmdexec

// A `match` branch whose value is a struct field's fixed array (raw C storage) gives the
// fixed-array struct value the other branches give, as an `if` expression does.
fn test_fastc_match_branch_fixed_array_field_value() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_match_branch_fixed_array_field_value_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Rounded {}

struct Square {}

struct Custom {
mut:
	glyphs [6]string
}

type Glyphs = Rounded | Square | Custom

struct Border {
	glyphs Glyphs = Rounded{}
}

struct Opts {
	border Border
}

fn corners(opts Opts) string {
	glyphs := match opts.border.glyphs {
		Rounded { ['a', 'b', 'c', 'd', 'e', 'f']! }
		Square { ['A', 'B', 'C', 'D', 'E', 'F']! }
		Custom { opts.border.glyphs.glyphs }
	}
	return glyphs[0] + glyphs[5]
}

fn main() {
	println(corners(Opts{}))
	println(corners(Opts{
		border: Border{
			glyphs: Square{}
		}
	}))
	println(corners(Opts{
		border: Border{
			glyphs: Custom{
				glyphs: ['1', '2', '3', '4', '5', '6']!
			}
		}
	}))
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
	assert run.output.trim_space() == 'af\nAF\n16', run.output
}
