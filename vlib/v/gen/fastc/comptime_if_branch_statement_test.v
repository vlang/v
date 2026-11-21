module fastc

import os
import v.cmdexec

// A `$if` that does not end its block is a statement, also in a `match` or `if`
// expression branch whose value follows it.
fn test_fastc_comptime_if_statements_in_expression_branches() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_if_branch_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

enum Tag {
	mac
	other
}

struct Platform {
	name string
}

fn platform_init(tag Tag, name string) !Platform {
	return match tag {
		.mac {
			$if !macos && !linux {
				return error('UnsupportedPlatform')
			}
			if name == '' {
				return error('name must be set')
			}
			Platform{
				name: name
			}
		}
		.other {
			$if macos {
				Platform{
					name: 'mac-' + name
				}
			} $else {
				Platform{
					name: 'other-' + name
				}
			}
		}
	}
}

fn label(n int) string {
	value := if n > 0 {
		$if macos || linux {
			println('positive')
		}
		'pos'
	} else {
		'neg'
	}
	return value
}

fn main() {
	println(platform_init(.mac, 'view') or { Platform{err.msg()} }.name)
	println(platform_init(.mac, '') or { Platform{err.msg()} }.name)
	p := platform_init(.other, 'x') or { panic(err) }
	println(p.name.len > 0)
	println(label(3))
	println(label(-3))
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
	assert run.output.trim_space() == 'view\nname must be set\ntrue\npositive\npos\nneg', run.output
}
