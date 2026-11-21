module fastc

import os
import v.cmdexec

// An `or` block ending in a `match` whose branches all return gives each branch a zero value
// of the payload type, so the conditional type-checks.
fn test_fastc_or_block_match_with_returning_branches() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_or_block_match_with_returning_branches_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Res {
	n int
}

fn parse(s string) !Res {
	if s.len == 0 {
		return error('Empty')
	}
	if s == 'bad' {
		return error('Bad')
	}
	return Res{s.len}
}

fn put(s string) !int {
	updated := parse(s) or {
		match err.msg() {
			'Empty' {
				println('empty input')
				return 0
			}
			'Bad' {
				return error('InvalidFormat')
			}
			else {
				return err
			}
		}
	}
	return updated.n
}

fn main() {
	println(put('abc') or { -1 })
	println(put('') or { -1 })
	println(put('bad') or { -2 })
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
	assert run.output.trim_space() == '3\nempty input\n0\n-2', run.output
}
