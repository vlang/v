module fastc

import os
import v.cmdexec

// In a `?T` function, `return f() or { ...; none }` returns none instead of assigning `none`
// to a payload value.
fn test_fastc_or_block_none_values_return_none() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_or_block_none_values_return_none_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "@[has_globals]
module main

struct Head {
	units int
}

fn Head.init(data []u8) !Head {
	if data.len < 2 {
		return error('short')
	}
	return Head{int(data[0]) * 256 + int(data[1])}
}

__global failures = 0

fn parse_head(data []u8) ?Head {
	return Head.init(data) or {
		failures += err.msg().len
		none
	}
}

fn main() {
	if h := parse_head([u8(4), 0]) {
		println(h.units)
	}
	println(parse_head([u8(1)]) == none)
	println(failures)
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
	assert run.output.trim_space() == '1024\ntrue\n5', run.output
}
