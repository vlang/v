module fastc

import os
import v.cmdexec

// An `or` block of a specialized generic method call that returns `!` binds `err`, because
// the call is typed by the instance signature.
fn test_fastc_or_blocks_of_generic_method_calls() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_or_blocks_of_generic_method_calls_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'dec')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'dec', 'dec.v'), "module dec

pub struct Reader {
pub mut:
	left int
}

pub struct Decoder {
pub mut:
	state int
}

fn (mut d Decoder) fill[R](mut input R) ! {
	if input.left == 0 {
		return error('EndOfStream')
	}
	input.left--
}

fn (mut d Decoder) fill_plain(mut input Reader) ! {
	if input.left == 0 {
		return error('EndOfStream')
	}
	input.left--
}

pub fn drain(mut d Decoder, mut input Reader) ! {
	for {
		d.fill(mut input) or {
			if err.msg() == 'EndOfStream' && d.state == 0 {
				return
			}
			return error('ReadFailed')
		}
	}
}

pub fn drain_plain(mut d Decoder, mut input Reader) ! {
	for {
		d.fill_plain(mut input) or {
			if err.msg() == 'EndOfStream' {
				return
			}
			return error('ReadFailed')
		}
	}
}

") or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import dec

fn main() {
	mut d := dec.Decoder{}
	mut r := dec.Reader{3}
	dec.drain(mut d, mut r) or { panic(err) }
	mut r2 := dec.Reader{2}
	dec.drain_plain(mut d, mut r2) or { panic(err) }
	println('\${r.left} \${r2.left}')
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
	assert run.output.trim_space() == '0 0', run.output
}
