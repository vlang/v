module fastc

import os
import v.cmdexec

// An alias of a sum type (`type Buf = KBuf`, also inside `$if`) is a sum type for `match`
// and casts, and an `or` fallback chosen by `$if` is boxed into the sum type.
fn test_fastc_sum_type_alias_and_comptime_or_fallback() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_sum_type_alias_and_comptime_or_fallback_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'm')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'm', 'm.v'), "module m

pub struct ShellCmd {
pub:
	shell string
}

pub struct DirectCmd {
pub:
	args []string
}

pub type Command = ShellCmd | DirectCmd

pub struct Config {
pub:
	command ?Command
}

pub struct SliceBuf {
pub:
	slice []u8
}

pub struct ArrayBuf {
pub:
	n int
}

$if macos {
	pub type KBuf = ArrayBuf | SliceBuf
	pub type Buf = KBuf
} $else {
	pub type KBuf = ArrayBuf | SliceBuf
	pub type Buf = KBuf
}

pub fn describe(b Buf) string {
	return match b {
		SliceBuf { 'slice \${b.slice.len}' }
		ArrayBuf { 'array \${b.n}' }
	}
}
") or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import m

fn show(c m.Command) string {
	return match c {
		m.ShellCmd { 'shell \${c.shell}' }
		m.DirectCmd { 'direct \${c.args.len}' }
	}
}

fn main() {
	cfg := m.Config{}
	cmd := cfg.command or {
		$if windows {
			m.ShellCmd{
				shell: 'cmd.exe'
			}
		} $else {
			m.ShellCmd{
				shell: 'sh'
			}
		}
	}
	println(show(cmd))
	slice := [u8(1), 2, 3]
	println(m.describe(m.Buf(m.SliceBuf{
		slice: slice
	})))
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
	assert run.output.trim_space() == 'shell sh\nslice 3', run.output
}
