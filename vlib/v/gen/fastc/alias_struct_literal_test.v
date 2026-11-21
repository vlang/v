module fastc

import os
import v.cmdexec

// A literal of an alias uses the aliased struct, from another module or from C.
fn test_fastc_literals_of_struct_aliases() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_literals_of_struct_aliases_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'cfg')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'diag')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'cfg', 'cfg.v'), "module cfg

import diag

pub type Diagnostic = diag.Diagnostic

pub struct Config {
pub mut:
	diagnostics []Diagnostic
}

pub fn (mut c Config) add(d Diagnostic) {
	c.diagnostics << d
}

pub fn (mut c Config) warn(key string, message string) {
	c.add(Diagnostic{
		key:     key
		message: message
	})
	c.diagnostics[0] = Diagnostic{
		key:     key + '!'
		message: message
	}
}
") or { panic(err) }
	os.write_file(os.join_path(root, 'diag', 'diag.v'), 'module diag

pub struct Diagnostic {
pub:
	key     string
	message string
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import cfg

#include <time.h>

struct C.tm {
	tm_hour int
	tm_min  int
}

type Tm = C.tm

fn minutes(t Tm) int {
	return t.tm_hour * 60 + t.tm_min
}

fn main() {
	mut c := cfg.Config{}
	c.warn('font', 'missing')
	println(c.diagnostics[0].key)
	println(c.diagnostics.len)
	println(minutes(Tm{
		tm_hour: 4
		tm_min:  2
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
	assert run.output.trim_space() == 'font!\n1\n242', run.output
}
