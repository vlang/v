module fastc

import os
import v.cmdexec

// `[]Alias << []Base`, where `type Alias = Base`, appends each element (push-many), for
// field and local targets.
fn test_fastc_append_array_of_aliased_elements() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_append_array_of_aliased_elements_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'd')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'd', 'd.v'), 'module d

pub struct Diagnostic {
pub:
	message string
}

pub struct List {
pub mut:
	list []Diagnostic
}

pub fn (l List) items() []Diagnostic {
	return l.list
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import d

pub type Diagnostic = d.Diagnostic

struct Config {
mut:
	diagnostics []Diagnostic
}

fn main() {
	mut c := Config{}
	l := d.List{
		list: [d.Diagnostic{'a'}, d.Diagnostic{'b'}]
	}
	c.diagnostics << l.items()
	c.diagnostics << d.Diagnostic{'c'}
	println(c.diagnostics.len)
	println(c.diagnostics.map(it.message).join(','))
	mut local := []Diagnostic{}
	local << l.items()
	println(local.len)
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
	assert run.output.trim_space() == '3\na,b,c\n2', run.output
}
