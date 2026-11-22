module fastc

import os
import v.cmdexec

// The erased placeholder of a generic function that reads members of a parameter typed by
// its type parameter is a stub; its instances do the work.
fn test_fastc_stubs_placeholders_reading_erased_parameters() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_stubs_placeholders_reading_erased_parame_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'm')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'm', 'm.v'), 'module m

pub struct Derived {
pub mut:
	families []string
	size     int
}

pub fn Derived.init[T](config &T) !Derived {
	return Derived{
		families: config.families.clone()
		size:     config.size
	}
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import m

struct Config {
	families []string
	size     int
}

fn main() {
	c := Config{['mono', 'sans'], 12}
	d := m.Derived.init(&c)!
	println(d.families)
	println(d.size)
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
	assert run.output.trim_space() == "['mono', 'sans']\n12", run.output
}
