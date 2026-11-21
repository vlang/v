module fastc

import os
import v.cmdexec

// A value passed to a generic `&T` parameter is referenced, and `T` is its type, as with
// the C backend.
fn test_fastc_values_passed_to_generic_reference_parameters() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_values_passed_to_generic_reference_param_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'cfg')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'font')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'cfg', 'cfg.v'), "module cfg

pub struct Config {
pub:
	families []string
	size     int
}

pub fn load() Config {
	return Config{['mono'], 12}
}
") or { panic(err) }
	os.write_file(os.join_path(root, 'font', 'font.v'), 'module font

pub struct DerivedConfig {
pub mut:
	families []string
	size     int
}

pub fn DerivedConfig.init[T](config &T) !DerivedConfig {
	return DerivedConfig{
		families: config.families.clone()
		size:     config.size
	}
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import cfg
import font

fn main() {
	config := cfg.load()
	d := font.DerivedConfig.init(config)!
	println(d.families)
	r := &config
	d2 := font.DerivedConfig.init(r)!
	println(d2.size)
}
') or { panic(err) }
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
	assert run.output.trim_space() == "['mono']\n12", run.output
}
