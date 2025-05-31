module cbuilder

import os
import strings
import v.pref
import v.util
import v.builder
import v.gen.c

pub fn start() {
	mut args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, _ := pref.parse_args([], args_and_flags)
	builder.compile('build', prefs, compile_c)
}

pub fn compile_c(mut b builder.Builder) {
	if b.pref.is_verbose {
		println('all .v files before:')
	}
	$if windows {
		b.find_win_cc() or {
			builder.verror('
==================
Error: no C compiler detected.

You can find instructions on how to install one in the V wiki:
https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows

If you think you have one installed, make sure it is in your PATH.
If you do have one in your PATH, please raise an issue on GitHub:
https://github.com/vlang/v/issues/new/choose

You can also use `v doctor`, to see what V knows about your current environment.

You can also seek #help on Discord: https://discord.gg/vlang
')
		}
	}
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	if b.pref.is_verbose {
		println('all .v files:')
		println(files)
	}
	mut out_name_c := b.get_vtmp_filename(b.pref.out_name, '.tmp.c')
	if b.pref.is_shared {
		out_name_c = b.get_vtmp_filename(b.pref.out_name, '.tmp.so.c')
	}
	build_c(mut b, files, out_name_c)
	if !b.pref.parallel_cc {
		b.cc()
	}
}

pub fn build_c(mut b builder.Builder, v_files []string, out_file string) {
	b.out_name_c = out_file
	b.pref.out_name_c = os.real_path(out_file)
	b.info('build_c(${out_file})')
	mut output2 := gen_c(mut b, v_files)
	if b.pref.is_vlines {
		output2 = c.fix_reset_dbg_line(output2, out_file)
	}
	os.write_file_array(out_file, output2) or { panic(err) }
	if b.pref.is_stats {
		b.stats_lines = output2.count(it == `\n`) + 1
		b.stats_bytes = output2.len
	}
}

pub fn gen_c(mut b builder.Builder, v_files []string) strings.Builder {
	b.front_and_middle_stages(v_files) or {
		if err.code() > 7000 {
			return []u8{}
		}
		builder.verror(err.msg())
	}

	util.timing_start('C GEN')
	result := c.gen(b.parsed_files, mut b.table, b.pref)
	util.timing_measure('C GEN')

	if b.pref.parallel_cc {
		b.cc() // Call it just to gen b.str_args
		util.timing_start('Parallel C compilation')
		parallel_cc(mut b, result) or { builder.verror(err.msg()) }
		util.timing_measure('Parallel C compilation')
	}

	return result.res_builder
}
