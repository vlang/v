import os

fn test_dbg_compiles_with_reserved_function_argument_name() {
	pid := os.getpid()
	source_path := os.join_path(os.vtmp_dir(), 'v_issue_26746_debugger_reserved_arg_name_${pid}.v')
	exe_path := os.join_path(os.vtmp_dir(), 'v_issue_26746_debugger_reserved_arg_name_${pid}.exe')
	source := [
		'pub fn bad_function(array []u8) u8 {',
		r'	$dbg',
		'	return array[0]',
		'}',
		'',
		'fn main() {',
		'	_ = bad_function([u8(1)])',
		'}',
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(exe_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failed command: ${cmd}')
		eprintln(res.output)
	}
	assert res.exit_code == 0
}

fn test_dbg_compiles_as_first_script_statement() {
	pid := os.getpid()
	source_path := os.join_path(os.vtmp_dir(), 'v_dbg_first_script_statement_${pid}.v')
	exe_path := os.join_path(os.vtmp_dir(), 'v_dbg_first_script_statement_${pid}.exe')
	source := [
		r'$dbg;',
		"println('after dbg')",
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(exe_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failed command: ${cmd}')
		eprintln(res.output)
	}
	assert res.exit_code == 0
}

fn test_dbg_compiles_in_first_script_comptime_if_branch() {
	pid := os.getpid()
	source_path := os.join_path(os.vtmp_dir(), 'v_dbg_first_script_comptime_if_branch_${pid}.v')
	exe_path := os.join_path(os.vtmp_dir(), 'v_dbg_first_script_comptime_if_branch_${pid}.exe')
	source := [
		r'$if linux {',
		r'	$dbg;',
		r'}',
		"println('after dbg')",
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(exe_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failed command: ${cmd}')
		eprintln(res.output)
	}
	assert res.exit_code == 0
}

fn test_dbg_compiles_in_first_script_comptime_match_branch() {
	pid := os.getpid()
	source_path := os.join_path(os.vtmp_dir(), 'v_dbg_first_script_comptime_match_branch_${pid}.v')
	exe_path := os.join_path(os.vtmp_dir(), 'v_dbg_first_script_comptime_match_branch_${pid}.exe')
	source := [
		r'$match @OS {',
		r"	'linux' { $dbg; }",
		r"	'macos' { $dbg; }",
		r"	'windows' { $dbg; }",
		r'	$else { $dbg; }',
		r'}',
		"println('after dbg')",
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(exe_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failed command: ${cmd}')
		eprintln(res.output)
	}
	assert res.exit_code == 0
}

fn test_dbg_compiles_after_hash_in_first_script_comptime_branches() {
	pid := os.getpid()
	sources := {
		'if':    [
			r'$if linux {',
			r'	#include <stdint.h>',
			r'	$dbg;',
			r'} $else {',
			r'	#include <stdint.h>',
			r'	$dbg;',
			r'}',
			"println('after dbg')",
		].join_lines()
		'match': [
			r'$match @OS {',
			r"	'linux' {",
			r'		#include <stdint.h>',
			r'		$dbg;',
			r'	}',
			r'	$else {',
			r'		#include <stdint.h>',
			r'		$dbg;',
			r'	}',
			r'}',
			"println('after dbg')",
		].join_lines()
	}
	for name, source in sources {
		source_path := os.join_path(os.vtmp_dir(),
			'v_dbg_first_script_comptime_hash_${name}_${pid}.v')
		exe_path := os.join_path(os.vtmp_dir(),
			'v_dbg_first_script_comptime_hash_${name}_${pid}.exe')
		os.write_file(source_path, source)!
		defer {
			os.rm(source_path) or {}
			os.rm(exe_path) or {}
		}
		cmd := '${os.quoted_path(@VEXE)} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
		res := os.execute(cmd)
		if res.exit_code != 0 {
			eprintln('> failed command: ${cmd}')
			eprintln(res.output)
		}
		assert res.exit_code == 0
	}
}

fn test_dbg_compiles_in_global_anon_fn_initializer() {
	pid := os.getpid()
	source_path := os.join_path(os.vtmp_dir(), 'v_dbg_global_anon_fn_initializer_${pid}.v')
	exe_path := os.join_path(os.vtmp_dir(), 'v_dbg_global_anon_fn_initializer_${pid}.exe')
	source := [
		'__global cb = fn () {',
		r'	$dbg',
		'}',
		'',
		'fn main() {',
		'	cb()',
		'}',
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(exe_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} -enable-globals -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failed command: ${cmd}')
		eprintln(res.output)
	}
	assert res.exit_code == 0
}
