module c

import os
import v3.parser
import v3.pref

fn test_c_directive_targets_use_requested_platform() {
	target := pref.target_from('macos', 'arm64') or { panic(err) }
	assert c_flag_args('macos -DMACOS', '', '', target) == ['-DMACOS']
	assert c_flag_args('arm64 -DARM64', '', '', target) == ['-DARM64']
	assert c_flag_args('linux -DLINUX', '', '', target).len == 0
	assert c_flag_args('amd64 -DAMD64', '', '', target).len == 0
	assert c_include_arg_for_target('macos <TargetConditionals.h>', '', '', target) == '<TargetConditionals.h>'
	assert c_include_arg_for_target('windows <windows.h>', '', '', target) == ''
}

fn test_termux_c_directive_target_is_distinct_from_android() {
	termux := pref.target_from('termux', 'arm64') or { panic(err) }
	android := pref.target_from('android', 'arm64') or { panic(err) }
	assert c_flag_args('termux -DTERMUX', '', '', termux) == ['-DTERMUX']
	assert c_flag_args('termux -DTERMUX', '', '', android).len == 0
}

fn test_termux_comptime_branch_uses_canonical_target() {
	dir := os.join_path(os.vtmp_dir(), 'v3_termux_comptime_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source,
		'module main\n\n\$if termux {\nfn termux_selected() {}\n} \$else {\nfn android_selected() {}\n}\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('termux', 'arm64') or { panic(err) }
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([source])
	fn_names := a.nodes.filter(it.kind == .fn_decl).map(it.value)
	assert 'termux_selected' in fn_names
	assert 'android_selected' !in fn_names
}
