module fastc

import os
import v.pref
import v.cmdexec

fn test_fastc_imports_modules_declared_only_in_manifest_subdirectories() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_manifest_module_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'palette', 'src', 'types'))!
	os.mkdir_all(os.join_path(root, 'palette', 'src', 'helpers'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'palette', 'v.mod'), "Module { name: 'palette', base_url: 'src', subdirs: ['types', 'helpers'] }\n")!
	os.write_file(os.join_path(root, 'palette', 'src', 'types', 'color.v'), 'module palette\npub struct Color { pub: value i32 }\n')!
	os.write_file(os.join_path(root, 'palette', 'src', 'helpers', 'helper.v'), 'module palette\npub fn color() Color { return Color{42} }\n')!
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
import palette
#include "@DIR/native.m"
fn C.native_color(palette.Color) i32
fn main() { assert C.native_color(palette.color()) == 42 }
')!
	os.write_file(os.join_path(root, 'native.m'), 'int32_t native_color(palette__Color color) { return color.value; }\n')!
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.module_search_paths = [root, os.join_path(prefs.vroot, 'vlib')]
	files := fastc_list_module_sources(os.join_path(root, 'palette'), prefs)
	assert files.len == 2
	generation := generate_files_with_source_paths([root], prefs)!
	for path in files {
		assert os.real_path(path) in generation.source_paths
	}
	// A new source in a declared subdirectory must invalidate a warm listing.
	extra := os.join_path(root, 'palette', 'src', 'helpers', 'extra.v')
	os.write_file(extra, 'module palette\npub fn extra() i32 { return 7 }\n')!
	updated := generate_files_with_source_paths([root], prefs)!
	assert os.real_path(extra) in updated.source_paths
	$if macos {
		program := os.join_path(root, 'program')
		built := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', 'tcc', '-no-retry-compilation', '-path',
			'${root}|@vlib', '-o', program, root])
		assert built.exit_code == 0, built.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
