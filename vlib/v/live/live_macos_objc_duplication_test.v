import os

fn test_sharedlive_macos_does_not_export_sokol_objc_classes() {
	$if !macos {
		return
	}
	nm_path := os.find_abs_path_of_executable('nm') or { panic(err) }
	tmp_dir := os.join_path(os.vtmp_dir(), 'live_macos_objc_duplication')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	dylib_path := os.join_path(tmp_dir, 'graph_sharedlive.dylib')
	graph_path := os.join_path(@VEXEROOT, 'examples', 'hot_reload', 'graph.v')
	build_cmd := '${os.quoted_path(@VEXE)} -nocolor -cc clang -sharedlive -shared -o ${os.quoted_path(dylib_path)} ${os.quoted_path(graph_path)}'
	build_res := os.execute(build_cmd)
	assert build_res.exit_code == 0
	nm_res := os.execute('${os.quoted_path(nm_path)} -gjU ${os.quoted_path(dylib_path)}')
	assert nm_res.exit_code == 0
	assert !nm_res.output.contains(r'_OBJC_CLASS_$_MyView2')
	assert !nm_res.output.contains(r'_OBJC_CLASS_$__sapp_macos_')
	assert !nm_res.output.contains(r'_OBJC_METACLASS_$__sapp_macos_')
}
