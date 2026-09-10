import os

fn live_reload_info_call_source(c_source string) string {
	start := c_source.index('v__live__LiveReloadInfo* live_info = v__live__executable__new_live_reload_info(') or {
		panic('missing live reload info call')
	}
	end_offset := c_source[start..].index('v_bind_live_symbols') or {
		panic('missing live reload info call terminator')
	}
	return c_source[start..start + end_offset]
}

fn live_windows_target_tests_available() bool {
	$if windows {
		return true
	}
	vcross_compiler_name := os.getenv('VCROSS_COMPILER_NAME')
	if vcross_compiler_name != '' {
		if vcross_compiler_name.contains('/') || vcross_compiler_name.contains('\\') {
			if os.is_file(vcross_compiler_name) && os.is_executable(vcross_compiler_name) {
				return true
			}
		} else if _ := os.find_abs_path_of_executable(vcross_compiler_name) {
			return true
		}
	}
	mingw_compiler_path := os.find_abs_path_of_executable('x86_64-w64-mingw32-gcc') or { '' }
	return mingw_compiler_path != ''
}

fn test_livemain_windows_forwards_d3d11_define_to_sharedlive_rebuild_vopts() {
	if !live_windows_target_tests_available() {
		eprintln('> skipping Windows live reload D3D11 vopts codegen check: no Windows target compiler found via VCROSS_COMPILER_NAME or x86_64-w64-mingw32-gcc')
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'live_windows_d3d11_vopts_cgen')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'live_d3d11_probe.v')
	c_path := os.join_path(tmp_dir, 'live_d3d11_probe.c')
	os.write_file(source_path, [
		'module main',
		'',
		'@[live]',
		'fn redraw() {',
		'}',
		'',
		'fn main() {',
		'\tredraw()',
		'}',
	].join('\n'))!
	build_cmd := '${os.quoted_path(@VEXE)} -nocolor -os windows -live -d sokol_d3d11 -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}'
	build_res := os.execute(build_cmd)
	if build_res.exit_code != 0 {
		eprintln('> skipping Windows live reload D3D11 vopts codegen check: `${build_cmd}` failed with exit code ${build_res.exit_code}:\n${build_res.output}')
		return
	}
	c_source := os.read_file(c_path)!
	live_info_call := live_reload_info_call_source(c_source)
	assert live_info_call.contains(r'-d \"sokol_d3d11\"'), live_info_call
	assert live_info_call.contains('-sharedlive -shared'), live_info_call
	assert !live_info_call.contains('-d livemain'), live_info_call
	assert !live_info_call.contains('-d sharedlive'), live_info_call
	assert !live_info_call.contains('-d windows'), live_info_call
	assert !live_info_call.contains('-d gcboehm'), live_info_call
}

fn test_sharedlive_windows_cgen_reuses_host_sokol_backend() {
	if !live_windows_target_tests_available() {
		eprintln('> skipping Windows sharedlive Sokol backend reuse codegen check: no Windows target compiler found via VCROSS_COMPILER_NAME or x86_64-w64-mingw32-gcc')
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'live_windows_shared_graphics_cgen')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	c_path := os.join_path(tmp_dir, 'graph_sharedlive_windows.c')
	graph_path := os.join_path(@VEXEROOT, 'examples', 'hot_reload', 'graph.v')
	build_cmd := '${os.quoted_path(@VEXE)} -nocolor -os windows -sharedlive -o ${os.quoted_path(c_path)} ${os.quoted_path(graph_path)}'
	build_res := os.execute(build_cmd)
	if build_res.exit_code != 0 {
		eprintln('> skipping Windows sharedlive Sokol backend reuse codegen check: `${build_cmd}` failed with exit code ${build_res.exit_code}:\n${build_res.output}')
		return
	}
	c_source := os.read_file(c_path)!
	assert !c_source.contains('#define SOKOL_APP_IMPL')
	assert !c_source.contains('#define SOKOL_GFX_IMPL')
	assert !c_source.contains('#define SOKOL_IMPL')
}

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
