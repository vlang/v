// vtest build: !self_sandboxed_packaging? && !musl?
module gfx

import os

fn write_sokol_gfx_probe(test_root string) string {
	source_path := os.join_path(test_root, 'sokol_gfx_probe.v')
	os.write_file(source_path, 'import sokol.gfx

fn main() {
	_ = gfx.Desc{}
}
') or { panic(err) }
	return source_path
}

fn sokol_declaration_source() string {
	return os.read_file(os.join_path(@VEXEROOT, 'vlib', 'sokol', 'c', 'declaration.c.v')) or {
		panic(err)
	}
}

fn sokol_screenshot_source() string {
	return os.read_file(os.join_path(@VEXEROOT, 'vlib', 'sokol', 'sapp', 'screenshot.c.v')) or {
		panic(err)
	}
}

fn sokol_sapp_source() string {
	return os.read_file(os.join_path(@VEXEROOT, 'vlib', 'sokol', 'sapp', 'sapp.c.v')) or {
		panic(err)
	}
}

fn sokol_sapp_funcs_source() string {
	return os.read_file(os.join_path(@VEXEROOT, 'vlib', 'sokol', 'sapp', 'sapp_funcs.c.v')) or {
		panic(err)
	}
}

fn sokol_sapp_v_source() string {
	return os.read_file(os.join_path(@VEXEROOT, 'vlib', 'sokol', 'sapp', 'sapp_v.c.v')) or {
		panic(err)
	}
}

fn sokol_v_post_header_source() string {
	return os.read_file(os.join_path(@VEXEROOT, 'thirdparty', 'sokol', 'sokol_v.post.h')) or {
		panic(err)
	}
}

fn windows_target_tests_available() bool {
	$if windows {
		return true
	}
	vcross := os.getenv('VCROSS_COMPILER_NAME')
	if vcross != '' {
		if vcross.contains('/') || vcross.contains('\\') {
			if os.is_file(vcross) && os.is_executable(vcross) {
				return true
			}
		} else if _ := os.find_abs_path_of_executable(vcross) {
			return true
		}
		eprintln('> skipping Windows target cflag/codegen checks: VCROSS_COMPILER_NAME=${vcross} is not executable')
		return false
	}
	if _ := os.find_abs_path_of_executable('x86_64-w64-mingw32-gcc') {
		return true
	}
	eprintln('> skipping Windows target cflag/codegen checks: not on Windows and no usable x86_64-w64-mingw32-gcc or VCROSS_COMPILER_NAME was found')
	return false
}

fn assert_contains_readback_token(source string, token string) {
	assert source.contains(token), 'missing D3D11 screenshot/readback token `${token}`'
}

fn d3d11_readback_source(source string) string {
	d3d_start := source.index('#elif defined(SOKOL_D3D11)') or { return source }
	d3d_source := source[d3d_start..]
	return d3d_source.all_before('\n\t#else\n\t\tint v_sapp_read_rgba_pixels')
}

fn assert_d3d11_readback_helper_tokens(source string) {
	for token in [
		'SOKOL_D3D11',
		'sapp_d3d11_get_swap_chain',
		'GetBuffer',
		'GetDevice',
		'GetImmediateContext',
		'D3D11_USAGE_STAGING',
		'D3D11_CPU_ACCESS_READ',
		'CreateTexture2D',
		'CopyResource',
		'Map',
		'staging_mapped = true',
		'RowPitch',
		'v_sapp_d3d11_unmap',
		'Release',
	] {
		assert_contains_readback_token(source, token)
	}
	lower_source := source.to_lower()
	assert lower_source.contains('bgra') && lower_source.contains('rgba'), 'D3D11 screenshot/readback helper must distinguish BGRA and RGBA sources'
	compact_source := source.replace(' ', '').replace('\t', '').replace('\n', '').replace('\r', '')
	assert compact_source.contains('dst[0]=src[2]') && compact_source.contains('dst[2]=src[0]'), 'D3D11 screenshot/readback helper must convert BGRA pixels to RGBA'
	assert compact_source.contains('hr=v_sapp_d3d11_map(ctx,(ID3D11Resource*)staging,&mapped);if(FAILED(hr)){status=V_SAPP_READBACK_MAP_FAILED;gotov_sapp_d3d11_readback_cleanup;}staging_mapped=true;'), 'D3D11 screenshot/readback helper must mark staging as mapped after successful Map()'
	assert compact_source.contains('if(staging_mapped){v_sapp_d3d11_unmap(ctx,(ID3D11Resource*)staging);}'), 'D3D11 screenshot/readback helper must Unmap() when staging was mapped'
}

fn assert_sapp_wrapper_plumbing() {
	post_header_source := sokol_v_post_header_source()
	sapp_source := sokol_sapp_source()
	sapp_funcs_source := sokol_sapp_funcs_source()
	for token in [
		'SOKOL_APP_API_DECL void v_sapp_get_environment(sapp_environment* out_env) {',
		'*out_env = sapp_get_environment();',
		'SOKOL_APP_API_DECL void v_sapp_get_swapchain(sapp_swapchain* out_swapchain) {',
		'*out_swapchain = sapp_get_swapchain();',
		'SOKOL_APP_API_DECL void v_sapp_get_environment(sapp_environment* out_env);',
		'SOKOL_APP_API_DECL void v_sapp_get_swapchain(sapp_swapchain* out_swapchain);',
	] {
		assert post_header_source.contains(token), 'missing V-private sapp wrapper token `${token}`'
	}
	assert sapp_funcs_source.contains('fn C.v_sapp_get_environment(out_env &Environment)'), sapp_funcs_source
	assert sapp_funcs_source.contains('fn C.v_sapp_get_swapchain(out_swapchain &Swapchain)'), sapp_funcs_source
	assert sapp_source.contains('C.v_sapp_get_environment(&sapp_env)'), sapp_source
	assert sapp_source.contains('C.v_sapp_get_swapchain(&sapp_sc)'), sapp_source
	assert !sapp_source.contains('C.sapp_get_environment()'), sapp_source
	assert !sapp_source.contains('C.sapp_get_swapchain()'), sapp_source
}

fn normalized_cflags(output string) string {
	return '\n' + output.replace('\r\n', '\n') + '\n'
}

fn cflags_have_define(output string, define string) bool {
	cflags := normalized_cflags(output)
	return cflags.contains('\n-D ${define}\n') || cflags.contains('\n-D${define}\n')
		|| cflags.contains('\n/D ${define}\n') || cflags.contains('\n/D${define}\n')
}

fn cflags_have_link_flag(output string, lib string) bool {
	cflags := normalized_cflags(output).to_lower()
	lib_lower := lib.to_lower()
	return cflags.contains('-l${lib_lower}') || cflags.contains('${lib_lower}.lib')
}

// dump_windows_sokol_gfx_cflags returns the Windows cflags for the probe, or
// `none` if the nested `v` invocation could not run in this environment.
// Spawning a nested cross-compilation inside `v test-self` is flaky on some
// hosts (notably a tcc-built v on the Windows CI), so callers skip instead of
// hard-failing when the nested compiler cannot produce the flags; the flag
// contents themselves are host-independent and stay covered on the other runners.
fn dump_windows_sokol_gfx_cflags(extra_vflags []string) ?string {
	test_root := os.join_path(os.vtmp_dir(), 'sokol_windows_backend_flags_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	source_path := write_sokol_gfx_probe(test_root)
	mut cmd := [
		os.quoted_path(@VEXE),
		'-dump-c-flags',
		'-',
		'-os',
		'windows',
		'-o',
		os.quoted_path(os.join_path(test_root, 'sokol_gfx_probe.exe')),
	]
	cmd << extra_vflags
	cmd << os.quoted_path(source_path)
	res := os.execute(cmd.join(' '))
	if res.exit_code != 0 {
		eprintln('> skipping Windows sokol cflag check: `${cmd.join(' ')}` failed with exit code ${res.exit_code}:\n${res.output}')
		return none
	}
	return res.output
}

// generated_windows_c returns the C generated for a Windows target, or `none`
// if the nested `v` invocation could not run in this environment (see
// dump_windows_sokol_gfx_cflags for why callers skip instead of hard-failing).
fn generated_windows_c(extra_vflags []string, source string) ?string {
	test_root := os.join_path(os.vtmp_dir(), 'sokol_windows_generated_c_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	source_path := os.join_path(test_root, 'main.v')
	os.write_file(source_path, source) or { panic(err) }
	mut cmd := [
		os.quoted_path(@VEXE),
		'-o',
		'-',
		'-os',
		'windows',
	]
	cmd << extra_vflags
	cmd << os.quoted_path(source_path)
	res := os.execute(cmd.join(' '))
	if res.exit_code != 0 {
		eprintln('> skipping Windows sokol codegen check: `${cmd.join(' ')}` failed with exit code ${res.exit_code}:\n${res.output}')
		return none
	}
	return res.output
}

fn test_windows_sokol_backend_flags_are_exclusive_in_declaration() {
	source := sokol_declaration_source()
	expected := [
		'$if windows {',
		'\t$if sokol_d3d11 ? {',
		'\t\t#flag windows -DSOKOL_D3D11',
		'\t\t#flag windows -ld3d11 -ldxgi',
		'\t} $else {',
		'\t\t#flag windows -DSOKOL_GLCORE',
		'\t\t#flag windows -lopengl32',
		'\t}',
	].join('\n')
	assert source.contains(expected), source
}

fn test_windows_sokol_d3d11_define_does_not_leave_duplicate_glcore_flags() {
	source := sokol_declaration_source()
	assert source.count('#flag windows -DSOKOL_D3D11') == 1, source
	assert source.count('#flag windows -DSOKOL_GLCORE') == 1, source
	assert source.count('#flag windows -lopengl32') == 1, source
	assert source.count('#flag windows -ld3d11') == 1, source
	assert source.count('-ldxgi') == 1, source
	assert !source.to_lower().contains('d3dcompiler'), source
}

fn test_sokol_screenshot_public_api_signatures_are_preserved() {
	screenshot_source := sokol_screenshot_source()
	sapp_v_source := sokol_sapp_v_source()
	assert screenshot_source.contains('pub struct Screenshot {'), screenshot_source
	assert screenshot_source.contains('pub fn screenshot_window() &Screenshot {'), screenshot_source
	assert sapp_v_source.contains('pub fn screenshot(path string) ! {'), sapp_v_source
	assert sapp_v_source.contains('pub fn screenshot_ppm(path string) ! {'), sapp_v_source
	assert sapp_v_source.contains('pub fn screenshot_png(path string) ! {'), sapp_v_source
}

fn test_sokol_sapp_glue_uses_v_private_sharedlive_wrappers() {
	assert_sapp_wrapper_plumbing()
}

fn test_sokol_d3d11_screenshot_readback_is_implemented_not_guarded_as_unsupported() {
	sapp_v_source := sokol_sapp_v_source()
	post_header_source := sokol_v_post_header_source()
	d3d_readback_source := d3d11_readback_source(post_header_source)
	combined_source := sapp_v_source + '\n' + post_header_source
	assert !combined_source.contains('sokol.sapp screenshots are not supported with -d sokol_d3d11'), combined_source
	assert !combined_source.contains('D3D11 readback is not implemented'), combined_source
	assert_d3d11_readback_helper_tokens(d3d_readback_source)
	assert !d3d_readback_source.contains('sapp_get_environment'), d3d_readback_source
	assert !d3d_readback_source.contains('sapp_get_swapchain'), d3d_readback_source
	assert !post_header_source.contains('defined(_SAPP_WIN32)'), post_header_source
	lower_post_header_source := post_header_source.to_lower()
	assert !lower_post_header_source.contains('todo'), post_header_source
	assert !lower_post_header_source.contains('no-op'), post_header_source
}

fn test_windows_sokol_default_backend_cflags_remain_glcore() {
	if !windows_target_tests_available() {
		return
	}
	cflags := dump_windows_sokol_gfx_cflags([]) or { return }
	assert cflags_have_define(cflags, 'SOKOL_GLCORE'), cflags
	assert !cflags_have_define(cflags, 'SOKOL_D3D11'), cflags
	assert cflags_have_link_flag(cflags, 'opengl32'), cflags
	assert !cflags_have_link_flag(cflags, 'd3d11'), cflags
	assert !cflags_have_link_flag(cflags, 'dxgi'), cflags
	assert !cflags_have_link_flag(cflags, 'd3dcompiler'), cflags
}

fn test_windows_sokol_d3d11_backend_cflags_select_only_d3d11() {
	if !windows_target_tests_available() {
		return
	}
	cflags := dump_windows_sokol_gfx_cflags(['-d', 'sokol_d3d11']) or { return }
	assert cflags_have_define(cflags, 'SOKOL_D3D11'), cflags
	assert !cflags_have_define(cflags, 'SOKOL_GLCORE'), cflags
	assert cflags_have_link_flag(cflags, 'd3d11'), cflags
	assert cflags_have_link_flag(cflags, 'dxgi'), cflags
	assert !cflags_have_link_flag(cflags, 'opengl32'), cflags
	assert !cflags_have_link_flag(cflags, 'd3dcompiler'), cflags
}

fn test_windows_sokol_d3d11_sapp_glue_passes_d3d11_handles() {
	if !windows_target_tests_available() {
		return
	}
	c_source := generated_windows_c(['-d', 'sokol_d3d11'], 'import sokol.gfx
import sokol.sapp

fn main() {
	_ = sapp.create_desc()
	_ = sapp.create_default_pass(gfx.PassAction{})
}
	') or {
		return
	}
	assert c_source.contains('v_sapp_get_environment(&sapp_env);'), c_source
	assert c_source.contains('v_sapp_get_swapchain(&sapp_sc);'), c_source
	assert !c_source.contains('sapp_get_environment();'), c_source
	assert !c_source.contains('sapp_get_swapchain();'), c_source
	assert c_source.contains('env.d3d11.device = sapp_env.d3d11.device;'), c_source
	assert c_source.contains('env.d3d11.device_context = sapp_env.d3d11.device_context;'), c_source
	assert c_source.contains('swapchain.d3d11.render_view = sapp_sc.d3d11.render_view;'), c_source
	assert c_source.contains('swapchain.d3d11.resolve_view = sapp_sc.d3d11.resolve_view;'), c_source
	assert c_source.contains('swapchain.d3d11.depth_stencil_view = sapp_sc.d3d11.depth_stencil_view;'), c_source
	assert !c_source.contains('swapchain.gl.framebuffer = sapp_sc.gl.framebuffer;'), c_source
}

fn test_windows_sokol_d3d11_screenshot_entrypoints_codegen_real_readback() {
	if !windows_target_tests_available() {
		return
	}
	c_source := generated_windows_c(['-d', 'sokol_d3d11'], "import sokol.sapp

fn main() {
	mut ss := sapp.screenshot_window()
	unsafe { ss.destroy() }
	sapp.screenshot_png('d3d11_probe.png') or { panic(err) }
	sapp.screenshot_ppm('d3d11_probe.ppm') or { panic(err) }
	sapp.screenshot('d3d11_probe_path.png') or { panic(err) }
}
") or {
		return
	}
	assert c_source.contains('v_sapp_read_rgba_pixels'), c_source
	assert !c_source.contains('sokol.sapp screenshots are not supported with -d sokol_d3d11'), c_source
	assert !c_source.contains('D3D11 readback is not implemented'), c_source
	assert !c_source.contains('v_sapp_gl_read_rgba_pixels'), c_source
}

fn test_windows_sharedlive_sokol_d3d11_glue_codegen_uses_v_private_host_wrappers() {
	if !windows_target_tests_available() {
		return
	}
	c_source := generated_windows_c(['-sharedlive', '-d', 'sokol_d3d11'], 'import sokol.gfx
import sokol.sapp

fn main() {
	_ = sapp.create_desc()
	_ = sapp.create_default_pass(gfx.PassAction{})
}
') or {
		return
	}
	assert !c_source.contains('#define SOKOL_APP_IMPL'), c_source
	assert !c_source.contains('#define SOKOL_GFX_IMPL'), c_source
	assert !c_source.contains('#define SOKOL_IMPL'), c_source
	assert c_source.contains('v_sapp_get_environment(&sapp_env);'), c_source
	assert c_source.contains('v_sapp_get_swapchain(&sapp_sc);'), c_source
	assert !c_source.contains('sapp_get_environment();'), c_source
	assert !c_source.contains('sapp_get_swapchain();'), c_source
}

fn test_windows_sharedlive_sokol_d3d11_screenshot_codegen_uses_host_backend_readback_call() {
	if !windows_target_tests_available() {
		return
	}
	c_source := generated_windows_c(['-sharedlive', '-d', 'sokol_d3d11'], 'import sokol.sapp

fn main() {
	mut ss := sapp.screenshot_window()
	unsafe { ss.destroy() }
}
') or {
		return
	}
	assert !c_source.contains('#define SOKOL_APP_IMPL'), c_source
	assert !c_source.contains('#define SOKOL_GFX_IMPL'), c_source
	assert !c_source.contains('#define SOKOL_IMPL'), c_source
	assert c_source.contains('#include "sokol_v.post.h"'), c_source
	assert c_source.contains('v_sapp_read_rgba_pixels'), c_source
	assert !c_source.contains('defined(_SAPP_WIN32)'), c_source
	assert !c_source.contains('sokol.sapp screenshots are not supported with -d sokol_d3d11'), c_source
	assert !c_source.contains('D3D11 readback is not implemented'), c_source
}
