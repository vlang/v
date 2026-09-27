// vtest build: !musl? // legacy gg import needs X11/GL headers on Linux
module gg

import os
import time

fn test_multiwindow_enabled_and_disabled_renderer_capability_signatures_match() {
	signature := 'pub fn capabilities_for_backend_with_renderer(backend MultiWindowBackend) !Capabilities'
	enabled_source := os.read_file(os.join_path(@DIR, 'multiwindow_d_gg_multiwindow.v')) or {
		panic(err)
	}
	disabled_source := os.read_file(os.join_path(@DIR, 'multiwindow_notd_gg_multiwindow.v')) or {
		panic(err)
	}

	assert enabled_source.contains(signature)
	assert disabled_source.contains(signature)
	assert !enabled_source.contains('pub fn capabilities_for_backend_with_renderer(backend MultiWindowBackend,')
	assert !disabled_source.contains('pub fn capabilities_for_backend_with_renderer(backend MultiWindowBackend,')
}

fn test_multiwindow_plain_capability_probe_does_not_start_native_app_source_guard() {
	enabled_source := os.read_file(os.join_path(@DIR, 'multiwindow_d_gg_multiwindow.v')) or {
		panic(err)
	}
	plain_source :=
		enabled_source.all_after('// capabilities_for_backend reports capabilities for a backend policy.').all_before('// capabilities_for_backend_with_renderer')
	render_source :=
		enabled_source.all_after('// capabilities_for_backend_with_renderer reports capabilities while requiring').all_before('// create_window creates')

	assert plain_source.contains('multiwindow.capabilities_for_backend(backend_to_core(backend))!')
	assert !plain_source.contains('multiwindow.new_app')
	assert !plain_source.contains('capabilities_for_app_config')
	assert !plain_source.contains('.start(')
	assert !plain_source.contains('require_renderer: true')
	assert render_source.contains('multiwindow.capabilities_for_backend_with_renderer')
	assert render_source.contains('backend_to_core(backend)')
	assert render_source.contains('true')
}

fn test_legacy_import_gg_does_not_pull_multiwindow_core() {
	vlib_dir := os.dir(@DIR)
	unique := 'gg_legacy_import_no_multiwindow_${os.getpid()}_${time.now().unix_nano()}'
	source_path := os.join_path(os.temp_dir(), '${unique}.v')
	source := 'import gg

fn main() {
	ctx := gg.new_context(width: 1, height: 1, create_window: false)
	assert ctx.width == 1
	assert ctx.height == 1
}
'
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
	}

	assert_legacy_host_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_host')
	assert_legacy_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_windows_tcc',
		'-os windows -cc tcc')
	assert_legacy_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_darwin',
		'-os macos -cc clang')
}

fn test_legacy_context_event_source_has_no_multiwindow_input_dependency() {
	legacy_source := os.read_file(os.join_path(@DIR, 'gg.c.v')) or { panic(err) }

	assert !legacy_source.contains('x.multiwindow')
	assert !legacy_source.contains('WindowInputEvent')
	assert !legacy_source.contains('drain_input_events')
	assert !legacy_source.contains('input_fn')
}

fn test_legacy_context_callbacks_do_not_pull_multiwindow_input_markers() {
	vlib_dir := os.dir(@DIR)
	unique := 'gg_legacy_callbacks_no_multiwindow_${os.getpid()}_${time.now().unix_nano()}'
	source_path := os.join_path(os.temp_dir(), '${unique}.v')
	mut bin_path := os.join_path(os.temp_dir(), '${unique}_bin')
	$if windows {
		bin_path += '.exe'
	}
	source := 'import gg

fn on_event(e &gg.Event, data voidptr) {
	_ = e
	_ = data
}

fn on_keydown(key gg.KeyCode, modifier gg.Modifier, data voidptr) {
	_ = key
	_ = modifier
	_ = data
}

fn on_keyup(key gg.KeyCode, modifier gg.Modifier, data voidptr) {
	_ = key
	_ = modifier
	_ = data
}

fn on_char(code u32, data voidptr) {
	_ = code
	_ = data
}

fn on_move(x f32, y f32, data voidptr) {
	_ = x
	_ = y
	_ = data
}

fn on_click(x f32, y f32, button gg.MouseButton, data voidptr) {
	_ = x
	_ = y
	_ = button
	_ = data
}

fn on_unclick(x f32, y f32, button gg.MouseButton, data voidptr) {
	_ = x
	_ = y
	_ = button
	_ = data
}

fn main() {
	ctx := gg.new_context(
		width:         1
		height:        1
		create_window: false
		event_fn:      on_event
		keydown_fn:    on_keydown
		keyup_fn:      on_keyup
		char_fn:       on_char
		move_fn:       on_move
		click_fn:      on_click
		unclick_fn:    on_unclick
		scroll_fn:     on_event
		enter_fn:      on_event
		leave_fn:      on_event
		resized_fn:    on_event
		quit_fn:       on_event
	)
	assert ctx.width == 1
	assert ctx.height == 1
}
'
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	assert_legacy_host_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_host')
	assert_legacy_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_windows_tcc',
		'-os windows -cc tcc')
	assert_legacy_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_darwin',
		'-os macos -cc clang')

	if legacy_host_gg_import_available() {
		compile_cmd := '${os.quoted_path(@VEXE)} ${legacy_child_host_v_flags()} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
		compile_result := os.execute(compile_cmd)
		assert compile_result.exit_code == 0, 'legacy callbacks smoke compile failed
command: ${compile_cmd}
exit_code: ${compile_result.exit_code}
output:
${compile_result.output}'

		run_result := os.execute(os.quoted_path(bin_path))
		assert run_result.exit_code == 0, 'legacy callbacks smoke run failed
command: ${bin_path}
exit_code: ${run_result.exit_code}
output:
${run_result.output}'
	} else {
		legacy_skip_host_gg_import_probe('${unique}_host_smoke')
	}
}

fn test_missing_gg_multiwindow_flag_reports_clear_error_without_native_deps() {
	vlib_dir := os.dir(@DIR)
	unique := 'gg_missing_multiwindow_flag_${os.getpid()}_${time.now().unix_nano()}'
	source_path := os.join_path(os.temp_dir(), '${unique}.v')
	mut bin_path := os.join_path(os.temp_dir(), '${unique}_bin')
	$if windows {
		bin_path += '.exe'
	}
	source := 'import gg

fn main() {
	gg.new_app() or {
		assert err.msg() == "gg.multiwindow: compile with `-d gg_multiwindow` to enable gg.App"
		println(err.msg())
		return
	}
	assert false, "gg.new_app unexpectedly succeeded without -d gg_multiwindow"
}
'
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	assert_legacy_host_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_host')
	assert_legacy_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_windows_tcc',
		'-os windows -cc tcc')
	assert_legacy_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_darwin',
		'-os macos -cc clang')

	if legacy_host_gg_import_available() {
		compile_cmd := '${os.quoted_path(@VEXE)} ${legacy_child_host_v_flags()} -subsystem console -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
		compile_result := os.execute(compile_cmd)
		assert compile_result.exit_code == 0, 'missing gg_multiwindow flag smoke compile failed
command: ${compile_cmd}
exit_code: ${compile_result.exit_code}
output:
${compile_result.output}'

		run_result := os.execute(os.quoted_path(bin_path))
		assert run_result.exit_code == 0, 'missing gg_multiwindow flag smoke run failed
command: ${bin_path}
exit_code: ${run_result.exit_code}
output:
${run_result.output}'

		assert run_result.output.contains('compile with `-d gg_multiwindow`'), 'missing gg_multiwindow flag smoke run did not report the expected opt-in hint
command: ${bin_path}
exit_code: ${run_result.exit_code}
expected output to contain: compile with `-d gg_multiwindow`
output:
${run_result.output}'
	} else {
		legacy_skip_host_gg_import_probe('${unique}_host_smoke')
	}
}

fn test_multiwindow_api_without_define_reports_opt_in_error() {
	vlib_dir := os.dir(@DIR)
	unique := 'gg_disabled_multiwindow_api_${os.getpid()}_${time.now().unix_nano()}'
	source_path := os.join_path(os.temp_dir(), '${unique}.v')
	mut bin_path := os.join_path(os.temp_dir(), unique)
	$if windows {
		bin_path += '.exe'
	}
	source := "import gg

const expected = 'gg.multiwindow: compile with `-d gg_multiwindow` to enable gg.App'

fn expect_opt_in_error(err IError) {
	assert err.msg() == expected
}

fn main() {
	gg.capabilities_for_backend(.auto) or { expect_opt_in_error(err) }
	gg.capabilities_for_backend_with_renderer(.auto) or { expect_opt_in_error(err) }
	gg.new_app() or { expect_opt_in_error(err) }
	gg.new_app(require_renderer: true) or { expect_opt_in_error(err) }
	mut app := gg.App{}
	app.create_window(gg.WindowConfig{ title: 'disabled' }) or { expect_opt_in_error(err) }
	id := gg.WindowId{}
	app.set_window_title(id, 'disabled') or { expect_opt_in_error(err) }
	app.resize_window(id, 1, 1) or { expect_opt_in_error(err) }
	app.set_window_cursor(id, gg.WindowCursorShape.pointer) or { expect_opt_in_error(err) }
	app.begin_window_move(id) or { expect_opt_in_error(err) }
	app.begin_window_resize(id, gg.WindowResizeEdge.bottom_right) or { expect_opt_in_error(err) }
	app.window_info(id) or { expect_opt_in_error(err) }
	app.window_ids() or { expect_opt_in_error(err) }
	app.window_infos() or { expect_opt_in_error(err) }
	app.drain_events() or { expect_opt_in_error(err) }
	app.drain_input_events() or { expect_opt_in_error(err) }
	app.poll_events() or { expect_opt_in_error(err) }
	app.drain_pending(1) or { expect_opt_in_error(err) }
	app.run(event_fn: fn (_ gg.WindowEvent, mut _ gg.App) ! {}) or { expect_opt_in_error(err) }
	app.run(input_fn: fn (_ gg.WindowInputEvent, mut _ gg.App) ! {}) or { expect_opt_in_error(err) }
	app.stop() or { expect_opt_in_error(err) }
}
"
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(bin_path) or {}
	}

	assert_legacy_host_import_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_host')
	assert_source_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_windows_tcc',
		'-os windows -cc tcc')
	assert_source_has_no_multiwindow_markers(vlib_dir, source_path, '${unique}_darwin',
		'-os macos -cc clang')

	if legacy_host_gg_import_available() {
		cmd := '${os.quoted_path(@VEXE)} ${legacy_child_host_v_flags()} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(bin_path)} ${os.quoted_path(source_path)}'
		result := os.execute(cmd)
		assert result.exit_code == 0, 'disabled gg multiwindow API smoke failed
command: ${cmd}
exit_code: ${result.exit_code}
output:
${result.output}'

		run_result := os.execute(os.quoted_path(bin_path))
		assert run_result.exit_code == 0, 'disabled gg multiwindow API smoke failed at runtime
command: ${bin_path}
exit_code: ${run_result.exit_code}
output:
${run_result.output}'
	} else {
		legacy_skip_host_gg_import_probe('${unique}_host_smoke')
	}
}

fn test_legacy_generated_code_filter_preserves_non_line_worktree_paths() {
	worktree_dir := os.dir(os.dir(@DIR))
	generated := '#line 1 "${worktree_dir}/vlib/x/multiwindow/app.v"
#include "${worktree_dir}/vlib/builtin/gc_debugger_linux.h"
#include "${worktree_dir}/vlib/x/multiwindow/wayland_backend.c.v"
'
	filtered := legacy_generated_code_without_path_metadata(generated)

	assert !filtered.contains('#line ')
	assert !filtered.contains(worktree_dir)
	assert filtered.contains('#include "<worktree>/vlib/builtin/gc_debugger_linux.h"')
	assert filtered.contains('vlib/x/multiwindow/wayland_backend.c.v')
}

fn assert_legacy_import_has_no_multiwindow_markers(vlib_dir string, source_path string, label string, target_args string) {
	assert_source_has_no_multiwindow_markers(vlib_dir, source_path, label, target_args)
}

fn assert_legacy_host_import_has_no_multiwindow_markers(vlib_dir string, source_path string, label string) {
	if !legacy_host_gg_import_available() {
		legacy_skip_host_gg_import_probe(label)
		return
	}
	assert_source_has_no_multiwindow_markers(vlib_dir, source_path, label, '')
}

fn assert_source_has_no_multiwindow_markers(vlib_dir string, source_path string, label string, target_args string) {
	c_path := os.join_path(os.temp_dir(), '${label}.c')
	defer {
		os.rm(c_path) or {}
	}
	v_flags := if target_args == '' { legacy_child_host_v_flags() } else { target_args }
	cmd := '${os.quoted_path(@VEXE)} ${v_flags} -b c -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}'
	result := os.execute(cmd)
	assert result.exit_code == 0, 'legacy gg import C generation failed
command: ${cmd}
exit_code: ${result.exit_code}
output:
${result.output}'

	generated := legacy_generated_code_without_path_metadata(os.read_file(c_path) or { panic(err) })
	for forbidden in legacy_multiwindow_forbidden_markers() {
		assert !generated.contains(forbidden), '${label} legacy gg import leaked `${forbidden}` into generated C'
	}
}

fn legacy_generated_code_without_path_metadata(generated string) string {
	worktree_dir := os.dir(os.dir(@DIR))
	worktree_dir_slashes := worktree_dir.replace('\\', '/')
	mut lines := []string{cap: generated.len / 80}
	for line in generated.split_into_lines() {
		if line.starts_with('#line ') {
			continue
		}
		mut cleaned := line.replace(worktree_dir, '<worktree>')
		if worktree_dir_slashes != worktree_dir {
			cleaned = cleaned.replace(worktree_dir_slashes, '<worktree>')
		}
		lines << cleaned
	}
	return lines.join('\n')
}

fn legacy_child_host_v_flags() string {
	mut flags := ''
	$if gcc {
		flags += ' -cc gcc'
	}
	$if msvc {
		flags += ' -cc msvc'
	}
	return flags
}

fn legacy_host_gg_import_available() bool {
	$if linux {
		for header in [
			'X11/Xlib.h',
			'X11/extensions/Xrandr.h',
			'X11/extensions/XInput2.h',
			'X11/Xcursor/Xcursor.h',
			'GL/gl.h',
		] {
			if !legacy_c_header_available(header) {
				return false
			}
		}
	}
	return true
}

fn legacy_c_header_available(header string) bool {
	header_label := header.replace('/', '_').replace('.', '_')
	source_path := os.join_path(os.temp_dir(), 'gg_legacy_header_${os.getpid()}_${header_label}.c')
	os.write_file(source_path, '#include <${header}>\n') or { return false }
	defer {
		os.rm(source_path) or {}
	}
	cc := if os.getenv('CC') == '' { 'cc' } else { os.getenv('CC') }
	result := os.execute('${cc} -fsyntax-only ${os.quoted_path(source_path)}')
	return result.exit_code == 0
}

fn legacy_skip_host_gg_import_probe(label string) {
	eprintln('skip ${label}: legacy gg host import requires host X11/GL headers')
}

fn legacy_multiwindow_forbidden_markers() []string {
	return [
		'x.multiwindow',
		'vlib/x/multiwindow',
		'multiwindow__App',
		'multiwindow__Backend',
		'multiwindow__InputEvent',
		'multiwindow__InputEventKind',
		'multiwindow__QueuedEvent',
		'X11Backend',
		'WaylandBackend',
		'AppKitBackend',
		'Win32Backend',
		'v_multiwindow_',
		'win32_backend_helpers.h',
		'win32_d3d11_backend_helpers.h',
		'x11_backend.c.v',
		'wayland_backend.c.v',
		'appkit_backend.c.v',
		'win32_backend.c.v',
	]
}
