module gg

import os
import time
import gg.testdata.multiwindow_probe_watchdog

const render_runtime_opt_in_error = 'gg.multiwindow: compile with `-d gg_multiwindow` to enable gg.App'
const render_runtime_probe_timeout = 30 * time.second

fn test_multiwindow_render_runtime_enabled_disabled_public_manifests_match() {
	enabled := render_runtime_public_manifest(render_runtime_facade_source(true))
	disabled := render_runtime_public_manifest(render_runtime_facade_source(false))

	assert render_runtime_duplicate_declarations(enabled).len == 0, 'enabled facade contains duplicate declarations: ${render_runtime_duplicate_declarations(enabled)}'

	assert render_runtime_duplicate_declarations(disabled).len == 0, 'disabled facade contains duplicate declarations: ${render_runtime_duplicate_declarations(disabled)}'

	assert enabled == disabled, render_runtime_manifest_diff(enabled, disabled)
}

fn test_multiwindow_render_runtime_manifest_discovers_every_public_source() {
	enabled_paths := render_runtime_facade_source_paths(true)
	disabled_paths := render_runtime_facade_source_paths(false)
	for required_shared in ['multiwindow_render_api.c.v', 'multiwindow_render_errors.v',
		'multiwindow_render_sgl.c.v', 'multiwindow_render_types.v'] {
		path := os.join_path(@DIR, required_shared)
		assert path in enabled_paths, 'enabled manifest omitted shared public file `${required_shared}`'
		assert path in disabled_paths, 'disabled manifest omitted shared public file `${required_shared}`'
	}
	for required_enabled in ['multiwindow_d_gg_multiwindow.v',
		'multiwindow_render_impl_d_gg_multiwindow.v',
		'multiwindow_render_lifecycle_d_gg_multiwindow.v',
		'multiwindow_render_state_d_gg_multiwindow.v', 'multiwindow_render_stub_d_gg_multiwindow.v',
		'multiwindow_resource_impl_d_gg_multiwindow.v',
		'multiwindow_resource_state_d_gg_multiwindow.v'] {
		path := os.join_path(@DIR, required_enabled)
		assert path in enabled_paths, 'enabled manifest omitted flag source `${required_enabled}`'
		assert path !in disabled_paths
	}
	for required_disabled in ['multiwindow_notd_gg_multiwindow.v',
		'multiwindow_render_stub_notd_gg_multiwindow.v'] {
		path := os.join_path(@DIR, required_disabled)
		assert path in disabled_paths, 'disabled manifest omitted flag source `${required_disabled}`'
		assert path !in enabled_paths
	}
	for path in render_runtime_production_source_paths() {
		name := os.file_name(path)
		if name.contains('_notd_gg_multiwindow.v') {
			assert path in disabled_paths
			assert path !in enabled_paths
		} else if name.contains('_d_gg_multiwindow.v') {
			assert path in enabled_paths
			assert path !in disabled_paths
		} else {
			assert path in enabled_paths, 'enabled manifest omitted shared public file `${name}`'
			assert path in disabled_paths, 'disabled manifest omitted shared public file `${name}`'
		}
	}
}

fn test_multiwindow_render_runtime_complete_consumer_names_every_public_declaration() {
	fixture := os.read_file(os.join_path(@DIR, 'testdata', 'multiwindow_render_runtime_consumer.v')) or {
		panic(err)
	}
	for raw_declaration in render_runtime_public_manifest(render_runtime_facade_source(true)) {
		declaration := render_runtime_without_attributes(raw_declaration)
		if declaration.starts_with('pub enum ') || declaration.starts_with('pub struct ')
			|| declaration.starts_with('pub type ') {
			name := declaration.fields()[2]
			assert fixture.contains('gg.${name}'), 'complete consumer omitted public declaration `gg.${name}`'
		} else if declaration.starts_with('pub fn (') {
			method := declaration.all_after(') ').all_before('(')
			assert fixture.contains('.${method}('), 'complete consumer omitted public method `${method}`'
		} else if declaration.starts_with('pub fn ') {
			function_name := declaration.all_after('pub fn ').all_before('(')
			assert fixture.contains('gg.${function_name}('), 'complete consumer omitted public function `gg.${function_name}`'
		}
	}
}

fn test_multiwindow_render_runtime_frozen_declarations_are_present() {
	manifest := render_runtime_public_manifest(render_runtime_facade_source(true)).join('\n')
	required := [
		'pub enum WindowRedrawMode',
		'pub enum WindowCleanupReason',
		'pub enum WindowReadbackStatus',
		'pub struct WindowLogicalSize',
		'pub struct WindowPixelSize',
		'pub struct WindowLogicalRect',
		'pub struct WindowPixelRect',
		'pub struct WindowReadbackCapabilities',
		'pub struct WindowMetrics',
		'pub struct WindowRenderTargetInfo',
		'pub struct WindowFrameInfo',
		'pub struct WindowBufferId',
		'pub struct WindowImageId',
		'pub struct WindowSamplerId',
		'pub struct WindowShaderId',
		'pub struct WindowPipelineId',
		'pub struct WindowAttachmentsId',
		'pub struct WindowSglPipelineId',
		'pub struct WindowReadbackId',
		'pub struct WindowOffscreenPassConfig',
		'pub struct WindowAttachmentsConfig',
		'pub struct WindowBufferBinding',
		'pub struct WindowImageBinding',
		'pub struct WindowSamplerBinding',
		'pub struct WindowStageBindings',
		'pub struct WindowBindings',
		'pub struct WindowReadbackConfig',
		'pub struct WindowReadbackResult',
		'pub struct WindowInitContext',
		'pub struct WindowContext',
		'pub struct WindowCleanupContext',
		'pub struct WindowResourceContext',
		'pub struct WindowPassContext',
		'pub struct WindowSglContext',
		'pub struct NativeWindowLease',
		'pub type AppResourceContext = WindowResourceContext',
		'pub type WindowInitFn',
		'pub type WindowFrameFn',
		'pub type WindowCleanupFn',
		'pub type AppResourceInitFn',
		'pub type AppResourceFrameFn',
		'pub type AppResourceCleanupFn',
		'pub type WindowReadbackFn',
		'pub type WindowResourceFn',
		'pub type WindowPassFn',
		'pub type WindowSglFn',
		'pub type NativeWindowBorrowFn',
		'request_redraw(',
		'window_metrics(',
		'window_render_target_info(',
		'set_window_clear_color(',
		'window_readback_capabilities(',
		'request_window_capture(',
		'frame_info(',
		'logical_to_pixel_rect(',
		'pixel_to_logical_rect(',
		'with_resources(',
		'with_offscreen(',
		'with_swapchain(',
		'with_offscreen_sgl(',
		'with_swapchain_sgl(',
		'request_image_readback(',
		'graphics_available(',
		'with_native_window(',
		'make_buffer(',
		'make_image(',
		'make_sampler(',
		'make_shader(',
		'make_pipeline(',
		'make_attachments(',
		'make_sgl_pipeline(',
		'make_sgl_pipeline_with_shader(',
		'update_buffer(',
		'append_buffer(',
		'update_image(',
		'replace_image(',
		'retire_buffer(',
		'retire_image(',
		'retire_sampler(',
		'retire_shader(',
		'retire_pipeline(',
		'retire_attachments(',
		'retire_sgl_pipeline(',
		'apply_pipeline(',
		'apply_bindings(',
		'apply_uniforms(',
		'draw(',
		'apply_viewport(',
		'apply_scissor(',
	]
	for declaration in required {
		assert manifest.contains(declaration), 'missing frozen gg multi-window declaration `${declaration}`'
	}
}

fn test_multiwindow_render_runtime_frozen_sgl_surface_is_exact() {
	manifest := render_runtime_public_manifest(render_runtime_facade_source(true))
	mut actual := []string{}
	for declaration in manifest {
		if declaration.contains('pub fn (mut context WindowSglContext) ') {
			actual << declaration.all_after('pub fn (mut context WindowSglContext) ').all_before('(')
		}
	}
	mut expected := render_runtime_frozen_sgl_methods()
	actual.sort()
	expected.sort()
	assert actual == expected, 'frozen WindowSglContext surface differs\nactual: ${actual}\nexpected: ${expected}'
}

fn test_multiwindow_render_runtime_public_manifests_exclude_raw_render_authority() {
	gg_manifest := render_runtime_public_manifest(render_runtime_facade_source(true)).join('\n')
	x_manifest := render_runtime_public_manifest(render_runtime_x_source()).join('\n')
	for forbidden in ['window_context(', 'gfx.Environment', 'gfx.Swapchain', 'RenderFrame',
		'sgl.Context', 'begin_render(', 'end_render(', 'abort_render(', 'gfx.commit(', 'present('] {
		assert !gg_manifest.contains(forbidden), 'gg public facade exposed forbidden `${forbidden}` authority'
		assert !x_manifest.contains(forbidden), 'x.multiwindow public facade exposed forbidden `${forbidden}` authority'
	}
}

fn test_multiwindow_render_runtime_consumer_compiles_enabled_and_disabled() {
	vlib_dir := os.dir(@DIR)
	fixture := os.join_path(@DIR, 'testdata', 'multiwindow_render_runtime_consumer.v')
	for enabled in [false, true] {
		mode := if enabled { 'enabled' } else { 'disabled' }
		output := render_runtime_temp_binary('gg_render_runtime_${mode}')
		defer {
			os.rm(output) or {}
		}
		flags := render_runtime_child_flags(enabled)
		cmd := '${os.quoted_path(@VEXE)}${flags} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(output)} ${os.quoted_path(fixture)}'
		result := render_runtime_execute_child(cmd, enabled)
		assert result.exit_code == 0, 'package-1 ${mode} consumer compile failed\ncommand: ${cmd}\noutput:\n${result.output}'
	}
}

fn test_multiwindow_render_runtime_disposable_consumer_probes_compile() {
	for probe in render_runtime_disposable_probe_paths() {
		output := render_runtime_temp_binary('gg_${os.file_name(probe).all_before_last('.')}')
		defer {
			os.rm(output) or {}
		}
		render_runtime_compile_program(probe, output, true)
	}
}

fn test_multiwindow_render_runtime_checked_in_example_compiles() {
	example := os.join_path(os.dir(os.dir(@DIR)), 'examples', 'gg', 'multiwindow_render_runtime.v')
	output := render_runtime_temp_binary('gg_multiwindow_render_runtime_example')
	defer {
		os.rm(output) or {}
	}
	render_runtime_compile_program(example, output, true)
}

fn test_multiwindow_probe_watchdog_rejects_invalid_public_config_before_spawn() {
	assert_watchdog_config_rejected(multiwindow_probe_watchdog.Config{
		timeout:    time.second
		start_file: 'unused-gate'
	}, 'multi-window probe watchdog: executable is required')
	assert_watchdog_config_rejected(multiwindow_probe_watchdog.Config{
		executable: 'must-not-spawn'
		start_file: 'unused-gate'
	}, 'multi-window probe watchdog: timeout must be positive')
	assert_watchdog_config_rejected(multiwindow_probe_watchdog.Config{
		executable: 'must-not-spawn'
		timeout:    time.second
	}, 'multi-window probe watchdog: a bounded parent start gate is required')
}

fn assert_watchdog_config_rejected(config multiwindow_probe_watchdog.Config, expected string) {
	mut rejected := false
	multiwindow_probe_watchdog.run(config) or {
		assert err.msg() == expected
		rejected = true
	}
	assert rejected, 'watchdog accepted invalid public configuration'
}

fn test_multiwindow_probe_watchdog_kills_descendants_and_reaps_probe() {
	helper := os.join_path(@DIR, 'testdata', 'multiwindow_probe_watchdog_child.v')
	output := render_runtime_temp_binary('gg_multiwindow_watchdog_child')
	defer {
		os.rm(output) or {}
	}
	render_runtime_compile_program(helper, output, false)
	assert_watchdog_clean_success(output)!
	assert_watchdog_natural_failure(output)!
	assert_watchdog_descendant_cleanup(output, 'root', 2 * time.second, true)!
	assert_watchdog_descendant_cleanup(output, 'root_exits', 5 * time.second, false)!
}

fn assert_watchdog_clean_success(executable string) ! {
	gate_path := render_runtime_temp_binary('gg_multiwindow_watchdog_clean_gate')
	defer {
		os.rm(gate_path) or {}
	}
	result := multiwindow_probe_watchdog.run(
		executable: executable
		args:       ['clean', gate_path]
		timeout:    5 * time.second
		start_file: gate_path
	)!
	assert !result.timed_out, 'clean watchdog child timed out\n${result.combined_output()}'
	assert result.reaped, 'clean watchdog child leader was not reaped'
	assert result.confinement_empty, 'clean watchdog child confinement was not proven empty'
	assert result.exit_code == 0, 'clean watchdog child exited with ${result.exit_code}\n${result.combined_output()}'
	assert !result.forced_cleanup, 'clean watchdog child required forced cleanup'
	actual := render_runtime_last_nonempty_line(result.stdout)
	expected := '{"probe":"watchdog_self_test","status":"PASS","cleanup":"complete"}'
	assert actual == expected, 'clean watchdog child final record mismatch: `${actual}`'
}

fn assert_watchdog_natural_failure(executable string) ! {
	gate_path := render_runtime_temp_binary('gg_multiwindow_watchdog_failure_gate')
	defer {
		os.rm(gate_path) or {}
	}
	result := multiwindow_probe_watchdog.run(
		executable: executable
		args:       ['failure', gate_path]
		timeout:    5 * time.second
		start_file: gate_path
	)!
	assert !result.timed_out, 'failing watchdog child timed out\n${result.combined_output()}'
	assert result.reaped, 'failing watchdog child leader was not reaped'
	assert result.confinement_empty, 'failing watchdog child confinement was not proven empty'
	assert result.exit_code == 37, 'failing watchdog child exited with ${result.exit_code}\n${result.combined_output()}'
	assert !result.forced_cleanup, 'natural nonzero watchdog exit was misclassified as forced cleanup'
	actual := render_runtime_last_nonempty_line(result.stdout)
	assert actual == 'watchdog intentional failure', 'failing watchdog child output was not drained: `${actual}`'
}

fn assert_watchdog_descendant_cleanup(executable string, mode string, timeout time.Duration, expect_timeout bool) ! {
	gate_path := render_runtime_temp_binary('gg_multiwindow_watchdog_${mode}_gate')
	pid_path := render_runtime_temp_binary('gg_multiwindow_watchdog_${mode}_descendant_pid')
	started_path := render_runtime_temp_binary('gg_multiwindow_watchdog_${mode}_descendant_started')
	defer {
		os.rm(gate_path) or {}
		os.rm(pid_path) or {}
		os.rm(started_path) or {}
	}
	result := multiwindow_probe_watchdog.run(
		executable: executable
		args:       [mode, gate_path, pid_path, started_path]
		timeout:    timeout
		start_file: gate_path
	)!
	assert result.timed_out == expect_timeout, 'watchdog `${mode}` timeout state was ${result.timed_out}, expected ${expect_timeout}\n${result.combined_output()}'
	if !expect_timeout {
		assert result.exit_code == 0, 'watchdog `${mode}` leader failed with ${result.exit_code}\n${result.combined_output()}'
	}
	assert result.reaped, 'watchdog did not reap `${mode}` probe leader ${result.pid}'
	assert result.confinement_empty, 'watchdog did not prove `${mode}` confinement empty'
	assert result.forced_cleanup, 'watchdog `${mode}` did not report its required forced cleanup'
	assert multiwindow_probe_watchdog.wait_until_process_gone(result.pid, 2 * time.second), 'watchdog probe leader ${result.pid} remains alive'

	assert os.exists(pid_path), 'watchdog descendant pid handshake was not written'
	assert os.exists(started_path), 'watchdog descendant did not start'
	descendant_pid := os.read_file(pid_path)!.trim_space().int()
	assert descendant_pid > 0
	assert multiwindow_probe_watchdog.wait_until_process_gone(descendant_pid, 2 * time.second), 'watchdog descendant ${descendant_pid} remains alive'
}

fn test_multiwindow_render_runtime_disposable_consumer_probes_run_when_requested() {
	if os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') != '1' {
		eprintln('multi-window runtime probes are not closure proof unless CI forces VGG_MULTIWINDOW_RUNTIME_PROBES=1; this invocation did not run them')
		return
	}
	backend := os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND')
	assert backend in ['x11', 'wayland', 'appkit', 'win32'], 'VGG_MULTIWINDOW_RUNTIME_BACKEND must select x11, wayland, appkit, or win32'

	for probe in render_runtime_disposable_probe_paths() {
		output := render_runtime_temp_binary('gg_run_${os.file_name(probe).all_before_last('.')}')
		gate_path :=
			render_runtime_temp_binary('gg_run_${os.file_name(probe).all_before_last('.')}_gate')
		defer {
			os.rm(output) or {}
			os.rm(gate_path) or {}
		}
		render_runtime_compile_program(probe, output, true)
		result := multiwindow_probe_watchdog.run(
			executable:  output
			timeout:     render_runtime_probe_timeout
			start_file:  gate_path
			environment: {
				'V_MULTIWINDOW_PROBE_BACKEND': backend
			}
		)!
		assert !result.timed_out, 'runtime consumer probe `${os.file_name(probe)}` timed out after ${render_runtime_probe_timeout}\n${result.combined_output()}'
		assert result.reaped, 'runtime consumer probe `${os.file_name(probe)}` was not reaped'
		assert result.confinement_empty, 'runtime consumer probe `${os.file_name(probe)}` confinement was not proven empty'
		assert result.exit_code == 0, 'runtime consumer probe `${os.file_name(probe)}` failed with exit code ${result.exit_code}\n${result.combined_output()}'
		assert !result.forced_cleanup, 'runtime consumer probe `${os.file_name(probe)}` required forced cleanup'
		expected := render_runtime_probe_pass_record(probe)
		actual := render_runtime_last_nonempty_line(result.stdout)
		assert actual == expected, 'runtime consumer probe `${os.file_name(probe)}` did not finish with its post-cleanup PASS record\nexpected: ${expected}\nactual: ${actual}\n${result.combined_output()}'
	}
}

fn test_multiwindow_render_runtime_dynamic_lifecycle_probes_run_when_requested() {
	if os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') != '1' {
		eprintln('multi-window dynamic lifecycle probes require VGG_MULTIWINDOW_RUNTIME_PROBES=1; this invocation did not run them')
		return
	}
	backend := os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND')
	assert backend in ['x11', 'wayland', 'appkit', 'win32'], 'VGG_MULTIWINDOW_RUNTIME_BACKEND must select x11, wayland, appkit, or win32'

	source_path := '${render_runtime_temp_binary('gg_dynamic_lifecycle_probe')}.v'
	output := render_runtime_temp_binary('gg_dynamic_lifecycle_probe')
	os.write_file(source_path, render_runtime_dynamic_lifecycle_probe_source()) or { panic(err) }
	defer {
		os.rm(source_path) or {}
		os.rm(output) or {}
	}
	render_runtime_compile_program(source_path, output, true)

	for mode in ['dynamic_job', 'init_only', 'continuous_counter', 'mixed_event'] {
		gate_path := render_runtime_temp_binary('gg_dynamic_lifecycle_${mode}_gate')
		defer {
			os.rm(gate_path) or {}
		}
		result := multiwindow_probe_watchdog.run(
			executable:  output
			args:        [mode]
			timeout:     render_runtime_probe_timeout
			start_file:  gate_path
			environment: {
				'V_MULTIWINDOW_PROBE_BACKEND': backend
			}
		)!
		assert !result.timed_out, 'dynamic lifecycle probe `${mode}` timed out after ${render_runtime_probe_timeout}\n${result.combined_output()}'
		assert result.reaped, 'dynamic lifecycle probe `${mode}` was not reaped'
		assert result.confinement_empty, 'dynamic lifecycle probe `${mode}` confinement was not proven empty'
		assert result.exit_code == 0, 'dynamic lifecycle probe `${mode}` failed with exit code ${result.exit_code}\n${result.combined_output()}'
		assert !result.forced_cleanup, 'dynamic lifecycle probe `${mode}` required forced cleanup'
		expected := '{"probe":"dynamic_lifecycle","mode":"${mode}","status":"PASS","cleanup":"complete"}'
		actual := render_runtime_last_nonempty_line(result.stdout)
		assert actual == expected, 'dynamic lifecycle probe `${mode}` final record mismatch\nexpected: ${expected}\nactual: ${actual}\n${result.combined_output()}'
	}
}

fn test_multiwindow_render_runtime_no_flag_generated_c_is_isolated() {
	vlib_dir := os.dir(@DIR)
	fixtures := {
		'legacy':   os.join_path(@DIR, 'testdata', 'multiwindow_render_runtime_legacy_consumer.v')
		'disabled': os.join_path(@DIR, 'testdata', 'multiwindow_render_runtime_consumer.v')
	}
	for target in render_runtime_cross_targets() {
		for label, fixture in fixtures {
			c_path := '${render_runtime_temp_binary('gg_render_runtime_${target}_${label}')}.c'
			defer {
				os.rm(c_path) or {}
			}
			cmd := '${os.quoted_path(@VEXE)}${render_runtime_child_flags(false)} -os ${target} -b c -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(c_path)} ${os.quoted_path(fixture)}'
			result := render_runtime_execute_child(cmd, false)
			assert result.exit_code == 0, 'no-flag ${target} ${label} consumer C generation failed\ncommand: ${cmd}\noutput:\n${result.output}'
			generated := render_runtime_generated_c_without_paths(os.read_file(c_path) or {
				panic(err)
			})
			for marker in render_runtime_forbidden_no_flag_markers() {
				assert !generated.contains(marker), 'no-flag ${target} ${label} consumer leaked `${marker}` into generated C'
			}
		}
	}
}

fn test_multiwindow_render_runtime_disabled_facade_uses_one_stable_error() {
	$if !gg_multiwindow ? {
		mut app := App{}
		id := WindowId{}
		app.request_redraw(id) or {
			assert err.msg() == render_runtime_opt_in_error
			return
		}
		assert false, 'disabled request_redraw unexpectedly succeeded'
	}
}

fn render_runtime_public_manifest(source string) []string {
	lines := source.split_into_lines()
	mut declarations := []string{}
	mut attributes := []string{}
	mut i := 0
	mut test_block_depth := 0
	for i < lines.len {
		line := render_runtime_source_line(lines[i])
		if test_block_depth > 0 {
			test_block_depth += render_runtime_brace_delta(line)
			i++
			continue
		}
		if line.starts_with('$if test {') {
			test_block_depth = render_runtime_brace_delta(line)
			attributes.clear()
			i++
			continue
		}
		if line == '' {
			i++
			continue
		}
		if line.starts_with('@[') {
			attributes << line
			i++
			continue
		}
		if line.starts_with('pub enum ') || line.starts_with('pub struct ') {
			block, next := render_runtime_braced_declaration(lines, i)
			mut declaration := if line.starts_with('pub enum ') {
				render_runtime_normalize(block.join(' '))
			} else {
				render_runtime_public_struct(block)
			}
			if attributes.len > 0 {
				declaration = '${render_runtime_normalize(attributes.join(' '))} ${declaration}'
			}
			declarations << declaration
			attributes.clear()
			i = next
			continue
		}
		if line.starts_with('pub fn ') {
			signature, next := render_runtime_function_signature(lines, i)
			mut declaration := render_runtime_normalize(signature)
			if attributes.len > 0 {
				declaration = '${render_runtime_normalize(attributes.join(' '))} ${declaration}'
			}
			declarations << declaration
			attributes.clear()
			i = next
			continue
		}
		if line.starts_with('pub type ') {
			mut declaration := render_runtime_normalize(line)
			if attributes.len > 0 {
				declaration = '${render_runtime_normalize(attributes.join(' '))} ${declaration}'
			}
			declarations << declaration
			attributes.clear()
			i++
			continue
		}
		attributes.clear()
		i++
	}
	declarations.sort()
	return declarations
}

fn render_runtime_braced_declaration(lines []string, start int) ([]string, int) {
	mut block := []string{}
	mut depth := 0
	mut opened := false
	mut i := start
	for i < lines.len {
		line := render_runtime_source_line(lines[i])
		block << line
		for ch in line {
			if ch == `{` {
				depth++
				opened = true
			} else if ch == `}` {
				depth--
			}
		}
		i++
		if opened && depth == 0 {
			break
		}
	}
	return block, i
}

fn render_runtime_function_signature(lines []string, start int) (string, int) {
	mut parts := []string{}
	mut i := start
	for i < lines.len {
		line := render_runtime_source_line(lines[i])
		if line.ends_with('{') {
			parts << line[..line.len - 1]
			i++
			break
		}
		parts << line
		i++
	}
	return parts.join(' '), i
}

fn render_runtime_public_struct(block []string) string {
	header := render_runtime_normalize(block[0].all_before('{'))
	mut public_body := []string{}
	mut public_section := false
	mut depth := 0
	for i, raw_line in block {
		line := render_runtime_source_line(raw_line)
		if i == 0 {
			depth += render_runtime_brace_delta(line)
			continue
		}
		if depth == 1 && line == 'pub:' {
			public_section = true
		} else if depth == 1 && line == 'pub mut:' {
			public_section = true
		} else if depth == 1 && (line == 'mut:' || line == 'global:') {
			public_section = false
		} else if public_section && !(depth == 1 && line == '}') && line != '' {
			public_body << line
		}
		depth += render_runtime_brace_delta(line)
	}
	return '${header} { ${render_runtime_normalize(public_body.join(' '))} }'
}

fn render_runtime_brace_delta(line string) int {
	mut delta := 0
	for ch in line {
		if ch == `{` {
			delta++
		} else if ch == `}` {
			delta--
		}
	}
	return delta
}

fn render_runtime_source_line(line string) string {
	trimmed := line.trim_space()
	if trimmed.starts_with('//') {
		return ''
	}
	return trimmed
}

fn render_runtime_normalize(value string) string {
	return value.fields().join(' ')
}

fn render_runtime_without_attributes(declaration string) string {
	index := declaration.index('pub ') or { return declaration }
	return declaration[index..]
}

fn render_runtime_manifest_diff(enabled []string, disabled []string) string {
	mut missing_disabled := []string{}
	mut missing_enabled := []string{}
	for declaration in enabled {
		if declaration !in disabled {
			missing_disabled << declaration
		}
	}
	for declaration in disabled {
		if declaration !in enabled {
			missing_enabled << declaration
		}
	}
	return 'enabled/disabled gg public declarations differ\nmissing from disabled:\n${missing_disabled.join('\n')}\nmissing from enabled:\n${missing_enabled.join('\n')}'
}

fn render_runtime_duplicate_declarations(declarations []string) []string {
	mut seen := map[string]bool{}
	mut duplicates := []string{}
	for declaration in declarations {
		if seen[declaration] && declaration !in duplicates {
			duplicates << declaration
		}
		seen[declaration] = true
	}
	return duplicates
}

fn render_runtime_facade_source(enabled bool) string {
	paths := render_runtime_facade_source_paths(enabled)
	mut sources := []string{cap: paths.len}
	for path in paths {
		sources << render_runtime_read_source(path)
	}
	return sources.join('\n')
}

fn render_runtime_x_source() string {
	x_dir := os.join_path(os.dir(@DIR), 'x', 'multiwindow')
	mut sources := []string{}
	for path in render_runtime_v_source_paths(x_dir, '') {
		sources << render_runtime_read_source(path)
	}
	return sources.join('\n')
}

fn render_runtime_read_source(path string) string {
	source := os.read_file(path) or {
		panic('failed to read render runtime source `${path}`: ${err.msg()}')
	}
	return source.split_into_lines().join('\n')
}

fn render_runtime_facade_source_paths(enabled bool) []string {
	mut paths := []string{}
	for path in render_runtime_production_source_paths() {
		name := os.file_name(path)
		if name.contains('_notd_gg_multiwindow.v') {
			if !enabled {
				paths << path
			}
		} else if name.contains('_d_gg_multiwindow.v') {
			if enabled {
				paths << path
			}
		} else {
			paths << path
		}
	}
	paths.sort()
	return paths
}

fn render_runtime_production_source_paths() []string {
	return render_runtime_v_source_paths(@DIR, 'multiwindow')
}

fn render_runtime_v_source_paths(dir string, name_prefix string) []string {
	entries := os.ls(dir) or {
		panic('failed to list render runtime source directory `${dir}`: ${err.msg()}')
	}
	mut paths := []string{cap: entries.len}
	for entry in entries {
		path := os.join_path(dir, entry)
		if entry.starts_with(name_prefix) && os.is_file(path) && path.ends_with('.v')
			&& !path.ends_with('_test.v') {
			paths << path
		}
	}
	paths.sort()
	if paths.len == 0 {
		panic('render runtime source directory `${dir}` contains no production V sources')
	}
	return paths
}

fn render_runtime_frozen_sgl_methods() []string {
	return [
		'defaults',
		'viewport',
		'scissor_rect',
		'scissor_rectf',
		'enable_texture',
		'disable_texture',
		'texture',
		'load_default_pipeline',
		'default_pipeline',
		'load_pipeline',
		'push_pipeline',
		'pop_pipeline',
		'matrix_mode_modelview',
		'matrix_mode_projection',
		'matrix_mode_texture',
		'load_identity',
		'load_matrix',
		'load_transpose_matrix',
		'mult_matrix',
		'mult_transpose_matrix',
		'rotate',
		'scale',
		'translate',
		'frustum',
		'ortho',
		'perspective',
		'lookat',
		'push_matrix',
		'pop_matrix',
		't2f',
		'c3f',
		'c4f',
		'c3b',
		'c4b',
		'c1i',
		'point_size',
		'begin_points',
		'begin_lines',
		'begin_line_strip',
		'begin_triangles',
		'begin_triangle_strip',
		'begin_quads',
		'v2f',
		'v3f',
		'v2f_t2f',
		'v3f_t2f',
		'v2f_c3f',
		'v2f_c3b',
		'v2f_c4f',
		'v2f_c4b',
		'v2f_c1i',
		'v3f_c3f',
		'v3f_c3b',
		'v3f_c4f',
		'v3f_c4b',
		'v3f_c1i',
		'v2f_t2f_c3f',
		'v2f_t2f_c3b',
		'v2f_t2f_c4f',
		'v2f_t2f_c4b',
		'v2f_t2f_c1i',
		'v3f_t2f_c3f',
		'v3f_t2f_c3b',
		'v3f_t2f_c4f',
		'v3f_t2f_c4b',
		'v3f_t2f_c1i',
		'end',
	]
}

fn render_runtime_temp_binary(label string) string {
	mut path := os.join_path(os.temp_dir(), '${label}_${os.getpid()}_${time.now().unix_nano()}')
	$if windows {
		path += '.exe'
	}
	return path
}

fn render_runtime_disposable_probe_paths() []string {
	return [
		os.join_path(@DIR, 'testdata', 'multiwindow_dynamic_texture_consumer_probe.v'),
		os.join_path(@DIR, 'testdata', 'multiwindow_retained_ui_consumer_probe.v'),
	]
}

fn render_runtime_dynamic_lifecycle_probe_source() string {
	return 'module main

import gg
import os
import sokol.gfx
import time
import gg.testdata.multiwindow_probe_backend
import gg.testdata.multiwindow_probe_gate

const lifecycle_probe_timeout = 5 * time.second

struct LifecycleProof {
mut:
	window_keys             []string
	frames                  map[string]int
	dynamic_created         bool
	global_frames           int
	window_frames           int
	counter                 int
	init_calls              int
	cleanup_calls           int
	init_submitted_frame    u64
	cleanup_submitted_frame u64
}

struct ContinuousFrameSignal {
	frame   int
	counter int
}

fn main() {
	run_lifecycle_probe() or { panic(err) }
}

fn run_lifecycle_probe() ! {
	multiwindow_probe_gate.await_parent_release(lifecycle_probe_timeout)!
	mode := if os.args.len > 1 { os.args[1] } else { \'\' }
	match mode {
		\'dynamic_job\' { run_dynamic_job_probe()! }
		\'init_only\' { run_init_only_probe()! }
		\'continuous_counter\' { run_continuous_counter_probe()! }
		\'mixed_event\' { run_mixed_event_probe()! }
		else { return error(\'unknown dynamic lifecycle mode: \' + mode) }
	}
	println(\'{"probe":"dynamic_lifecycle","mode":"\' + mode + \'","status":"PASS","cleanup":"complete"}\')
}

fn new_lifecycle_probe_app() !&gg.App {
	backend := multiwindow_probe_backend.selected()!
	mut app := gg.new_app(
		backend:          backend
		queue_size:       16
		require_renderer: true
	)!
	multiwindow_probe_backend.validate(app.capabilities(), backend)!
	return app
}

fn run_dynamic_job_probe() ! {
	frames := chan string{cap: 8}
	driver_result := chan string{cap: 1}
	mut proof := &LifecycleProof{}
	mut app := new_lifecycle_probe_app()!
	app.post(fn [frames, mut proof] (mut app gg.App) ! {
		for title in [\'dynamic-left\', \'dynamic-right\'] {
			window := app.create_window(
				title:       title
				width:       160
				height:      120
				redraw_mode: .on_demand
				frame_fn:    fn [frames, mut proof] (mut context gg.WindowContext) ! {
					info := context.frame_info()
					context.with_swapchain(gfx.create_clear_pass_action(0, 0, 0, 0), fn (mut pass gg.WindowPassContext) ! {
						_ = pass
					})!
					key := info.window.str()
					proof.frames[key]++
					if proof.frames[key] == 1 {
						frames <- key
					}
				}
			)!
			proof.window_keys << window.str()
		}
	})!

	driver := spawn stop_after_dynamic_frames(mut app, frames, driver_result)
	mut run_error := \'\'
	app.run(
		event_fn: fn (event gg.WindowEvent, mut app gg.App) ! {
			_ = event
			_ = app
		}
	) or { run_error = err.msg() }
	driver.wait()
	driver_error := <-driver_result
	if run_error != \'\' {
		return error(\'dynamic job owner loop: \' + run_error)
	}
	if driver_error != \'\' {
		return error(\'dynamic job driver: \' + driver_error)
	}
	if proof.window_keys.len != 2 {
		return error(\'dynamic job created \' + proof.window_keys.len.str() + \' render windows\')
	}
	if proof.window_keys[0] == proof.window_keys[1] {
		return error(\'dynamic job returned duplicate window identities\')
	}
	for key in proof.window_keys {
		if proof.frames[key] < 1 {
			return error(\'dynamic window \' + key + \' did not render\')
		}
	}
}

fn stop_after_dynamic_frames(mut app gg.App, frames chan string, result chan string) {
	mut seen := map[string]bool{}
	for seen.len < 2 {
		select {
			key := <-frames {
				seen[key] = true
			}
			lifecycle_probe_timeout {
				post_lifecycle_probe_stop(mut app) or {
					result <- (\'frame timeout; stop admission: \' + err.msg())
					return
				}
				result <- (\'frame timeout with \' + seen.len.str() + \'/2 windows rendered\')
				return
			}
		}
	}
	post_lifecycle_probe_stop(mut app) or {
		result <- (\'stop admission: \' + err.msg())
		return
	}
	result <- \'\'
}

fn post_lifecycle_probe_stop(mut app gg.App) ! {
	app.post(fn (mut app gg.App) ! {
		app.stop()!
	})!
}

fn run_init_only_probe() ! {
	initialized := chan bool{cap: 1}
	driver_result := chan string{cap: 1}
	mut proof := &LifecycleProof{}
	mut app := new_lifecycle_probe_app()!
	_ = app.create_window(
		title:       \'init-only\'
		width:       160
		height:      120
		redraw_mode: .on_demand
		init_fn:     fn [initialized, mut proof] (mut context gg.WindowInitContext) ! {
			proof.init_calls++
			proof.init_submitted_frame = context.metrics().submitted_frame
			if proof.init_calls == 1 {
				initialized <- true
			}
		}
		cleanup_fn:  fn [mut proof] (mut context gg.WindowCleanupContext) ! {
			proof.cleanup_calls++
			proof.cleanup_submitted_frame = context.metrics().submitted_frame
		}
	)!

	driver := spawn stop_after_init_only(mut app, initialized, driver_result)
	mut run_error := \'\'
	app.run() or { run_error = err.msg() }
	driver.wait()
	driver_error := <-driver_result
	if run_error != \'\' {
		return error(\'init-only owner loop: \' + run_error)
	}
	if driver_error != \'\' {
		return error(\'init-only driver: \' + driver_error)
	}
	if proof.init_calls != 1 {
		return error(\'init-only callback count was \' + proof.init_calls.str())
	}
	if proof.cleanup_calls != 1 {
		return error(\'init-only cleanup count was \' + proof.cleanup_calls.str())
	}
	if proof.init_submitted_frame != 0 || proof.cleanup_submitted_frame != 0 {
		return error(\'init-only window submitted a frame\')
	}
	remaining_windows := app.window_ids()!
	if remaining_windows.len != 0 {
		return error(\'init-only cleanup left \' + remaining_windows.len.str() + \' window(s)\')
	}
	app.stop()!
	if proof.init_calls != 1 || proof.cleanup_calls != 1 {
		return error(\'init-only stop replay repeated a callback\')
	}
}

fn stop_after_init_only(mut app gg.App, initialized chan bool, result chan string) {
	mut message := \'\'
	select {
		signal := <-initialized {
			if !signal {
				message = \'init callback returned an invalid signal\'
			}
		}
		lifecycle_probe_timeout {
			message = \'init callback timeout\'
		}
	}
	post_lifecycle_probe_stop(mut app) or {
		prefix := if message == \'\' { \'stop admission\' } else { message + \'; stop admission\' }
		result <- (prefix + \': \' + err.msg())
		return
	}
	result <- message
}

fn run_continuous_counter_probe() ! {
	frames := chan ContinuousFrameSignal{cap: 16}
	driver_result := chan string{cap: 1}
	mut proof := &LifecycleProof{}
	mut app := new_lifecycle_probe_app()!
	_ = app.create_window(
		title:       \'continuous-counter\'
		width:       160
		height:      120
		redraw_mode: .continuous
		frame_fn:    fn [frames, mut proof] (mut context gg.WindowContext) ! {
			context.with_swapchain(gfx.create_clear_pass_action(0, 0, 0, 0), fn (mut pass gg.WindowPassContext) ! {
				_ = pass
			})!
			observed_counter := proof.counter
			proof.window_frames++
			if proof.window_frames == 1 {
				proof.counter++
			}
			if proof.window_frames <= 2 {
				frames <- ContinuousFrameSignal{
					frame:   proof.window_frames
					counter: observed_counter
				}
			}
		}
	)!

	driver := spawn drive_continuous_counter(mut app, frames, driver_result)
	mut run_error := \'\'
	app.run() or { run_error = err.msg() }
	driver.wait()
	driver_error := <-driver_result
	if run_error != \'\' {
		return error(\'continuous counter owner loop: \' + run_error)
	}
	if driver_error != \'\' {
		return error(\'continuous counter driver: \' + driver_error)
	}
	if proof.window_frames < 2 || proof.counter != 1 {
		return error(\'continuous counter did not reach a post-mutation second frame\')
	}
	remaining_windows := app.window_ids()!
	if remaining_windows.len != 0 {
		return error(\'continuous counter cleanup left \' + remaining_windows.len.str() + \' window(s)\')
	}
}

fn drive_continuous_counter(mut app gg.App, frames chan ContinuousFrameSignal, result chan string) {
	mut saw_first := false
	for !saw_first {
		select {
			signal := <-frames {
				if signal.frame != 1 || signal.counter != 0 {
					finish_continuous_counter_driver(mut app, result, \'first frame observed an invalid counter state\')
					return
				}
				saw_first = true
			}
			lifecycle_probe_timeout {
				finish_continuous_counter_driver(mut app, result, \'first frame timeout\')
				return
			}
		}
	}
	for {
		select {
			signal := <-frames {
				if signal.frame >= 2 && signal.counter == 1 {
					finish_continuous_counter_driver(mut app, result, \'\')
					return
				}
			}
			lifecycle_probe_timeout {
				finish_continuous_counter_driver(mut app, result, \'post-mutation frame timeout\')
				return
			}
		}
	}
}

fn finish_continuous_counter_driver(mut app gg.App, result chan string, message string) {
	post_lifecycle_probe_stop(mut app) or {
		prefix := if message == \'\' { \'stop admission\' } else { message + \'; stop admission\' }
		result <- (prefix + \': \' + err.msg())
		return
	}
	result <- message
}

fn run_mixed_event_probe() ! {
	mut proof := &LifecycleProof{}
	mut app := new_lifecycle_probe_app()!
	seed := app.create_window(
		title:  \'mixed-seed\'
		width:  160
		height: 120
	)!
	mut run_error := \'\'
	app.run(
		frame_fn: fn [mut proof] (mut app gg.App) ! {
			proof.global_frames++
			_ = app
		}
		event_fn: fn [seed, mut proof] (event gg.WindowEvent, mut app gg.App) ! {
			if event.kind != .window_created || event.window != seed || proof.dynamic_created {
				return
			}
			proof.dynamic_created = true
			_ = app.create_window(
				title:       \'mixed-dynamic\'
				width:       120
				height:      90
				redraw_mode: .on_demand
				frame_fn:    fn [mut proof] (mut context gg.WindowContext) ! {
					proof.window_frames++
					_ = context
				}
			)!
		}
	) or { run_error = err.msg() }
	if !proof.dynamic_created {
		return error(\'mixed callback probe did not create its per-window frame callback\')
	}
	expected_run_error := \'gg.multiwindow: callback aggregation failed: gg.multiwindow: app frame drawing and per-window frame callbacks cannot be mixed\'
	if run_error != expected_run_error {
		return error(\'mixed callback probe returned unexpected error: \' + run_error)
	}
	if proof.global_frames != 0 || proof.window_frames != 0 {
		return error(\'mixed callback probe executed a frame callback before rejection\')
	}
	remaining_windows := app.window_ids()!
	if remaining_windows.len != 0 {
		return error(\'mixed callback probe cleanup left \' + remaining_windows.len.str() + \' window(s)\')
	}
}
'
}

fn render_runtime_probe_pass_record(path string) string {
	return match os.file_name(path) {
		'multiwindow_dynamic_texture_consumer_probe.v' {
			'{"probe":"dynamic_texture_consumer","status":"PASS","cleanup":"complete"}'
		}
		'multiwindow_retained_ui_consumer_probe.v' {
			'{"probe":"retained_ui_consumer","status":"PASS","cleanup":"complete"}'
		}
		else {
			panic('unregistered runtime consumer probe `${path}`')
		}
	}
}

fn render_runtime_last_nonempty_line(output string) string {
	lines := output.trim_space().split_into_lines()
	if lines.len == 0 {
		return ''
	}
	return lines.last().trim_space()
}

fn render_runtime_cross_targets() []string {
	return ['linux', 'macos', 'windows']
}

fn render_runtime_compile_program(source string, output string, enabled bool) {
	vlib_dir := os.dir(@DIR)
	cmd := '${os.quoted_path(@VEXE)}${render_runtime_child_flags(enabled)} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(output)} ${os.quoted_path(source)}'
	result := render_runtime_execute_child(cmd, enabled)
	assert result.exit_code == 0, 'consumer compile failed for `${os.file_name(source)}`\ncommand: ${cmd}\noutput:\n${result.output}'
}

fn render_runtime_execute_child(command string, enabled bool) os.Result {
	$if linux_wayland_session ? {
		if !enabled {
			return os.execute('env -u DISPLAY -u WAYLAND_DISPLAY -u XDG_SESSION_TYPE ${command}')
		}
	}
	return os.execute(command)
}

fn render_runtime_child_flags(enabled bool) string {
	mut flags := ''
	if enabled {
		flags += ' -d gg_multiwindow'
		$if x_multiwindow_x11 ? {
			flags += ' -d x_multiwindow_x11'
		}
		$if sokol_wayland ? {
			flags += ' -d sokol_wayland'
		}
		$if sokol_metal ? {
			flags += ' -d sokol_metal'
		}
		$if sokol_d3d11 ? {
			flags += ' -d sokol_d3d11'
		}
	}
	$if windows {
		flags += ' -subsystem console'
	}
	$if gcc {
		flags += ' -cc gcc'
	}
	$if msvc {
		flags += ' -cc msvc'
	}
	return flags
}

fn render_runtime_generated_c_without_paths(generated string) string {
	worktree := os.dir(os.dir(@DIR))
	worktree_slashes := worktree.replace('\\', '/')
	mut lines := []string{}
	for line in generated.split_into_lines() {
		if line.starts_with('#line ') {
			continue
		}
		mut clean := line.replace(worktree, '<worktree>')
		if worktree_slashes != worktree {
			clean = clean.replace(worktree_slashes, '<worktree>')
		}
		lines << clean
	}
	return lines.join('\n')
}

fn render_runtime_forbidden_no_flag_markers() []string {
	return [
		'x.multiwindow',
		'vlib/x/multiwindow',
		'multiwindow__App',
		'multiwindow__Backend',
		'multiwindow__RenderFrame',
		'v_multiwindow_',
		'win32_d3d11_backend_helpers.h',
		'x11_backend.c.v',
		'wayland_backend.c.v',
		'appkit_backend.c.v',
	]
}
