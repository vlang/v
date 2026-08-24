module multiwindow

import os

$if darwin {
	import time
}

$if darwin && sokol_metal ? {
	import sokol.gfx
}

$if darwin {
	#flag darwin -framework Cocoa
	#flag darwin -framework Metal
	#flag darwin -framework QuartzCore
	#flag darwin -Wl,-export_dynamic
	#flag darwin -DV_MULTIWINDOW_APPKIT_READBACK_TEST_PROBE=1
	#include "@VMODROOT/vlib/x/multiwindow/testdata/appkit_readback_metal_probe.h"

	fn C.v_multiwindow_appkit_readback_probe_symbol_mask() u32
	fn C.v_multiwindow_appkit_readback_probe_state(window voidptr) voidptr
	fn C.v_multiwindow_appkit_readback_probe_framebuffer_only(window voidptr) int
	fn C.v_multiwindow_appkit_readback_probe_make_pattern_texture(window voidptr) voidptr
	fn C.v_multiwindow_appkit_readback_probe_make_pattern_texture_size(window voidptr, width usize, height usize) voidptr
	fn C.v_multiwindow_appkit_readback_probe_release_texture(texture voidptr)
	fn C.v_multiwindow_appkit_readback_probe_arm_offscreen(state voidptr, texture voidptr, pass_serial u64, producing_frame u64) int
	fn C.v_multiwindow_appkit_readback_probe_make_command_buffer(window voidptr) voidptr
	fn C.v_multiwindow_appkit_readback_probe_commit_command_buffer(command_buffer voidptr, wait int) int
	fn C.v_multiwindow_appkit_readback_probe_wait_command_buffer(command_buffer voidptr) int
	fn C.v_multiwindow_appkit_readback_probe_release_object(object voidptr)
	fn C.v_multiwindow_appkit_readback_probe_stage_window(state voidptr, request u64, x int, y int, width int, height int, producing_frame u64) int
	fn C.v_multiwindow_appkit_readback_probe_stage_image(state voidptr, texture voidptr, request u64, x int, y int, width int, height int, producing_frame u64) int
	fn C.v_multiwindow_appkit_readback_probe_resolve(state voidptr, submitted_frame u64, submission_succeeded int) int
	fn C.v_multiwindow_appkit_readback_probe_take(state voidptr, out_request &u64, out_status &int, out_width &int, out_height &int, out_stride &int, out_submitted_frame &u64, out_byte_length &usize) int
	fn C.v_multiwindow_appkit_readback_probe_copy(state voidptr, request u64, out_pixels &u8, capacity usize) int
	fn C.v_multiwindow_appkit_readback_probe_release(state voidptr, request u64) int
	fn C.v_multiwindow_appkit_readback_probe_cancel(state voidptr, request u64) int
	fn C.v_multiwindow_appkit_readback_probe_cancel_all(state voidptr) int
	fn C.v_multiwindow_appkit_readback_test_pause_completion() int
	fn C.v_multiwindow_appkit_readback_test_wait_completion_paused() int
	fn C.v_multiwindow_appkit_readback_test_release_completion()
	fn C.v_multiwindow_appkit_readback_test_invoke_end_pass(command_buffer voidptr, drawable voidptr) int
	fn C.v_multiwindow_appkit_readback_test_record_count(state voidptr) int
	fn C.v_multiwindow_appkit_readback_test_ready_count(state voidptr) int
	fn C.v_multiwindow_appkit_readback_test_gpu_completed(state voidptr, request u64) int
	fn C.v_multiwindow_appkit_readback_test_mark_encoding_failed(state voidptr, request u64) int
	fn C.v_multiwindow_appkit_readback_test_make_offscreen_slot_stale() int
	fn C.v_multiwindow_appkit_readback_test_offscreen_slot_present() int
	fn C.v_multiwindow_appkit_service_release_window_services(state voidptr) int
}

struct AppKitReadbackContractNeed {
	label  string
	source string
	needle string
}

struct AppKitReadbackProbeResult {
	request         u64
	status          int
	width           int
	height          int
	stride          int
	submitted_frame u64
	byte_length     usize
mut:
	take_status int
}

fn appkit_readback_module_source(name string) string {
	return os.read_file(os.join_path(@DIR, name)) or { panic(err) }
}

fn appkit_readback_sokol_source() string {
	return os.read_file(os.join_path(@DIR, '..', '..', '..', 'thirdparty', 'sokol', 'sokol_gfx.h')) or {
		panic(err)
	}
}

fn appkit_readback_body(source string, signature string) string {
	start := source.index(signature) or { return '' }
	relative_open := source[start..].index('{') or { return '' }
	open := start + relative_open
	mut depth := 0
	for index := open; index < source.len; index++ {
		if source[index] == `{` {
			depth++
		} else if source[index] == `}` {
			depth--
			if depth == 0 {
				return source[open + 1..index]
			}
		}
	}
	return ''
}

fn appkit_readback_last_body(source string, signature string) string {
	mut start := -1
	mut offset := 0
	for offset < source.len {
		relative := source[offset..].index(signature) or { break }
		start = offset + relative
		offset = start + signature.len
	}
	if start < 0 {
		return ''
	}
	relative_open := source[start..].index('{') or { return '' }
	open := start + relative_open
	mut depth := 0
	for index := open; index < source.len; index++ {
		if source[index] == `{` {
			depth++
		} else if source[index] == `}` {
			depth--
			if depth == 0 {
				return source[open + 1..index]
			}
		}
	}
	return ''
}

fn appkit_readback_missing(needs []AppKitReadbackContractNeed) []string {
	mut missing := []string{}
	for need in needs {
		if !need.source.contains(need.needle) {
			missing << '${need.label}: `${need.needle}`'
		}
	}
	return missing
}

fn appkit_readback_occurrences_guarded(source string, needle string) bool {
	mut depth := 0
	mut metal_guard_depth := 0
	mut found := false
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('#if') {
			depth++
			if trimmed == '#if defined(SOKOL_METAL) && defined(V_SOKOL_MTL_END_PASS_HOOK)' {
				metal_guard_depth = depth
			}
		}
		if line.contains(needle) {
			found = true
			if metal_guard_depth == 0 {
				return false
			}
		}
		if trimmed.starts_with('#endif') {
			if metal_guard_depth == depth {
				metal_guard_depth = 0
			}
			depth--
		}
	}
	return found
}

fn appkit_readback_runtime_requested() bool {
	return os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') == '1'
		&& os.getenv('VGG_MULTIWINDOW_RUNTIME_BACKEND') == 'appkit'
}

fn test_appkit_readback_links_rendererless_and_glcore33() {
	$if darwin {
		tag := '${os.getpid()}_${u64(time.now().unix_nano())}'
		source_path := os.join_path(os.temp_dir(), 'appkit_readback_link_${tag}.v')
		os.write_file(source_path, 'module main

import x.multiwindow

fn main() {
	mut app := multiwindow.new_app(backend: .appkit, require_renderer: false) or {
		panic(err)
	}
	assert !app.capabilities().readback
	app.stop() or { panic(err) }
}') or {
			panic(err)
		}
		defer {
			os.rm(source_path) or {}
		}
		for variant, defines in {
			'rendererless': '-d gg_multiwindow'
			'glcore33':     '-d gg_multiwindow -d darwin_sokol_glcore33'
		} {
			output_path := os.join_path(os.temp_dir(), 'appkit_readback_link_${variant}_${tag}')
			defer {
				os.rm(output_path) or {}
			}
			command := '${os.quoted_path(@VEXE)} -gc none ${defines} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
			result := os.execute(command)
			assert result.exit_code == 0, '${variant} AppKit readback link failed:\n${result.output}'
		}
	}
}

fn test_appkit_metal_readback_native_contract_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	backend := appkit_readback_module_source('appkit_backend.c.v')
	mut missing := appkit_readback_missing([
		AppKitReadbackContractNeed{'arm offscreen pass definition', native, 'int v_multiwindow_appkit_service_arm_offscreen_readback_pass('},
		AppKitReadbackContractNeed{'stage window definition', native, 'int v_multiwindow_appkit_service_stage_window_readback('},
		AppKitReadbackContractNeed{'stage image definition', native, 'int v_multiwindow_appkit_service_stage_image_readback('},
		AppKitReadbackContractNeed{'resolve after submit definition', native, 'int v_multiwindow_appkit_service_resolve_readbacks_after_submit('},
		AppKitReadbackContractNeed{'take result definition', native, 'int v_multiwindow_appkit_service_take_readback_result('},
		AppKitReadbackContractNeed{'copy pixels definition', native, 'int v_multiwindow_appkit_service_copy_readback_pixels('},
		AppKitReadbackContractNeed{'release result definition', native, 'int v_multiwindow_appkit_service_release_readback_result('},
		AppKitReadbackContractNeed{'cancel request definition', native, 'int v_multiwindow_appkit_service_cancel_readback('},
		AppKitReadbackContractNeed{'cancel all definition', native, 'int v_multiwindow_appkit_service_cancel_all_readbacks('},
		AppKitReadbackContractNeed{'capture-capable drawable', native, 'framebufferOnly = NO'},
		AppKitReadbackContractNeed{'Metal staging buffer', native, 'newBufferWithLength:'},
		AppKitReadbackContractNeed{'overflow bound', native, 'SIZE_MAX'},
		AppKitReadbackContractNeed{'blit encoder', native, 'blitCommandEncoder'},
		AppKitReadbackContractNeed{'texture to buffer copy', native, 'copyFromTexture:'},
		AppKitReadbackContractNeed{'compact destination row pitch', native, 'destinationBytesPerRow:public_bytes_per_row'},
		AppKitReadbackContractNeed{'2D destination image pitch', native, 'destinationBytesPerImage:0'},
		AppKitReadbackContractNeed{'GPU completion handler', native, 'addCompletedHandler:'},
		AppKitReadbackContractNeed{'BGRA to compact RGBA normalization', native, 'v_multiwindow_appkit_bgra_to_rgba'},
		AppKitReadbackContractNeed{'backend readback staging', backend, 'v_multiwindow_appkit_service_stage_window_readback'},
		AppKitReadbackContractNeed{'backend readback collection', backend, 'v_multiwindow_appkit_service_take_readback_result'},
	])

	capability := appkit_readback_body(native, 'int v_multiwindow_appkit_service_capability(')
	if capability.contains('(void)renderer_ready;')
		|| !capability.contains('V_MULTIWINDOW_APPKIT_SERVICE_WINDOW_CAPTURE')
		|| !capability.contains('state.layer') || !capability.contains('framebufferOnly') {
		missing << 'rendererless capability honesty: capture support must require renderer_ready and a readable live Metal layer'
	}
	configure := appkit_readback_body(native,
		'static CAMetalLayer *v_multiwindow_appkit_configure_layer(')
	if !configure.contains('framebufferOnly = NO') {
		missing << 'window-only readable drawable: rendered window layer remains framebufferOnly'
	}
	anchor := appkit_readback_body(native,
		'static int v_multiwindow_appkit_create_renderer_anchor_raw(')
	if !anchor.contains('framebufferOnly = YES') {
		missing << 'renderer anchor isolation: non-capture anchor must remain framebufferOnly'
	}
	if native.contains('waitUntilCompleted') {
		missing << 'real-time synchronization: waitUntilCompleted is forbidden'
	}
	if native.contains('newCommandQueue') {
		missing << 'command-buffer authority: readback must not create a separate Metal queue'
	}

	completion := appkit_readback_body(native, 'addCompletedHandler:')
	if completion != '' {
		for forbidden in ['queueEvent', 'queueInputEvent',
			'v_multiwindow_appkit_service_take_readback_result',
			'v_multiwindow_appkit_service_copy_readback_pixels'] {
			if completion.contains(forbidden) {
				missing << 'native-only completion: completion handler contains `${forbidden}`'
			}
		}
	}
	assert missing.len == 0, 'missing AppKit Metal readback contracts:\n${missing.join('\n')}'
}

fn test_appkit_metal_readback_public_bridge_red() {
	backend := appkit_readback_module_source('appkit_backend.c.v')
	dispatch := appkit_readback_module_source('service_backend.v')
	api := appkit_readback_module_source('service_api.v')
	delivery := appkit_readback_module_source('event_delivery.v')
	scheduler := appkit_readback_module_source('render_scheduler.v')
	gg_render := os.read_file(os.join_path(@DIR, '..', '..', 'gg',
		'multiwindow_render_impl_d_gg_multiwindow.v')) or { panic(err) }
	missing := appkit_readback_missing([
		AppKitReadbackContractNeed{'arm ABI declaration', backend, 'fn C.v_multiwindow_appkit_service_arm_offscreen_readback_pass('},
		AppKitReadbackContractNeed{'backend stage window bridge', backend, 'fn (mut backend AppKitBackend) service_stage_window_readback('},
		AppKitReadbackContractNeed{'backend stage image bridge', backend, 'fn (mut backend AppKitBackend) service_stage_image_readback('},
		AppKitReadbackContractNeed{'backend arm image bridge', backend, 'fn (mut backend AppKitBackend) service_arm_image_readback_pass('},
		AppKitReadbackContractNeed{'backend resolve bridge', backend, 'fn (mut backend AppKitBackend) service_resolve_readbacks_after_submit('},
		AppKitReadbackContractNeed{'backend owner-thread collector', backend, 'fn (mut backend AppKitBackend) service_take_readback_results('},
		AppKitReadbackContractNeed{'dispatch stage window', dispatch, 'backend.appkit.service_stage_window_readback('},
		AppKitReadbackContractNeed{'dispatch stage image', dispatch, 'backend.appkit.service_stage_image_readback('},
		AppKitReadbackContractNeed{'core stage window', api, 'service_stage_window_readback_for_gg('},
		AppKitReadbackContractNeed{'core stage image', api, 'service_stage_image_readback_for_gg('},
		AppKitReadbackContractNeed{'bounded canonical terminal admission', delivery, 'accept_backend_readback_event_locked(event.readback)'},
		AppKitReadbackContractNeed{'per-window submission resolution', scheduler, 'service_resolve_readbacks_after_submit'},
		AppKitReadbackContractNeed{'gg AppKit capture path', gg_render, 'stage_appkit_window_capture('},
		AppKitReadbackContractNeed{'gg AppKit offscreen arm', gg_render, 'arm_appkit_image_readbacks_for_pass('},
		AppKitReadbackContractNeed{'post-pass image preservation', gg_render, 'submit_appkit_image_readback_pass('},
	])
	assert missing.len == 0, 'missing public AppKit Metal readback bridge:\n${missing.join('\n')}'
	capability := appkit_readback_body(backend, 'fn appkit_service_capability_from_native(')
	assert !capability.contains('operation in [.portal_parent, .image_readback, .window_capture]'), 'V bridge still masks native AppKit Metal readback capabilities'
}

fn test_appkit_metal_readback_common_result_validation() {
	window := WindowId{
		app_instance: 41
		slot:         2
		generation:   3
	}
	id := ServiceReadbackId{
		app_instance: 41
		serial:       7
		window:       window
	}
	valid := appkit_backend_readback_result(id, service_appkit_readback_ready, 9, 2, 1, 8, [
		u8(1),
		2,
		3,
		4,
		5,
		6,
		7,
		8,
	], true)
	assert valid.status == .ready
	assert valid.submitted_frame == 9
	assert valid.pixels_rgba8.len == 8
	malformed := appkit_backend_readback_result(id, service_appkit_readback_ready, 9, 2, 1, 7, [
		u8(1),
		2,
		3,
		4,
		5,
		6,
		7,
	], true)
	assert malformed.status == .failed
	assert malformed.pixels_rgba8.len == 0
	not_copied := appkit_backend_readback_result(id, service_appkit_readback_ready, 9, 2, 1, 8,
		[]u8{len: 8}, false)
	assert not_copied.status == .failed
	native_cancelled := appkit_backend_readback_result(id, 2, 0, 0, 0, 0, []u8{}, false)
	assert native_cancelled.status == .failed
	assert native_cancelled.status != .cancelled
	native_failed_region := appkit_backend_readback_result(id, service_appkit_readback_failed, 602,
		1, 1, 0, []u8{}, false)
	assert native_failed_region.status == .failed
	assert native_failed_region.submitted_frame == 602
	assert native_failed_region.width == 0
	assert native_failed_region.height == 0
	assert native_failed_region.stride == 0
	assert native_failed_region.pixels_rgba8.len == 0
	release_failed := appkit_backend_finalize_readback_release(valid, false)
	assert release_failed.status == .failed
	assert release_failed.pixels_rgba8.len == 0
	assert release_failed.error == err_readback_invalid
}

fn test_appkit_metal_readback_invalid_native_terminals_are_exactly_once() {
	mut app := new_app()!
	window := app.create_window()!
	_ = app.drain_events()!
	malformed_id := app.service_begin_window_readback(window)!
	cancelled_id := app.service_begin_window_readback(window)!
	malformed := appkit_backend_readback_result(malformed_id, service_appkit_readback_ready, 11, 2,
		1, 7, []u8{len: 7}, true)
	cancelled := appkit_backend_readback_result(cancelled_id, 2, 0, 0, 0, 0, []u8{}, false)
	acceptance := app.accept_backend_event_batch([
		queued_readback_event(ServiceReadbackResult{
			id:              malformed.id
			window:          malformed.id.window
			status:          malformed.status
			submitted_frame: malformed.submitted_frame
			error:           malformed.error
		}),
		queued_readback_event(ServiceReadbackResult{
			id:              cancelled.id
			window:          cancelled.id.window
			status:          cancelled.status
			submitted_frame: cancelled.submitted_frame
			error:           cancelled.error
		}),
	], 1)!
	assert acceptance.accepted == 2
	ticket := app.prepare_window_destroy(window)!
	app.seal_window_destroy(ticket)!
	app.finish_window_destroy(ticket, []string{})!
	events := app.drain_queued_events()!
	readbacks := events.filter(it.kind == .readback)
	assert readbacks.len == 2
	assert readbacks.all(it.readback.status == .failed)
	assert app.services.readbacks.len == 0
	app.stop()!
}

fn test_appkit_metal_readback_bridge_terminal_policy() {
	backend := appkit_readback_module_source('appkit_backend.c.v')
	resolve := appkit_readback_body(backend,
		'fn (mut backend AppKitBackend) service_resolve_readbacks_after_submit(')
	assert resolve.contains('status < service_appkit_result_unavailable'), 'resolve status 0 must be an idempotent no-op'
	collector := appkit_readback_body(backend,
		'fn (mut backend AppKitBackend) service_take_readback_results(')
	assert collector.contains('appkit_backend_readback_result(')
	assert collector.contains('appkit_backend_finalize_readback_release(')
	assert collector.contains('v_multiwindow_appkit_service_cancel_readback(record.state, request)')
	assert collector.contains('backend.poll_error = merge_backend_errors(')
	poll := appkit_readback_body(backend, 'fn (mut backend AppKitBackend) poll_queued_events(')
	assert poll.contains('queued_readback_events()'), 'readback completion bypasses the canonical backend event batch'
	release := appkit_readback_body(backend,
		'fn (mut backend AppKitBackend) release_window_services(')
	assert release.contains('v_multiwindow_appkit_service_cancel_all_readbacks(record.state) < 0')
	api := appkit_readback_module_source('service_api.v')
	assert !api.contains('service_collect_backend_readbacks_for_gg'), 'direct gg collection bypasses canonical event admission'
}

fn test_appkit_metal_readback_resolution_is_per_window_red() {
	scheduler := appkit_readback_module_source('render_scheduler.v')
	complete := appkit_readback_body(scheduler, 'fn (mut app App) complete_render_submission(')
	assert complete.contains('service_resolve_readbacks_after_submit'), 'AppKit readbacks are not resolved at the per-window submission authority'

	assert complete.contains('submitted_frame'), 'resolution does not use the authoritative window frame'
	assert complete.contains('finalized'), 'resolution does not distinguish mixed submit success/failure'
	gg_lifecycle := os.read_file(os.join_path(@DIR, '..', '..', 'gg',
		'multiwindow_render_lifecycle_d_gg_multiwindow.v')) or { panic(err) }
	assert !gg_lifecycle.contains('outcome.committed')
		|| !gg_lifecycle.contains('service_resolve_readbacks_after_submit'), 'global RenderBatchOutcome must not resolve per-window AppKit readbacks'
}

fn test_appkit_metal_readback_preserves_post_pass_request_contract_red() {
	gg_test := os.read_file(os.join_path(@DIR, '..', '..', 'gg',
		'multiwindow_wayland_services_d_gg_multiwindow_test.v')) or { panic(err) }
	pass_index := gg_test.index('context.with_offscreen_sgl(') or { -1 }
	request_index := gg_test.index('context.request_image_readback(') or { -1 }
	assert pass_index >= 0 && request_index > pass_index, 'public managed-image contract no longer proves a request made after its drawing pass'

	gg_render := os.read_file(os.join_path(@DIR, '..', '..', 'gg',
		'multiwindow_render_impl_d_gg_multiwindow.v')) or { panic(err) }
	assert gg_render.contains('submit_appkit_image_readback_pass('), 'AppKit has no bounded same-submit pass for a readback requested after drawing'
}

fn test_appkit_metal_readback_uses_render_command_buffer_before_present_red() {
	sokol := appkit_readback_sokol_source()
	end_pass := appkit_readback_body(sokol, '_SOKOL_PRIVATE void _sg_mtl_end_pass(void)')
	encoder_end := end_pass.index('[_sg.mtl.cmd_encoder endEncoding]') or { -1 }
	present := end_pass.index('[_sg.mtl.cmd_buffer presentDrawable:_sg.mtl.cur_drawable]') or { -1 }
	assert encoder_end >= 0
	assert present > encoder_end
	between := end_pass[encoder_end..present]
	assert between.contains('_v_sokol_metal_invoke_pre_present'), 'no AppKit readback hook exists between Metal render-encoder end and presentDrawable'
	assert between.contains('_sg.mtl.cmd_buffer'), 'pre-present readback hook does not receive the rendering command buffer'
	assert between.contains('_sg.mtl.cur_drawable'), 'pre-present readback hook does not receive the current drawable'
}

fn test_appkit_metal_readback_hook_is_link_guarded_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	sokol := appkit_readback_sokol_source()
	for needle in [
		'typedef void (*v_sokol_mtl_end_pass_hook_t)(',
		'SOKOL_GFX_API_DECL bool v_sokol_mtl_set_end_pass_hook(',
	] {
		assert appkit_readback_occurrences_guarded(sokol, needle), 'canonical Sokol Metal hook declaration `${needle}` is missing or escapes its link guard'
	}
	for needle in [
		'typedef void (*VMultiwindowSokolMetalEndPassHook)',
		'extern bool v_sokol_mtl_set_end_pass_hook',
	] {
		assert !native.contains(needle), 'AppKit must consume the canonical Sokol Metal hook declaration instead of redeclaring `${needle}`'
	}
	for needle in [
		'v_sokol_mtl_set_end_pass_hook(v_multiwindow_appkit_readback_end_pass_hook',
		'v_sokol_mtl_set_end_pass_hook(NULL',
		'@implementation VMultiwindowAppKitReadbackRecord',
		'- (BOOL)registerReadbackHook',
		'- (void)unregisterReadbackHook',
		'newBufferWithLength:',
		'copyFromTexture:',
		'addCompletedHandler:',
		'framebufferOnly = NO',
	] {
		assert appkit_readback_occurrences_guarded(native, needle), 'AppKit Metal hook reference `${needle}` escapes the SOKOL_METAL/V_SOKOL guard'
	}
}

fn test_appkit_metal_readback_native_cancel_is_non_terminal_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	cancel := appkit_readback_last_body(native, '- (BOOL)cancel')
	assert cancel != ''
	assert !cancel.contains('V_MULTIWINDOW_APPKIT_READBACK_CANCELLED'), 'native cancel must not publish a terminal result'
	assert !native.contains('V_MULTIWINDOW_APPKIT_READBACK_CANCELLED'), 'native readback must not define or expose a cancelled terminal'
	service_cancel := appkit_readback_body(native,
		'int v_multiwindow_appkit_service_cancel_readback(')
	assert service_cancel.contains('removeObjectIdenticalTo:record'), 'native cancel must reap the cancelled record instead of exposing it to take()'
}

fn test_appkit_metal_readback_cancel_preserves_matching_sibling_slot_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	service_cancel := appkit_readback_body(native,
		'int v_multiwindow_appkit_service_cancel_readback(')
	assert service_cancel.contains('v_multiwindow_appkit_offscreen_slot_has_active_record'), 'cancelling one staged image must preserve the slot while a matching sibling remains'
	helper := appkit_readback_body(native,
		'static BOOL v_multiwindow_appkit_offscreen_slot_has_active_record(')
	assert helper.contains('record.sourceTexture == slot.expectedTexture')
	assert helper.contains('record.producingFrame == slot.producingFrame')
	assert helper.contains('!record.released')
	assert helper.contains('!record.cancelRequested')
	assert helper.contains('!record.encoded')
}

fn test_appkit_metal_readback_quiesce_reaps_ready_results_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	quiesce := appkit_readback_last_body(native, '- (void)quiesceReadbacks')
	assert quiesce != ''
	assert quiesce.contains('removeAllObjects'), 'quiesce must make ready-but-undelivered results non-observable'
}

fn test_appkit_metal_readback_hook_reclaims_stale_local_installation_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	register := appkit_readback_last_body(native, '- (BOOL)registerReadbackHook')
	assert register != ''
	assert register.count('v_sokol_mtl_set_end_pass_hook') >= 2, 'renderer restart must verify/reclaim the native hook before reinstalling it'
}

fn test_appkit_metal_readback_failed_submit_has_no_frame_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	resolve := appkit_readback_last_body(native, '- (void)resolveSubmissionFrame:')
	assert resolve != ''
	assert resolve.contains('self.submittedFrame = succeeded ? submittedFrame : 0;'), 'failed submission must expose submitted_frame=0'
}

fn test_appkit_metal_readback_completion_publishes_pixels_atomically_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	completion := appkit_readback_last_body(native, '- (void)completeCommandBuffer:')
	assert completion != ''
	pixels := completion.index('self.publicPixels = pixels;') or { -1 }
	completed := completion.index('self.gpuCompleted = YES;') or { -1 }
	assert pixels >= 0
	assert completed > pixels, 'gpuCompleted became observable before pixel normalization was ready'
}

fn test_appkit_metal_readback_encoding_failure_waits_for_resolve_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	assert native.contains('@property(assign) BOOL encodingFailed;'), 'record needs a non-terminal encoding failure state'
	encode := appkit_readback_last_body(native, '- (void)encodeReadbacksWithCommandBuffer:')
	assert encode != ''
	assert !encode.contains('record.status = V_MULTIWINDOW_APPKIT_READBACK_FAILED;'), 'encoding failure must not publish a terminal before resolve'
}

fn test_appkit_metal_readback_offscreen_hook_is_state_scoped_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	hook := appkit_readback_last_body(native,
		'static void v_multiwindow_appkit_readback_end_pass_hook(')
	assert hook != ''
	assert !hook.contains('command_buffer == NULL || drawable == NULL'), 'a drawable-less offscreen pass must not be rejected globally'
	assert hook.contains('if (drawable == NULL)'), 'drawable-less passes need an explicit slot-consumption path'
	assert native.contains('v_multiwindow_appkit_offscreen_readback_slot'), 'image-only offscreen requests need one explicitly scoped pending state'
	assert hook.contains('pending_offscreen'), 'the hook must route a drawable-less pass only to its staged state'
	nil_path_end := hook.index('id<CAMetalDrawable> native_drawable') or { hook.len }
	assert !hook[..nil_path_end].contains('v_multiwindow_appkit_readback_states.allObjects'), 'drawable-less handling must not scan all window states'
	for field in ['stateGeneration', 'passSerial', 'producingFrame', 'expectedTexture'] {
		assert native.contains(field), 'offscreen slot is missing `${field}` authority'
	}
}

fn test_appkit_metal_readback_2d_copy_uses_compact_checked_rows_red() {
	native := appkit_readback_module_source('appkit_backend.m')
	encode := appkit_readback_last_body(native, '- (void)encodeReadbacksWithCommandBuffer:')
	assert encode != ''
	assert !encode.contains('minimumTextureBufferAlignmentForPixelFormat'), 'texture-buffer alignment is not a texture-to-buffer blit row-pitch requirement'
	assert encode.contains('destinationBytesPerImage:0'), 'Metal requires destinationBytesPerImage=0 for a 2D texture copy'
	assert encode.contains('newBufferWithLength:allocation_byte_length'), 'allocation size must be checked independently from the public row pitch'
	probe := os.read_file(@FILE) or { panic(err) }
	assert probe.contains('for width in [1, 3, 257]'), 'Metal validation widths 1, 3 and 257 are not covered'
}

fn test_appkit_nonmetal_readback_abi_stubs_are_available() {
	native := appkit_readback_module_source('appkit_backend.m')
	start := native.index('#else\nint v_multiwindow_appkit_service_arm_offscreen_readback_pass(') or {
		assert false, 'missing non-Metal AppKit readback ABI branch'
		return
	}
	relative_end := native[start..].index('#endif') or {
		assert false, 'unterminated non-Metal AppKit readback ABI branch'
		return
	}
	stubs := native[start..start + relative_end]
	for signature in [
		'v_multiwindow_appkit_service_arm_offscreen_readback_pass(',
		'v_multiwindow_appkit_service_stage_window_readback(',
		'v_multiwindow_appkit_service_stage_image_readback(',
		'v_multiwindow_appkit_service_resolve_readbacks_after_submit(',
		'v_multiwindow_appkit_service_take_readback_result(',
		'v_multiwindow_appkit_service_copy_readback_pixels(',
		'v_multiwindow_appkit_service_release_readback_result(',
		'v_multiwindow_appkit_service_cancel_readback(',
		'v_multiwindow_appkit_service_cancel_all_readbacks(',
	] {
		assert stubs.contains(signature), 'missing non-Metal ABI stub `${signature}`'
	}
	assert stubs.count('return V_MULTIWINDOW_APPKIT_SERVICE_RESULT_UNAVAILABLE;') >= 9
}

$if darwin && sokol_metal ? && gg_multiwindow ? {
	@[markused]
	fn appkit_readback_wait_until_eligible(mut app App, window WindowId) ! {
		deadline := time.now().add(5 * time.second)
		for {
			app.poll_events()!
			if app.render_window_eligible(window)! {
				return
			}
			if time.now() >= deadline {
				return error('AppKit readback window did not become render eligible')
			}
			time.sleep(5 * time.millisecond)
		}
	}

	@[markused]
	fn appkit_readback_window_native(mut app App, window WindowId) !(voidptr, voidptr, int) {
		mut pointers := []voidptr{len: 2, init: unsafe { nil }}
		mut framebuffer_only := []int{len: 1, init: -2}
		inspect := fn [mut pointers, mut framebuffer_only] (borrow NativeWindowBorrow) ! {
			pointers[1] = borrow.primary_for_gg()
			pointers[0] = C.v_multiwindow_appkit_readback_probe_state(pointers[1])
			framebuffer_only[0] =
				C.v_multiwindow_appkit_readback_probe_framebuffer_only(pointers[1])
		}
		app.with_native_window_for_gg(window, inspect)!
		if pointers[0] == unsafe { nil } || pointers[1] == unsafe { nil } {
			return error('AppKit readback probe did not receive the live NSWindow/state')
		}
		return pointers[0], pointers[1], framebuffer_only[0]
	}

	@[markused]
	fn appkit_readback_take(state voidptr) AppKitReadbackProbeResult {
		mut result := AppKitReadbackProbeResult{}
		result.take_status = C.v_multiwindow_appkit_readback_probe_take(state, &result.request,
			&result.status, &result.width, &result.height, &result.stride, &result.submitted_frame,
			&result.byte_length)
		return result
	}

	@[markused]
	fn appkit_readback_wait_result(state voidptr) !AppKitReadbackProbeResult {
		deadline := time.now().add(5 * time.second)
		for {
			result := appkit_readback_take(state)
			if result.take_status == 1 {
				return result
			}
			if result.take_status != 0 {
				return error('AppKit native take returned ${result.take_status}')
			}
			if time.now() >= deadline {
				return error('AppKit Metal completion did not become owner-thread observable')
			}
			time.sleep(5 * time.millisecond)
		}
		return error('AppKit Metal completion wait ended unexpectedly')
	}

	@[markused]
	fn appkit_readback_wait_ready_count(mut app App, state voidptr, expected int) ! {
		deadline := time.now().add(5 * time.second)
		for {
			if C.v_multiwindow_appkit_readback_test_ready_count(state) == expected {
				return
			}
			app.poll_events()!
			if time.now() >= deadline {
				return error('AppKit ready readback count did not reach ${expected}')
			}
			time.sleep(5 * time.millisecond)
		}
	}

	@[markused]
	fn appkit_readback_copy_once(state voidptr, result AppKitReadbackProbeResult) ![]u8 {
		assert result.width > 0
		assert result.height > 0
		assert result.stride == result.width * 4
		assert result.byte_length == usize(result.stride * result.height)
		mut pixels := []u8{len: int(result.byte_length)}
		if pixels.len > 1 {
			assert C.v_multiwindow_appkit_readback_probe_copy(state, result.request, pixels.data,
				usize(pixels.len - 1)) <= 0
		}
		assert C.v_multiwindow_appkit_readback_probe_copy(state, result.request, pixels.data,
			usize(pixels.len)) == 1
		assert C.v_multiwindow_appkit_readback_probe_copy(state, result.request, pixels.data,
			usize(pixels.len)) == 0
		assert C.v_multiwindow_appkit_readback_probe_release(state, result.request) == 1
		assert C.v_multiwindow_appkit_readback_probe_release(state, result.request) == 0
		return pixels
	}

	@[markused]
	fn appkit_readback_submit_window(mut app App, window WindowId, state voidptr, texture voidptr, window_request u64, image_request u64, frame u64) ![]int {
		mut capture := []int{len: 3}
		outcome := app.with_scheduled_render_batch(fn [mut app, window, state, texture, window_request, image_request, frame, mut capture] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			assert candidates.any(it.window == window)
			acquisition := app.acquire_render_target(batch, window)!
			assert acquisition.status == .ready
			app.with_render_target_pass(acquisition.lease, gfx.PassAction{}, fn [state, texture, window_request, image_request, frame, mut capture] () ! {
				if image_request == 0 {
					capture[0] = C.v_multiwindow_appkit_readback_probe_stage_window(state,
						window_request, 0, 0, 1, 1, frame)
					return
				}
				capture[0] = C.v_multiwindow_appkit_readback_probe_stage_window(state,
					window_request, 0, 0, 2, 2, frame)
				capture[1] = C.v_multiwindow_appkit_readback_probe_stage_image(state, texture,
					image_request, 1, 0, 2, 2, frame)
				capture[2] = appkit_readback_take(state).take_status
			})!
		})!
		assert capture[0] == 1
		if image_request != 0 {
			assert capture[1] == 1
			assert capture[2] == 0
		}
		assert outcome.committed
		if image_request != 0 {
			assert outcome.finalized_submissions == 1
		}
		assert C.v_multiwindow_appkit_readback_probe_resolve(state, frame, 1) == 1
		return capture
	}

	@[markused]
	fn appkit_readback_wait_native_result(state voidptr) !AppKitReadbackProbeResult {
		deadline := time.now().add(5 * time.second)
		for {
			result := appkit_readback_take(state)
			if result.take_status == 1 {
				return result
			}
			if result.take_status != 0 {
				return error('AppKit native take returned ${result.take_status}')
			}
			if time.now() >= deadline {
				return error('AppKit mixed-submit readback did not complete')
			}
			time.sleep(5 * time.millisecond)
		}
		return error('AppKit mixed-submit wait ended unexpectedly')
	}

	@[markused]
	fn appkit_readback_mixed_callback_batch(mut app App, success_window WindowId, failed_window WindowId, success_state voidptr, failed_state voidptr, success_request u64, failed_request u64, clear_red f32, clear_green f32, clear_blue f32, clear_alpha f32, callback_message string) !(u64, u64) {
		mut capture := []u64{len: 2}
		action := gfx.create_clear_pass_action(clear_red, clear_green, clear_blue, clear_alpha)
		outcome := app.with_scheduled_render_batch(fn [mut app, success_window, failed_window, success_state, failed_state, success_request, failed_request, action, callback_message, mut capture] (batch RenderBatchLease, candidates []RenderWindowSnapshot) ! {
			mut success_candidate := RenderWindowSnapshot{}
			mut failed_candidate := RenderWindowSnapshot{}
			for candidate in candidates {
				if candidate.window == success_window {
					success_candidate = candidate
				} else if candidate.window == failed_window {
					failed_candidate = candidate
				}
			}
			assert success_candidate.window == success_window
			assert failed_candidate.window == failed_window
			capture[0] = success_candidate.submitted_frame + 1
			capture[1] = failed_candidate.submitted_frame + 1
			success_target := app.acquire_render_target(batch, success_window)!
			failed_target := app.acquire_render_target(batch, failed_window)!
			assert success_target.status == .ready
			assert failed_target.status == .ready
			assert C.v_multiwindow_appkit_readback_probe_stage_window(success_state,
				success_request, 0, 0, 1, 1, capture[0]) == 1
			assert C.v_multiwindow_appkit_readback_probe_stage_window(failed_state, failed_request,
				0, 0, 1, 1, capture[1]) == 1
			app.with_render_target_pass(success_target.lease, action, fn () ! {})!
			return error(callback_message)
		})!
		assert outcome.committed
		assert outcome.finalized_submissions == 1
		assert outcome.error.contains(callback_message)
		return capture[0], capture[1]
	}

	@[markused]
	fn appkit_readback_assert_mixed_terminals(success_state voidptr, failed_state voidptr, success_request u64, failed_request u64, success_frame u64, expected_pixel []u8) ! {
		success := appkit_readback_wait_native_result(success_state)!
		assert success.request == success_request
		assert success.status == 1
		assert success.submitted_frame == success_frame
		assert success.width == 1
		assert success.height == 1
		assert success.stride == 4
		pixels := appkit_readback_copy_once(success_state, success)!
		assert pixels == expected_pixel

		failed := appkit_readback_wait_native_result(failed_state)!
		assert failed.request == failed_request
		assert failed.status == 3
		assert failed.submitted_frame == 0
		assert failed.width == 1
		assert failed.height == 1
		assert failed.stride == 0
		assert failed.byte_length == 0
		assert C.v_multiwindow_appkit_readback_probe_release(failed_state, failed_request) == 1
	}
}

fn test_appkit_metal_readback_rendererless_honesty_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		// Root Result payload types used only from markused runtime helpers for CGen.
		_ = RenderBatchOutcome{}
		_ = RenderTargetAcquisition{}
		assert C.v_multiwindow_appkit_readback_probe_symbol_mask() == u32(0xff)
		mut app := new_app(backend: .appkit, require_renderer: false)!
		defer {
			app.stop() or {}
		}
		window := app.create_window(title: 'rendererless readback honesty', visible: false)!
		state, _, framebuffer_only := appkit_readback_window_native(mut app, window)!
		assert framebuffer_only == -1
		assert C.v_multiwindow_appkit_service_capability(state,
			int(ServiceOperation.window_capture), 0) == 0
		assert C.v_multiwindow_appkit_readback_probe_stage_window(state, 1, 0, 0, 1, 1, 1) <= 0
	}
}

fn test_appkit_metal_readback_frame_region_and_exactly_once_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		assert C.v_multiwindow_appkit_readback_probe_symbol_mask() == u32(0xff)
		mut app := new_app(backend: .appkit, queue_size: 32, require_renderer: true)!
		defer {
			app.stop() or {}
		}
		app.start_renderer(RendererConfig{})!
		window := app.create_window(
			title:           'AppKit Metal readback RED'
			width:           96
			height:          64
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		appkit_readback_wait_until_eligible(mut app, window)!
		state, native_window, framebuffer_only := appkit_readback_window_native(mut app, window)!
		assert framebuffer_only == 0
		capability := app.service_operation_capability(window, .window_capture)!
		assert capability.support == .available
		assert C.v_multiwindow_appkit_service_capability(state,
			int(ServiceOperation.window_capture), 1) == 1
		texture := C.v_multiwindow_appkit_readback_probe_make_pattern_texture(native_window)
		assert texture != unsafe { nil }
		defer {
			C.v_multiwindow_appkit_readback_probe_release_texture(texture)
		}

		producing_frame := u64(41)
		capture := appkit_readback_submit_window(mut app, window, state, texture, 101, 102,
			producing_frame)!
		assert capture[0] == 1
		assert capture[1] == 1
		assert capture[2] == 0

		mut seen_window := false
		mut seen_image := false
		for _ in 0 .. 2 {
			result := appkit_readback_wait_result(state)!
			assert result.status == 1
			assert result.submitted_frame == producing_frame
			match result.request {
				101 {
					assert !seen_window
					seen_window = true
					assert result.width == 2
					assert result.height == 2
					_ = appkit_readback_copy_once(state, result)!
				}
				102 {
					assert !seen_image
					seen_image = true
					pixels := appkit_readback_copy_once(state, result)!
					assert pixels == [u8(17), 18, 19, 20, 33, 34, 35, 36, 81, 82, 83, 84, 97, 98,
						99, 100]
				}
				else {
					assert false, 'unexpected AppKit readback request ${result.request}'
				}
			}
		}
		assert seen_window
		assert seen_image
		assert appkit_readback_take(state).take_status == 0
	}
}

fn test_appkit_metal_readback_cancel_exactly_once_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		assert C.v_multiwindow_appkit_readback_probe_symbol_mask() == u32(0xff)
		mut app := new_app(backend: .appkit, queue_size: 16, require_renderer: true)!
		defer {
			app.stop() or {}
		}
		app.start_renderer(RendererConfig{})!
		window := app.create_window(title: 'AppKit readback cancellation', visible: false)!
		state, _, _ := appkit_readback_window_native(mut app, window)!
		assert C.v_multiwindow_appkit_readback_probe_stage_window(state, 201, 0, 0, 1, 1, 9) == 1
		assert C.v_multiwindow_appkit_readback_probe_cancel(state, 201) == 1
		assert C.v_multiwindow_appkit_readback_probe_cancel(state, 201) == 0
		assert appkit_readback_take(state).take_status == 0
		assert C.v_multiwindow_appkit_readback_probe_release(state, 201) == 0

		assert C.v_multiwindow_appkit_readback_probe_stage_window(state, 202, 0, 0, 1, 1, 10) == 1
		assert C.v_multiwindow_appkit_readback_probe_stage_window(state, 203, 0, 0, 1, 1, 10) == 1
		assert C.v_multiwindow_appkit_readback_probe_cancel_all(state) > 0
		assert appkit_readback_take(state).take_status == 0
		assert C.v_multiwindow_appkit_readback_test_record_count(state) == 0
		assert C.v_multiwindow_appkit_readback_probe_cancel_all(state) == 0
	}
}

fn test_appkit_metal_readback_cancel_preserves_matching_sibling_slot_runtime_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		mut app := new_app(backend: .appkit, queue_size: 16, require_renderer: true)!
		defer {
			app.stop() or {}
		}
		app.start_renderer(RendererConfig{})!
		window := app.create_window(
			title:           'readback sibling cancellation'
			width:           80
			height:          60
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		appkit_readback_wait_until_eligible(mut app, window)!
		state, native_window, _ := appkit_readback_window_native(mut app, window)!
		texture := C.v_multiwindow_appkit_readback_probe_make_pattern_texture(native_window)
		assert texture != unsafe { nil }
		defer {
			C.v_multiwindow_appkit_readback_probe_release_texture(texture)
		}

		assert C.v_multiwindow_appkit_readback_probe_stage_image(state, texture, 901, 0, 0, 3, 2,
			901) == 1
		assert C.v_multiwindow_appkit_readback_probe_stage_image(state, texture, 902, 0, 0, 3, 2,
			901) == 1
		assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state, texture, 901, 901) == 1
		assert C.v_multiwindow_appkit_readback_probe_cancel(state, 901) == 1
		assert C.v_multiwindow_appkit_readback_test_offscreen_slot_present() == 1
		assert C.v_multiwindow_appkit_readback_test_record_count(state) == 1
		command := C.v_multiwindow_appkit_readback_probe_make_command_buffer(native_window)
		assert command != unsafe { nil }
		assert C.v_multiwindow_appkit_readback_test_invoke_end_pass(command, unsafe { nil }) == 1
		assert C.v_multiwindow_appkit_readback_probe_commit_command_buffer(command, 1) == 1
		assert C.v_multiwindow_appkit_readback_probe_resolve(state, 901, 1) == 1
		result := appkit_readback_wait_result(state)!
		assert result.request == 902
		assert result.status == 1
		_ = appkit_readback_copy_once(state, result)!
		C.v_multiwindow_appkit_readback_probe_release_object(command)

		assert C.v_multiwindow_appkit_readback_probe_stage_image(state, texture, 903, 0, 0, 3, 2,
			903) == 1
		assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state, texture, 903, 903) == 1
		assert C.v_multiwindow_appkit_readback_probe_cancel(state, 903) == 1
		assert C.v_multiwindow_appkit_readback_test_offscreen_slot_present() == 0
		assert C.v_multiwindow_appkit_readback_test_record_count(state) == 0

		assert C.v_multiwindow_appkit_readback_probe_stage_image(state, texture, 904, 0, 0, 3, 2,
			904) == 1
		assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state, texture, 904, 904) == 1
		assert C.v_multiwindow_appkit_readback_test_offscreen_slot_present() == 1
		assert C.v_multiwindow_appkit_service_release_window_services(state) == 1
		assert C.v_multiwindow_appkit_readback_test_offscreen_slot_present() == 0
		assert C.v_multiwindow_appkit_readback_test_record_count(state) == 0
	}
}

fn test_appkit_metal_readback_offscreen_slot_two_windows_and_odd_widths_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		mut app := new_app(backend: .appkit, queue_size: 32, require_renderer: true)!
		defer {
			app.stop() or {}
		}
		app.start_renderer(RendererConfig{})!
		window_a := app.create_window(
			title:           'readback slot A'
			width:           80
			height:          60
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		window_b := app.create_window(
			title:           'readback slot B'
			width:           80
			height:          60
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		appkit_readback_wait_until_eligible(mut app, window_a)!
		appkit_readback_wait_until_eligible(mut app, window_b)!
		state_a, native_a, _ := appkit_readback_window_native(mut app, window_a)!
		state_b, native_b, _ := appkit_readback_window_native(mut app, window_b)!
		texture_a := C.v_multiwindow_appkit_readback_probe_make_pattern_texture_size(native_a, 1, 1)
		texture_b := C.v_multiwindow_appkit_readback_probe_make_pattern_texture_size(native_b, 3, 1)
		assert texture_a != unsafe { nil }
		assert texture_b != unsafe { nil }
		defer {
			C.v_multiwindow_appkit_readback_probe_release_texture(texture_a)
			C.v_multiwindow_appkit_readback_probe_release_texture(texture_b)
		}
		command_buffer := C.v_multiwindow_appkit_readback_probe_make_command_buffer(native_a)
		assert command_buffer != unsafe { nil }
		defer {
			C.v_multiwindow_appkit_readback_probe_release_object(command_buffer)
		}
		assert C.v_multiwindow_appkit_readback_probe_stage_image(state_a, texture_a, 301, 0, 0, 1,
			1, 301) == 1
		assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state_a, texture_a, 11, 301) == 1
		assert C.v_multiwindow_appkit_readback_test_invoke_end_pass(command_buffer, unsafe { nil }) == 1
		assert C.v_multiwindow_appkit_readback_test_offscreen_slot_present() == 0
		assert C.v_multiwindow_appkit_readback_probe_stage_image(state_b, texture_b, 302, 0, 0, 3,
			1, 302) == 1
		assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state_b, texture_b, 12, 302) == 1
		assert C.v_multiwindow_appkit_readback_test_invoke_end_pass(command_buffer, unsafe { nil }) == 1
		assert C.v_multiwindow_appkit_readback_probe_commit_command_buffer(command_buffer, 1) == 1
		assert C.v_multiwindow_appkit_readback_probe_resolve(state_a, 301, 1) == 1
		assert C.v_multiwindow_appkit_readback_probe_resolve(state_b, 302, 1) == 1
		result_a := appkit_readback_wait_result(state_a)!
		result_b := appkit_readback_wait_result(state_b)!
		assert result_a.request == 301
		assert result_a.stride == 4
		assert result_b.request == 302
		assert result_b.stride == 12
		_ = appkit_readback_copy_once(state_a, result_a)!
		_ = appkit_readback_copy_once(state_b, result_b)!

		for width in [1, 3, 257] {
			texture := C.v_multiwindow_appkit_readback_probe_make_pattern_texture_size(native_a,
				usize(width), 1)
			assert texture != unsafe { nil }
			request := u64(400 + width)
			command := C.v_multiwindow_appkit_readback_probe_make_command_buffer(native_a)
			assert command != unsafe { nil }
			assert C.v_multiwindow_appkit_readback_probe_stage_image(state_a, texture, request, 0,
				0, width, 1, request) == 1
			assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state_a, texture, request,
				request) == 1
			assert C.v_multiwindow_appkit_readback_test_invoke_end_pass(command, unsafe { nil }) == 1
			assert C.v_multiwindow_appkit_readback_probe_commit_command_buffer(command, 1) == 1
			assert C.v_multiwindow_appkit_readback_probe_resolve(state_a, request, 1) == 1
			result := appkit_readback_wait_result(state_a)!
			assert result.stride == width * 4
			_ = appkit_readback_copy_once(state_a, result)!
			C.v_multiwindow_appkit_readback_probe_release_object(command)
			C.v_multiwindow_appkit_readback_probe_release_texture(texture)
		}

		no_slot_texture := C.v_multiwindow_appkit_readback_probe_make_pattern_texture_size(native_a,
			1, 1)
		no_slot_command := C.v_multiwindow_appkit_readback_probe_make_command_buffer(native_a)
		assert C.v_multiwindow_appkit_readback_probe_stage_image(state_a, no_slot_texture, 501, 0,
			0, 1, 1, 501) == 1
		assert C.v_multiwindow_appkit_readback_test_invoke_end_pass(no_slot_command, unsafe { nil }) == 1
		assert C.v_multiwindow_appkit_readback_probe_commit_command_buffer(no_slot_command, 1) == 1
		assert C.v_multiwindow_appkit_readback_probe_resolve(state_a, 501, 1) == 1
		no_slot := appkit_readback_take(state_a)
		assert no_slot.take_status == 1
		assert no_slot.status == 3
		assert C.v_multiwindow_appkit_readback_probe_release(state_a, 501) == 1
		C.v_multiwindow_appkit_readback_probe_release_object(no_slot_command)
		C.v_multiwindow_appkit_readback_probe_release_texture(no_slot_texture)

		stale_texture := C.v_multiwindow_appkit_readback_probe_make_pattern_texture_size(native_a,
			1, 1)
		stale_command := C.v_multiwindow_appkit_readback_probe_make_command_buffer(native_a)
		assert C.v_multiwindow_appkit_readback_probe_stage_image(state_a, stale_texture, 502, 0, 0,
			1, 1, 502) == 1
		assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state_a, stale_texture, 502, 502) == 1
		assert C.v_multiwindow_appkit_readback_test_make_offscreen_slot_stale() == 1
		assert C.v_multiwindow_appkit_readback_test_invoke_end_pass(stale_command, unsafe { nil }) == 1
		assert C.v_multiwindow_appkit_readback_test_offscreen_slot_present() == 0
		assert C.v_multiwindow_appkit_readback_probe_commit_command_buffer(stale_command, 1) == 1
		assert C.v_multiwindow_appkit_readback_probe_resolve(state_a, 502, 1) == 1
		stale := appkit_readback_take(state_a)
		assert stale.take_status == 1
		assert stale.status == 3
		assert C.v_multiwindow_appkit_readback_probe_release(state_a, 502) == 1
		C.v_multiwindow_appkit_readback_probe_release_object(stale_command)
		C.v_multiwindow_appkit_readback_probe_release_texture(stale_texture)
	}
}

fn test_appkit_metal_readback_mixed_window_submit_and_callback_error_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		mut app := new_app(backend: .appkit, queue_size: 32, require_renderer: true)!
		defer {
			app.stop() or {}
		}
		app.start_renderer(RendererConfig{})!
		window_a := app.create_window(
			title:           'readback callback A'
			width:           48
			height:          32
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		window_b := app.create_window(
			title:           'readback callback B'
			width:           48
			height:          32
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		appkit_readback_wait_until_eligible(mut app, window_a)!
		appkit_readback_wait_until_eligible(mut app, window_b)!
		state_a, _, _ := appkit_readback_window_native(mut app, window_a)!
		state_b, _, _ := appkit_readback_window_native(mut app, window_b)!

		frame_a, _ := appkit_readback_mixed_callback_batch(mut app, window_a, window_b, state_a,
			state_b, 1001, 1002, 0, 0, 1, 1, 'injected callback failure A succeeds')!
		appkit_readback_assert_mixed_terminals(state_a, state_b, 1001, 1002, frame_a, [
			u8(0),
			0,
			255,
			255,
		])!

		app.request_redraw(window_a)!
		app.request_redraw(window_b)!
		appkit_readback_wait_until_eligible(mut app, window_a)!
		appkit_readback_wait_until_eligible(mut app, window_b)!
		frame_b, _ := appkit_readback_mixed_callback_batch(mut app, window_b, window_a, state_b,
			state_a, 1003, 1004, 1, 0, 0, 1, 'injected callback failure B succeeds')!
		appkit_readback_assert_mixed_terminals(state_b, state_a, 1003, 1004, frame_b, [
			u8(255),
			0,
			0,
			255,
		])!
	}
}

fn test_appkit_metal_readback_completion_barrier_and_deferred_failure_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		mut app := new_app(backend: .appkit, queue_size: 16, require_renderer: true)!
		defer {
			app.stop() or {}
		}
		app.start_renderer(RendererConfig{})!
		window := app.create_window(
			title:           'readback completion barrier'
			width:           80
			height:          60
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		appkit_readback_wait_until_eligible(mut app, window)!
		state, native_window, _ := appkit_readback_window_native(mut app, window)!
		texture := C.v_multiwindow_appkit_readback_probe_make_pattern_texture_size(native_window,
			257, 4)
		command := C.v_multiwindow_appkit_readback_probe_make_command_buffer(native_window)
		assert texture != unsafe { nil }
		assert command != unsafe { nil }
		defer {
			C.v_multiwindow_appkit_readback_test_release_completion()
			C.v_multiwindow_appkit_readback_probe_release_object(command)
			C.v_multiwindow_appkit_readback_probe_release_texture(texture)
		}
		assert C.v_multiwindow_appkit_readback_probe_stage_image(state, texture, 601, 0, 0, 257, 4,
			601) == 1
		assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state, texture, 601, 601) == 1
		assert C.v_multiwindow_appkit_readback_test_invoke_end_pass(command, unsafe { nil }) == 1
		assert C.v_multiwindow_appkit_readback_test_pause_completion() == 1
		assert C.v_multiwindow_appkit_readback_probe_commit_command_buffer(command, 0) == 1
		assert C.v_multiwindow_appkit_readback_test_wait_completion_paused() == 1
		assert C.v_multiwindow_appkit_readback_test_gpu_completed(state, 601) == 0
		assert C.v_multiwindow_appkit_readback_probe_resolve(state, 601, 1) == 1
		assert appkit_readback_take(state).take_status == 0
		C.v_multiwindow_appkit_readback_test_release_completion()
		assert C.v_multiwindow_appkit_readback_probe_wait_command_buffer(command) == 1
		assert C.v_multiwindow_appkit_readback_test_gpu_completed(state, 601) == 1
		assert C.v_multiwindow_appkit_readback_test_ready_count(state) == 1
		result := appkit_readback_wait_native_result(state)!
		assert result.status == 1
		assert result.submitted_frame == 601
		_ = appkit_readback_copy_once(state, result)!

		assert C.v_multiwindow_appkit_readback_probe_stage_window(state, 602, 0, 0, 1, 1, 602) == 1
		assert C.v_multiwindow_appkit_readback_test_mark_encoding_failed(state, 602) == 1
		assert appkit_readback_take(state).take_status == 0
		assert C.v_multiwindow_appkit_readback_probe_resolve(state, 602, 1) == 1
		failed := appkit_readback_take(state)
		assert failed.take_status == 1
		assert failed.status == 3
		assert failed.submitted_frame == 602
		assert failed.width == 1
		assert failed.height == 1
		assert failed.stride == 0
		assert failed.byte_length == 0
		assert C.v_multiwindow_appkit_readback_probe_release(state, 602) == 1

		assert C.v_multiwindow_appkit_readback_probe_stage_window(state, 603, 0, 0, 1, 1, 603) == 1
		assert C.v_multiwindow_appkit_readback_test_mark_encoding_failed(state, 603) == 1
		assert C.v_multiwindow_appkit_readback_probe_resolve(state, 603, 0) == 1
		failed_submit := appkit_readback_take(state)
		assert failed_submit.take_status == 1
		assert failed_submit.status == 3
		assert failed_submit.submitted_frame == 0
		assert failed_submit.width == 1
		assert failed_submit.height == 1
		assert failed_submit.stride == 0
		assert failed_submit.byte_length == 0
		assert C.v_multiwindow_appkit_readback_probe_release(state, 603) == 1
	}
}

fn test_appkit_metal_readback_ready_result_is_reaped_on_teardown_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		mut app := new_app(backend: .appkit, queue_size: 16, require_renderer: true)!
		defer {
			app.stop() or {}
		}
		app.start_renderer(RendererConfig{})!
		window := app.create_window(
			title:           'readback teardown'
			width:           80
			height:          60
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		appkit_readback_wait_until_eligible(mut app, window)!
		state, native_window, _ := appkit_readback_window_native(mut app, window)!
		texture := C.v_multiwindow_appkit_readback_probe_make_pattern_texture(native_window)
		command := C.v_multiwindow_appkit_readback_probe_make_command_buffer(native_window)
		assert C.v_multiwindow_appkit_readback_probe_stage_image(state, texture, 701, 0, 0, 3, 2,
			701) == 1
		assert C.v_multiwindow_appkit_readback_probe_arm_offscreen(state, texture, 701, 701) == 1
		assert C.v_multiwindow_appkit_readback_test_invoke_end_pass(command, unsafe { nil }) == 1
		assert C.v_multiwindow_appkit_readback_probe_commit_command_buffer(command, 1) == 1
		assert C.v_multiwindow_appkit_readback_probe_resolve(state, 701, 1) == 1
		appkit_readback_wait_ready_count(mut app, state, 1)!
		assert C.v_multiwindow_appkit_service_release_window_services(state) == 1
		assert C.v_multiwindow_appkit_readback_test_record_count(state) == 0
		assert appkit_readback_take(state).take_status == 0
		C.v_multiwindow_appkit_readback_probe_release_object(command)
		C.v_multiwindow_appkit_readback_probe_release_texture(texture)
	}
}

fn test_appkit_metal_readback_hook_reinstalls_after_renderer_restart_red() {
	$if darwin && sokol_metal ? && gg_multiwindow ? {
		if !appkit_readback_runtime_requested() {
			return
		}
		mut app := new_app(backend: .appkit, queue_size: 16, require_renderer: true)!
		defer {
			app.stop() or {}
		}
		app.start_renderer(RendererConfig{})!
		window := app.create_window(
			title:           'readback renderer restart'
			width:           80
			height:          60
			visible:         true
			redraw_mode:     .on_demand
			render_workload: true
		)!
		appkit_readback_wait_until_eligible(mut app, window)!
		state_before, _, _ := appkit_readback_window_native(mut app, window)!
		_ =
			appkit_readback_submit_window(mut app, window, state_before, unsafe { nil }, 801, 0, 801)!
		before := appkit_readback_wait_native_result(state_before)!
		assert before.status == 1
		_ = appkit_readback_copy_once(state_before, before)!
		app.shutdown_renderer()!
		quiesced := app.service_operation_capability(window, .window_capture)!
		assert quiesced.support == .unsupported
		app.start_renderer(RendererConfig{})!
		app.request_redraw(window)!
		state_after, _, framebuffer_only := appkit_readback_window_native(mut app, window)!
		assert framebuffer_only == 0
		capability := app.service_operation_capability(window, .window_capture)!
		assert capability.support == .available
		assert C.v_multiwindow_appkit_service_capability(state_after,
			int(ServiceOperation.window_capture), 1) == 1
		appkit_readback_wait_until_eligible(mut app, window)!
		_ =
			appkit_readback_submit_window(mut app, window, state_after, unsafe { nil }, 802, 0, 802)!
		after := appkit_readback_wait_native_result(state_after)!
		assert after.status == 1
		_ = appkit_readback_copy_once(state_after, after)!
	}
}
