module multiwindow

import os

struct AppKitContractNeed {
	label  string
	source string
	needle string
}

fn appkit_contract_source(name string) string {
	return os.read_file(os.join_path(@DIR, name)) or { panic(err) }
}

fn appkit_gg_contract_source(name string) string {
	return os.read_file(os.join_path(@DIR, '..', '..', 'gg', name)) or { panic(err) }
}

fn appkit_contract_compact(source string) string {
	return source.replace('\r', '').replace('\n', '').replace('\t', '').replace(' ', '')
}

fn appkit_contract_missing(needs []AppKitContractNeed) []string {
	mut missing := []string{}
	for need in needs {
		if !need.source.contains(need.needle) {
			missing << '${need.label}: `${need.needle}`'
		}
	}
	return missing
}

fn assert_appkit_contract(needs []AppKitContractNeed) {
	missing := appkit_contract_missing(needs)
	assert missing.len == 0, 'missing AppKit Package2 contracts:\n${missing.join('\n')}'
}

fn test_appkit_common_capability_and_state_conversion() {
	capability := appkit_service_capability_from_native(.clipboard_read, 1, false)
	assert capability.support == .available
	assert capability.asynchronous
	assert !capability.state_observable
	assert appkit_service_capability_from_native(.show, 2, false).state_observable
	capture := appkit_service_capability_from_native(.window_capture, 1, true)
	assert capture.support == .available
	assert capture.asynchronous
	assert appkit_service_capability_from_native(.window_capture, 1, false).support == .unsupported
	assert appkit_service_capability_from_native(.portal_parent, 1, false).support == .unsupported
	assert appkit_service_capability_from_native(.show, 99, false).support == .unsupported

	monitors := [
		AppKitServiceMonitorRecord{
			native_id:  91
			slot:       3
			generation: 7
			available:  true
		},
	]
	state := appkit_service_window_state_from_raw(AppKitServiceRawWindowState{
		mapping:           2
		visibility:        3
		active:            2
		focused:           2
		minimized:         1
		maximized:         2
		fullscreen:        1
		mouse_locked:      2
		position_known:    1
		x:                 12
		y:                 34
		monitor_native_id: 91
	}, monitors, 44)
	assert state.mapping == .mapped
	assert state.visibility == .occluded
	assert state.active == .on
	assert state.focused == .on
	assert state.minimized == .off
	assert state.maximized == .on
	assert state.fullscreen == .off
	assert state.mouse_locked == .on
	assert state.position == ServicePosition{
		known: true
		x:     12
		y:     34
	}
	assert state.monitor_ids == [
		ServiceMonitorId{
			app_instance: 44
			slot:         3
			generation:   7
		},
	]

	unknown := appkit_service_window_state_from_raw(AppKitServiceRawWindowState{
		mapping:        -1
		visibility:     99
		active:         99
		position_known: 2
	}, monitors, 44)
	assert unknown.mapping == .unknown
	assert unknown.visibility == .unknown
	assert unknown.active == .unknown
	assert !unknown.position.known
}

fn test_appkit_common_monitor_identity_reconciliation() {
	mut records := []AppKitServiceMonitorRecord{}
	first := appkit_reconcile_service_monitors(mut records, [
		AppKitServiceRawMonitor{
			native_id: 11
			name:      'Built-in'
			width:     100
			height:    80
			scale:     2.0
			primary:   2
		},
		AppKitServiceRawMonitor{
			native_id: 22
			name:      'External'
			x:         100
			width:     120
			height:    90
			scale:     1.0
			primary:   1
		},
	], 55)
	assert first.len == 2
	assert first[0].id == ServiceMonitorId{
		app_instance: 55
		slot:         0
		generation:   1
	}
	assert first[1].id == ServiceMonitorId{
		app_instance: 55
		slot:         1
		generation:   1
	}
	assert first[0].geometry.known
	assert first[0].scale == ServiceKnownScale{
		known: true
		value: 2.0
	}

	second := appkit_reconcile_service_monitors(mut records, [
		AppKitServiceRawMonitor{
			native_id: 22
			name:      'External'
			x:         100
			width:     120
			height:    90
			scale:     1.0
			primary:   2
		},
	], 55)
	assert second.len == 1
	assert second[0].id.slot == 1
	assert records[0].available == false

	third := appkit_reconcile_service_monitors(mut records, [
		AppKitServiceRawMonitor{
			native_id: 11
			name:      'Built-in'
			width:     100
			height:    80
			scale:     2.0
			primary:   2
		},
		AppKitServiceRawMonitor{
			native_id: 22
			name:      'External'
			x:         100
			width:     120
			height:    90
			scale:     1.0
			primary:   1
		},
	], 55)
	assert third[0].id.slot == 0
	assert third[0].id.generation == 2
	assert third[1].id.generation == 1
}

fn test_appkit_common_clipboard_owner_borrow_and_titlebar_contracts() {
	assert appkit_clipboard_length_is_valid(0)
	assert appkit_clipboard_length_is_valid(service_appkit_clipboard_max_bytes)
	assert !appkit_clipboard_length_is_valid(service_appkit_clipboard_max_bytes + 1)
	parent := WindowId{
		app_instance: 9
		slot:         0
		generation:   1
	}
	parent_state := voidptr(usize(0x1234))
	records := [AppKitWindowRecord{ id: parent, state: parent_state }]
	assert appkit_owner_native_state(parent, records)! == parent_state
	appkit_owner_native_state(WindowId{ app_instance: 9, slot: 1, generation: 1 }, records) or {
		assert err.msg() == err_window_not_found
	}
	borrow := appkit_native_window_borrow_from_pointer(voidptr(usize(0x5678)))!
	assert borrow.backend == .appkit
	assert borrow.primary == voidptr(usize(0x5678))
	appkit_native_window_borrow_from_pointer(unsafe { nil }) or {
		assert err.msg() == err_capability_unsupported
	}
	assert appkit_titlebar_appearance_code(.system) == 0
	assert appkit_titlebar_appearance_code(.light) == 1
	assert appkit_titlebar_appearance_code(.dark) == 2
}

fn test_appkit_common_canonical_events_and_cleanup_guard() {
	record := AppKitWindowRecord{
		id:                 WindowId{
			app_instance: 7
			slot:         2
			generation:   3
		}
		width:              320
		height:             200
		framebuffer_width:  640
		framebuffer_height: 400
	}
	state := ServiceWindowState{
		mapping:    .mapped
		visibility: .visible
		focused:    .on
	}
	focus := appkit_service_events_from_observation(record, .focused, state, 2.0)
	assert focus.len == 1
	assert focus[0].kind == .service
	assert focus[0].service.kind == .state
	assert focus[0].service.operation == .focus
	metrics := appkit_service_events_from_observation(record, .resized, state, 2.0)
	assert metrics.len == 1
	assert metrics[0].service.kind == .metrics
	assert metrics[0].service.metrics.logical_width == 320
	assert metrics[0].service.metrics.framebuffer_width == 640
	assert metrics[0].service.metrics.dpi_scale == 2.0
	assert appkit_service_events_from_observation(record, .key_down, state, 2.0).len == 0
	observed_record := AppKitWindowRecord{
		...record
		service_state_observed: true
	}
	assert appkit_service_transition_operations(observed_record, ServiceWindowState{
		minimized:  .off
		maximized:  .on
		fullscreen: .off
	}) == [.maximize]
	assert appkit_service_transition_operations(AppKitWindowRecord{
		...observed_record
		observed_maximized: true
	}, ServiceWindowState{
		minimized:  .off
		maximized:  .off
		fullscreen: .off
	}) == [.restore]

	mut cleanup_record := AppKitWindowRecord{}
	assert appkit_complete_window_service_release(mut cleanup_record)
	assert cleanup_record.services_released
	assert !appkit_complete_window_service_release(mut cleanup_record)
}

fn test_appkit_common_clipboard_terminal_is_delivered_exactly_once() {
	mut app := new_app(Config{
		backend: .mock
	})!
	window := app.create_window()!
	_ = app.drain_events()!
	request := app.begin_native_clipboard_request(window, .clipboard_read, false)!.request
	app.complete_native_clipboard_request(request, window, .clipboard_read, 'AppKit bridge', 0)!
	events := app.drain_service_events()!
	assert events.len == 1
	assert events[0].kind == .clipboard
	assert events[0].clipboard.id == request
	assert events[0].clipboard.status == .ready
	assert events[0].clipboard.text == 'AppKit bridge'
	app.complete_native_clipboard_request(request, window, .clipboard_read, 'duplicate', 0) or {
		assert err.msg() == err_service_request_stale
	}
	assert app.drain_service_events()!.len == 0
	app.stop()!
}

fn test_appkit_package2_window_controls_and_capabilities_red() {
	dispatch := appkit_contract_compact(appkit_contract_source('service_backend.v'))
	backend := appkit_contract_source('appkit_backend.c.v')
	assert_appkit_contract([
		AppKitContractNeed{'capability dispatch', dispatch, '.appkit{backend.appkit.service_operation_capability(id,operation)}'},
		AppKitContractNeed{'show dispatch', dispatch, '.appkit{backend.appkit.service_show_window(id)!}'},
		AppKitContractNeed{'hide dispatch', dispatch, '.appkit{backend.appkit.service_hide_window(id)!}'},
		AppKitContractNeed{'focus dispatch', dispatch, '.appkit{backend.appkit.service_focus_window(id)!}'},
		AppKitContractNeed{'raise dispatch', dispatch, '.appkit{backend.appkit.service_raise_window(id)!}'},
		AppKitContractNeed{'position dispatch', dispatch, '.appkit{backend.appkit.service_set_window_position(id,x,y)!}'},
		AppKitContractNeed{'minimize dispatch', dispatch, '.appkit{backend.appkit.service_minimize_window(id)!}'},
		AppKitContractNeed{'maximize dispatch', dispatch, '.appkit{backend.appkit.service_maximize_window(id)!}'},
		AppKitContractNeed{'restore dispatch', dispatch, '.appkit{backend.appkit.service_restore_window(id)!}'},
		AppKitContractNeed{'fullscreen dispatch', dispatch, '.appkit{backend.appkit.service_set_fullscreen(id,enabled)!}'},
		AppKitContractNeed{'native observed state', backend, 'fn (backend &AppKitBackend) service_window_state(id WindowId) !ServiceWindowState'},
	])
}

fn test_appkit_package2_runtime_monitors_scale_and_events_red() {
	dispatch := appkit_contract_compact(appkit_contract_source('service_backend.v'))
	backend := appkit_contract_source('appkit_backend.c.v')
	assert_appkit_contract([
		AppKitContractNeed{'monitor dispatch', dispatch, '.appkit{backend.appkit.service_monitor_snapshot(app_instance)!}'},
		AppKitContractNeed{'monitor snapshot', backend, 'fn (mut backend AppKitBackend) service_monitor_snapshot(app_instance u64) ![]ServiceMonitorInfo'},
		AppKitContractNeed{'screen enumeration ABI', backend, 'fn C.v_multiwindow_appkit_service_monitor_count() int'},
		AppKitContractNeed{'geometry/work area/scale ABI', backend, 'fn C.v_multiwindow_appkit_service_monitor_info'},
		AppKitContractNeed{'screen topology revision ABI', backend, 'fn C.v_multiwindow_appkit_service_monitor_revision() u64'},
		AppKitContractNeed{'generation-safe reconciliation', backend, 'fn appkit_reconcile_service_monitors'},
		AppKitContractNeed{'backing-scale state output', backend, 'out_scale &f32'},
		AppKitContractNeed{'canonical service event', backend, 'fn (mut backend AppKitBackend) monitor_change_event() ![]QueuedEvent'},
	])
}

fn test_appkit_package2_clipboard_red() {
	dispatch := appkit_contract_compact(appkit_contract_source('service_backend.v'))
	backend := appkit_contract_source('appkit_backend.c.v')
	assert_appkit_contract([
		AppKitContractNeed{'clipboard write dispatch', dispatch, '.appkit{backend.appkit.service_set_clipboard_text(id,request,text)!}'},
		AppKitContractNeed{'clipboard read dispatch', dispatch, '.appkit{backend.appkit.service_request_clipboard_text(id,request)!}'},
		AppKitContractNeed{'clipboard write backend', backend, 'fn (mut backend AppKitBackend) service_set_clipboard_text'},
		AppKitContractNeed{'clipboard read backend', backend, 'fn (mut backend AppKitBackend) service_request_clipboard_text'},
		AppKitContractNeed{'bounded clipboard payload', backend, 'const service_appkit_clipboard_max_bytes = 16 * 1024 * 1024'},
		AppKitContractNeed{'UTF-8 write ABI', backend, 'fn C.v_multiwindow_appkit_service_set_clipboard_text'},
		AppKitContractNeed{'bounded read length ABI', backend, 'fn C.v_multiwindow_appkit_service_clipboard_text_length'},
		AppKitContractNeed{'UTF-8 read ABI', backend, 'fn C.v_multiwindow_appkit_service_copy_clipboard_text'},
	])
}

fn test_appkit_package2_owner_modal_borrow_and_accessibility_seam_red() {
	dispatch := appkit_contract_compact(appkit_contract_source('service_backend.v'))
	backend := appkit_contract_source('appkit_backend.c.v')
	gg_enabled := appkit_gg_contract_source('multiwindow_d_gg_multiwindow.v')
	gg_disabled := appkit_gg_contract_source('multiwindow_notd_gg_multiwindow.v')
	for facade in [gg_enabled, gg_disabled] {
		assert facade.contains('pub fn (mut app App) with_native_window')
		assert facade.contains('pub fn (mut lease NativeWindowLease) with_appkit')
	}
	assert_appkit_contract([
		AppKitContractNeed{'native borrow dispatch', dispatch, '.appkit{backend.appkit.service_native_window_borrow(id)!}'},
		AppKitContractNeed{'native NSWindow borrow', backend, 'fn (backend &AppKitBackend) service_native_window_borrow(id WindowId) !BackendNativeWindowBorrow'},
		AppKitContractNeed{'owner/modal relation ABI', backend, 'fn C.v_multiwindow_appkit_service_set_owner'},
		AppKitContractNeed{'owner detach ABI', backend, 'fn C.v_multiwindow_appkit_service_clear_owner'},
		AppKitContractNeed{'owner/modal record fields', backend, 'owner ?WindowId'},
		AppKitContractNeed{'bounded NSWindow handle ABI', backend, 'fn C.v_multiwindow_appkit_service_native_window(state voidptr) voidptr'},
	])
}

fn test_appkit_package2_mouse_lock_red() {
	dispatch := appkit_contract_compact(appkit_contract_source('service_backend.v'))
	backend := appkit_contract_source('appkit_backend.c.v')
	assert_appkit_contract([
		AppKitContractNeed{'mouse lock dispatch', dispatch, '.appkit{backend.appkit.service_set_mouse_lock(id,enabled)!}'},
		AppKitContractNeed{'mouse lock backend', backend, 'fn (mut backend AppKitBackend) service_set_mouse_lock'},
		AppKitContractNeed{'mouse lock ABI', backend, 'fn C.v_multiwindow_appkit_service_set_mouse_lock'},
		AppKitContractNeed{'observed lock state', backend, '&raw.mouse_locked'},
		AppKitContractNeed{'cleanup revocation', backend, 'C.v_multiwindow_appkit_service_set_mouse_lock(record.state, 0)'},
		AppKitContractNeed{'canonical focus-loss state', backend, '.focused, .unfocused'},
	])
}

fn test_appkit_package2_titlebar_red() {
	api := appkit_contract_compact(appkit_contract_source('service_api.v'))
	dispatch := appkit_contract_source('service_backend.v')
	backend := appkit_contract_source('appkit_backend.c.v')
	assert_appkit_contract([
		AppKitContractNeed{'native titlebar API path', api, 'app.backend.service_set_titlebar_appearance(id,appearance)!'},
		AppKitContractNeed{'titlebar dispatch', dispatch, 'service_set_titlebar_appearance'},
		AppKitContractNeed{'titlebar backend', backend, 'fn (mut backend AppKitBackend) service_set_titlebar_appearance'},
		AppKitContractNeed{'titlebar ABI', backend, 'fn C.v_multiwindow_appkit_service_set_titlebar_appearance'},
		AppKitContractNeed{'system appearance conversion', backend, '.system { 0 }'},
		AppKitContractNeed{'light appearance conversion', backend, '.light { 1 }'},
		AppKitContractNeed{'dark appearance conversion', backend, '.dark { 2 }'},
	])
}

fn test_appkit_package2_managed_readback_and_window_capture_red() {
	dispatch := appkit_contract_compact(appkit_contract_source('service_backend.v'))
	backend := appkit_contract_source('appkit_backend.c.v')
	native := appkit_contract_source('appkit_backend.m')
	render := appkit_gg_contract_source('multiwindow_render_impl_d_gg_multiwindow.v')
	scheduler := appkit_contract_source('render_scheduler.v')
	assert_appkit_contract([
		AppKitContractNeed{'window readback staging dispatch', dispatch, '.appkit{backend.appkit.service_stage_window_readback('},
		AppKitContractNeed{'Metal capability path', render, 'gfx.query_backend() == .metal_macos'},
		AppKitContractNeed{'AppKit readback staging backend', backend, 'service_stage_window_readback'},
		AppKitContractNeed{'per-window submission resolution', scheduler, 'service_resolve_readbacks_after_submit'},
		AppKitContractNeed{'canonical backend collection', backend, 'queued_readback_events'},
		AppKitContractNeed{'drawable copy source', native, 'copyFromTexture:'},
		AppKitContractNeed{'CPU staging buffer', native, 'newBufferWithLength:'},
		AppKitContractNeed{'GPU completion', native, 'addCompletedHandler:'},
		AppKitContractNeed{'BGRA normalization', native, 'v_multiwindow_appkit_bgra_to_rgba'},
		AppKitContractNeed{'capture-capable drawable', native, 'framebufferOnly = NO'},
	])
}

fn test_appkit_package2_teardown_exactly_once_red() {
	backend := appkit_contract_source('appkit_backend.c.v')
	assert_appkit_contract([
		AppKitContractNeed{'native service cleanup', backend, 'fn (mut backend AppKitBackend) release_window_services'},
		AppKitContractNeed{'mouse lock cleanup ABI', backend, 'v_multiwindow_appkit_service_set_mouse_lock(record.state, 0)'},
		AppKitContractNeed{'owner cleanup ABI', backend, 'v_multiwindow_appkit_service_clear_owner(record.state)'},
		AppKitContractNeed{'accessibility detach ABI', backend, 'v_multiwindow_appkit_service_detach_accessibility(record.state)'},
		AppKitContractNeed{'native service release ABI', backend, 'v_multiwindow_appkit_service_release_window_services(record.state)'},
		AppKitContractNeed{'exactly-once guard', backend, 'services_released'},
		AppKitContractNeed{'cleanup before native lifetime release', backend, 'if !backend.release_window_services(mut record)'},
	])
}
