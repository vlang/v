module multiwindow

import os
import time

fn wayland_runtime_probe_available() bool {
	if os.getenv('WAYLAND_DISPLAY') != '' {
		return true
	}
	eprintln('SKIP Wayland native Package2 probe: WAYLAND_DISPLAY is not set')
	assert os.getenv('VGG_MULTIWINDOW_RUNTIME_PROBES') != '1', 'Wayland runtime probes were required, but no compositor is available'
	return false
}

fn test_wayland_app_id_reaches_the_native_marshal_boundary() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		expected := 'org.vlang.package2-wayland-proof'
		mut app := new_app(
			backend:          .wayland
			require_renderer: false
			app_id:           expected
		)!
		_ = app.create_window(title: 'package2-wayland-app-id')!
		marshaled := unsafe { cstring_to_vstring(C.v_multiwindow_wayland_get_last_marshaled_app_id()) }
		assert marshaled == expected
		app.stop()!
	}
}

fn test_wayland_owner_relation_reaches_xdg_toplevel_parent() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		owner := app.create_window(title: 'package2-wayland-owner')!
		child := app.create_window(
			title: 'package2-wayland-modal-child'
			owner: owner
			modal: true
		)!
		owner_index := app.backend.wayland.window_record_index(owner) or {
			app.stop()!
			panic(err_window_not_found)
		}
		child_index := app.backend.wayland.window_record_index(child) or {
			app.stop()!
			panic(err_window_not_found)
		}
		assert C.v_multiwindow_wayland_get_last_parent_child() == usize(app.backend.wayland.windows[child_index].xdg_toplevel)
		assert C.v_multiwindow_wayland_get_last_parent_owner() == usize(app.backend.wayland.windows[owner_index].xdg_toplevel)
		service_index := app.services.window_index(child)!
		registered_owner := app.services.windows[service_index].owner or {
			app.stop()!
			panic(err_window_not_found)
		}

		assert registered_owner == owner
		assert app.services.windows[service_index].modal
		app.stop()!
	}
}

fn test_wayland_cursor_support_is_runtime_and_shape_specific() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		assert backend.cursor_support(.pointer) == .conditional
		assert backend.cursor_support(.resize_all) == .unsupported
		backend.pointer = voidptr(usize(0x11))
		backend.cursor_shape_manager = voidptr(usize(0x12))
		backend.cursor_shape_device = voidptr(usize(0x13))
		for shape in [CursorShape.default, .pointer, .move, .n_resize, .s_resize, .e_resize,
			.w_resize, .ne_resize, .nw_resize, .se_resize, .sw_resize, .ew_resize, .ns_resize,
			.nesw_resize, .nwse_resize, .grab, .grabbing, .text, .crosshair, .not_allowed] {
			assert backend.cursor_support(shape) == .available
		}
		assert backend.cursor_support(.resize_all) == .unsupported
	}
}

fn test_wayland_output_removal_updates_membership_scale_and_metrics() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		output := &WaylandOutputRecord{
			slot:       0
			owner:      backend
			generation: 3
			scale:      2
			ready:      true
			available:  true
		}
		window := &WaylandWindowRecord{
			id:           WindowId{
				app_instance: 1
				slot:         0
				generation:   1
			}
			owner:        backend
			high_dpi:     true
			configured:   true
			width:        40
			height:       30
			buffer_scale: 2
			output_slots: [0]
		}
		backend.outputs = [output]
		backend.windows = [window]

		backend.destroy_output_record(0)

		assert !output.available
		assert output.generation == 3
		assert window.output_slots.len == 0
		assert window.buffer_scale == 1
		assert window.service_window_state().monitor_ids.len == 0
		assert window.service_window_state().monitor_membership_observed
		assert window.pending_events.len == 1
		assert window.pending_events[0].event.kind == .service
		assert window.pending_events[0].event.service.kind == .metrics
		assert window.pending_events[0].event.service.metrics.dpi_scale == f32(1)
	}
}

fn test_wayland_output_scale_commits_at_done_and_refreshes_only_member_windows() {
	$if linux && sokol_wayland ? {
		mut native_operations := &NativeOperationAuthority{}
		native_operations.bind_app_lifetime(1, 1)!
		native_operations.advance_renderer_attempt(1, 2)!
		mut backend := &WaylandBackend{
			started:           true
			display:           voidptr(usize(0x20))
			native_operations: native_operations
		}
		proxy := voidptr(usize(0x21))
		output := &WaylandOutputRecord{
			slot:              0
			owner:             backend
			global_name:       7
			bound_version:     4
			generation:        3
			proxy:             proxy
			mode_width:        1920
			mode_height:       1080
			scale:             1
			geometry_received: true
			mode_received:     true
			ready:             true
			available:         true
		}
		larger := &WaylandOutputRecord{
			slot:              1
			owner:             backend
			global_name:       8
			bound_version:     4
			generation:        4
			proxy:             voidptr(usize(0x22))
			mode_width:        2560
			mode_height:       1440
			scale:             3
			geometry_received: true
			mode_received:     true
			ready:             true
			available:         true
		}
		member := &WaylandWindowRecord{
			id:                       WindowId{
				app_instance: 1
				slot:         0
				generation:   1
			}
			owner:                    backend
			high_dpi:                 true
			configured:               true
			width:                    40
			height:                   30
			buffer_scale:             1
			render_scale:             1
			render_target_generation: 5
			output_slots:             [0]
		}
		nonmember := &WaylandWindowRecord{
			id:                       WindowId{
				app_instance: 1
				slot:         1
				generation:   1
			}
			owner:                    backend
			high_dpi:                 true
			configured:               true
			width:                    50
			height:                   20
			buffer_scale:             3
			render_scale:             3
			render_target_generation: 7
			output_slots:             [1]
		}
		multi_output := &WaylandWindowRecord{
			id:                       WindowId{
				app_instance: 1
				slot:         2
				generation:   1
			}
			owner:                    backend
			high_dpi:                 true
			configured:               true
			width:                    60
			height:                   25
			buffer_scale:             3
			render_scale:             3
			render_target_generation: 9
			output_slots:             [0, 1]
		}
		fractional := &WaylandWindowRecord{
			id:                         WindowId{
				app_instance: 1
				slot:         3
				generation:   1
			}
			owner:                      backend
			high_dpi:                   true
			configured:                 true
			width:                      64
			height:                     48
			buffer_scale:               1
			render_scale:               1.5
			fractional_scale:           voidptr(usize(0x23))
			fractional_scale_numerator: 180
			viewport:                   voidptr(usize(0x24))
			render_target_generation:   11
			output_slots:               [0]
		}
		backend.outputs = [output, larger]
		backend.windows = [member, nonmember, multi_output, fractional]

		wayland_output_scale(output.listener_data(), proxy, 2)
		assert output.scale == 1
		assert member.buffer_scale == 1
		assert member.pending_events.len == 0
		assert backend.pending_service_events.len == 0

		wayland_output_done(output.listener_data(), proxy)
		assert output.scale == 2
		assert backend.pending_service_events.len == 1
		monitor_event := backend.pending_service_events[0]
		assert monitor_event.event.kind == .service
		assert monitor_event.event.service.kind == .monitor
		assert monitor_event.event.service.monitors[0].scale.value == f32(2)
		assert member.buffer_scale == 2
		assert member.render_scale == f32(2)
		assert member.pending_egl_resize
		assert member.render_target_generation == 6
		assert member.pending_events.len == 1
		assert member.pending_events[0].event.service.kind == .metrics
		assert monitor_event.sequence < member.pending_events[0].sequence
		assert nonmember.buffer_scale == 3
		assert nonmember.render_target_generation == 7
		assert nonmember.pending_events.len == 0
		assert multi_output.buffer_scale == 3
		assert multi_output.render_target_generation == 9
		assert multi_output.pending_events.len == 0
		assert fractional.buffer_scale == 2
		assert fractional.render_scale == f32(1.5)
		assert fractional.render_target_generation == 11
		assert fractional.pending_events.len == 0

		wayland_output_done(output.listener_data(), proxy)
		assert backend.pending_service_events.len == 1
		assert member.pending_events.len == 1

		wayland_output_scale(output.listener_data(), proxy, 4)
		assert output.scale == 2
		wayland_output_done(output.listener_data(), proxy)
		assert output.scale == 4
		assert backend.pending_service_events.len == 2
		assert member.buffer_scale == 4
		assert member.render_target_generation == 7
		assert member.pending_events.len == 2
		assert multi_output.buffer_scale == 4
		assert multi_output.render_target_generation == 10
		assert multi_output.pending_events.len == 1
		assert nonmember.pending_events.len == 0
	}
}

fn test_wayland_fractional_scale_preference_updates_metrics_and_framebuffer() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		fractional_scale := voidptr(usize(0x31))
		viewport := voidptr(usize(0x32))
		mut record := &WaylandWindowRecord{
			id:                       WindowId{
				app_instance: 1
				slot:         0
				generation:   1
			}
			owner:                    backend
			high_dpi:                 true
			width:                    101
			height:                   51
			configured:               true
			fractional_scale:         fractional_scale
			viewport:                 viewport
			render_target_generation: 1
		}
		backend.windows << record

		wayland_fractional_scale_preferred(record.listener_data(), fractional_scale, 180)

		assert record.fractional_scale_numerator == 180
		assert record.render_scale == f32(1.5)
		assert record.pending_egl_resize
		assert record.render_target_generation == 2
		assert record.pending_events.len == 1
		assert record.pending_events[0].event.kind == .service
		metrics := record.pending_events[0].event.service.metrics
		assert metrics.framebuffer_width == 152
		assert metrics.framebuffer_height == 77
		assert metrics.dpi_scale == f32(1.5)
	}
}

fn test_wayland_framebuffer_extent_uses_widened_ceil_and_protocol_clamp() {
	$if linux && sokol_wayland ? {
		assert wayland_framebuffer_extent(0, 1, 1) == 1
		assert wayland_framebuffer_extent(-1, 1, 1) == 1
		assert wayland_framebuffer_extent(101, 180, 120) == 152
		assert wayland_framebuffer_extent(1_073_741_823, 2, 1) == 2_147_483_646
		assert wayland_framebuffer_extent(2_147_483_647, 1, 1) == 2_147_483_647
		assert wayland_framebuffer_extent(2_147_483_647, 2, 1) == 2_147_483_647
		assert wayland_framebuffer_extent(2_147_483_647, u64(~u32(0)),
			u64(wayland_fractional_scale_denominator)) == 2_147_483_647
	}
}

fn test_wayland_shm_layout_rejects_stride_and_size_narrowing_before_protocol_use() {
	$if linux && sokol_wayland ? {
		mut stride := i32(0)
		mut size := i32(0)
		assert C.v_multiwindow_wayland_shm_layout(1, 1, &stride, &size) == 1
		assert stride == 4
		assert size == 4

		assert C.v_multiwindow_wayland_shm_layout(536_870_911, 1, &stride, &size) == 1
		assert stride == 2_147_483_644
		assert size == 2_147_483_644
		assert C.v_multiwindow_wayland_shm_layout(536_870_912, 1, &stride, &size) == 0
		assert C.v_multiwindow_wayland_shm_layout(1, 536_870_911, &stride, &size) == 1
		assert stride == 4
		assert size == 2_147_483_644
		assert C.v_multiwindow_wayland_shm_layout(1, 536_870_912, &stride, &size) == 0
		assert C.v_multiwindow_wayland_shm_layout(0, 1, &stride, &size) == 0
	}
}

fn test_wayland_hide_barrier_and_hidden_state_ignore_configure_without_publishing() {
	$if linux && sokol_wayland ? {
		mut hidden := &WaylandWindowRecord{
			width:                       80
			height:                      60
			configured:                  true
			requested_visible:           false
			hide_barrier_active:         true
			pending_toplevel_width:      120
			pending_toplevel_height:     90
			pending_egl_resize:          true
			pending_service_state_valid: true
		}
		wayland_xdg_toplevel_configure(hidden.listener_data(), unsafe { nil }, 120, 90,
			unsafe { nil })
		assert hidden.pending_toplevel_width == 120
		assert hidden.pending_toplevel_height == 90
		assert hidden.pending_service_state_valid
		wayland_xdg_surface_configure(hidden.listener_data(), unsafe { nil }, 7)
		assert hidden.configured
		assert hidden.pending_toplevel_width == 120
		assert hidden.pending_toplevel_height == 90
		assert hidden.pending_egl_resize
		assert hidden.pending_service_state_valid

		mut initial_hidden := &WaylandWindowRecord{
			width:             80
			height:            60
			requested_visible: false
		}
		wayland_xdg_surface_configure(initial_hidden.listener_data(), unsafe { nil }, 8)
		assert !initial_hidden.configured
		assert !initial_hidden.frame_ready
		assert !initial_hidden.pending_service_state_valid
		assert initial_hidden.pending_events.len == 0
	}
}

fn test_wayland_remap_replays_persisted_toplevel_attributes_before_commit_and_parent_after_map() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{
			toplevel_replay_test: WaylandToplevelReplayTestSeam{
				active:                  true
				show_handshake_override: true
			}
		}
		owner_id := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		child_id := WindowId{
			app_instance: 1
			slot:         1
			generation:   1
		}
		mut owner := &WaylandWindowRecord{
			id:           owner_id
			owner:        backend
			xdg_toplevel: voidptr(usize(0xb1))
			mapped:       true
		}
		mut child := &WaylandWindowRecord{
			id:                        child_id
			owner:                     backend
			owner_id:                  owner_id
			title:                     'restored title'
			app_id:                    'org.vlang.remap'
			min_width:                 20
			min_height:                10
			max_width:                 200
			max_height:                100
			request_server_decoration: true
			requested_maximized:       true
			requested_fullscreen:      true
			surface:                   voidptr(usize(0xb2))
			xdg_toplevel:              voidptr(usize(0xb3))
			toplevel_decoration:       voidptr(usize(0xb4))
			mapped:                    true
		}
		backend.windows << owner
		backend.windows << child

		backend.replay_window_toplevel_attributes(1)
		backend.commit_window_show_handshake(1)
		assert backend.toplevel_replay_test.operations == ['title', 'app_id', 'parent', 'min_size',
			'max_size', 'decoration', 'maximize', 'fullscreen', 'commit']
		backend.toplevel_replay_test.operations.clear()
		backend.windows[1].requested_maximized = false
		backend.windows[1].requested_fullscreen = false
		backend.replay_window_toplevel_attributes(1)
		backend.commit_window_show_handshake(1)
		assert backend.toplevel_replay_test.operations == ['title', 'app_id', 'parent', 'min_size',
			'max_size', 'decoration', 'unmaximize', 'unfullscreen', 'commit']
		backend.toplevel_replay_test.operations.clear()
		backend.reapply_parent_to_live_children_on_first_map(owner_id, true)
		assert backend.toplevel_replay_test.operations.len == 0
		backend.reapply_parent_to_live_children_on_first_map(owner_id, false)
		assert backend.toplevel_replay_test.operations == ['child_parent']
	}
}

fn test_wayland_hide_barrier_failure_preserves_visible_window_state() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{
			display:              voidptr(usize(0xc1))
			toplevel_replay_test: WaylandToplevelReplayTestSeam{
				hide_barrier_override: true
			}
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:                             window
			owner:                          backend
			surface:                        voidptr(usize(0xc2))
			requested_visible:              true
			mapped:                         true
			configured:                     true
			frame_ready:                    true
			pending_toplevel_width:         77
			pending_toplevel_height:        55
			pending_egl_resize:             true
			pending_service_state_valid:    true
			observed_service_state_valid:   true
			toplevel_decoration_configured: true
			toplevel_decoration_mode:       wayland_xdg_toplevel_decoration_mode_server_side
			render_target_generation:       9
		}
		backend.windows << record
		mut failure := ''
		backend.service_hide_window(window) or { failure = err.msg() }
		assert failure == err_wayland_dispatch_failed
		assert record.requested_visible
		assert record.mapped
		assert record.configured
		assert record.frame_ready
		assert record.pending_toplevel_width == 77
		assert record.pending_toplevel_height == 55
		assert record.pending_egl_resize
		assert record.pending_service_state_valid
		assert record.observed_service_state_valid
		assert record.toplevel_decoration_configured
		assert record.toplevel_decoration_mode == wayland_xdg_toplevel_decoration_mode_server_side
		assert record.render_target_generation == 9
		assert !record.hide_barrier_active
	}
}

fn new_wayland_show_handshake_test_backend(observations []WaylandShowHandshakeTestObservation, requested_maximized bool, requested_fullscreen bool, baseline_maximized bool, baseline_fullscreen bool) (&WaylandBackend, WindowId) {
	mut backend := &WaylandBackend{
		display:              voidptr(usize(0xd1))
		toplevel_replay_test: WaylandToplevelReplayTestSeam{
			active:                  true
			show_handshake_override: true
			show_observations:       observations.clone()
		}
	}
	window := WindowId{
		app_instance: 1
		slot:         0
		generation:   1
	}
	backend.windows << &WaylandWindowRecord{
		id:                           window
		owner:                        backend
		title:                        'handshake'
		app_id:                       'org.vlang.handshake'
		surface:                      voidptr(usize(0xd2))
		xdg_surface:                  voidptr(usize(0xd3))
		xdg_toplevel:                 voidptr(usize(0xd4))
		wl_egl_window:                voidptr(usize(0xd5))
		width:                        360
		height:                       260
		min_width:                    1
		min_height:                   1
		requested_maximized:          requested_maximized
		requested_fullscreen:         requested_fullscreen
		observed_service_state_valid: true
		observed_maximized:           baseline_maximized
		observed_fullscreen:          baseline_fullscreen
		render_target_generation:     7
	}
	return backend, window
}

fn assert_wayland_show_failure_left_hidden(backend &WaylandBackend, window WindowId) {
	index := backend.window_record_index(window) or {
		assert false, 'Wayland handshake test record disappeared'
		return
	}
	record := backend.windows[index]
	assert !record.requested_visible
	assert !record.publish_show_on_map
	assert !record.mapped
	assert !record.configured
	assert !record.frame_ready
	assert !record.show_handshake_active
	assert record.show_configure_serial == 0
	assert record.pending_events.len == 0
}

fn test_wayland_show_handshake_defers_ack_until_final_intents_and_uses_latest_configure() {
	$if linux && sokol_wayland ? {
		mut backend, window := new_wayland_show_handshake_test_backend([
			WaylandShowHandshakeTestObservation{
				present:   true
				serial:    11
				width:     1024
				height:    568
				maximized: true
			},
			WaylandShowHandshakeTestObservation{
				present: true
				serial:  12
				width:   480
				height:  320
			},
		], false, false, false, false)

		state := backend.service_show_window(window)!
		assert state.mapping == .unmapped
		record := backend.windows[0]
		assert record.requested_visible
		assert record.publish_show_on_map
		assert record.configured
		assert record.frame_ready
		assert !record.mapped
		assert record.width == 480
		assert record.height == 320
		assert record.pending_egl_resize
		assert record.render_target_generation == 8
		assert record.pending_events.len == 0
		operations := backend.toplevel_replay_test.operations
		assert operations.count(it == 'commit') == 1
		assert operations.count(it == 'configure') == 2
		assert operations.count(it == 'ack') == 1
		assert operations.count(it == 'ack_cleanup') == 0
		assert operations.index('configure') < operations.index('final_unmaximize')
		assert operations.index('final_unfullscreen') < operations.index('ack')
		assert operations.last() == 'ack_flush'
		assert backend.toplevel_replay_test.show_acked_serials == [u32(12)]
		assert backend.toplevel_replay_test.show_flush_count == 2
		assert backend.toplevel_replay_test.show_roundtrip_count == 2
	}
}

fn test_wayland_show_handshake_accepts_c1_after_final_sync_without_c2() {
	$if linux && sokol_wayland ? {
		mut backend, window := new_wayland_show_handshake_test_backend([
			WaylandShowHandshakeTestObservation{
				present: true
				serial:  21
				width:   360
				height:  260
			},
		], false, false, false, false)
		backend.windows[0].pending_egl_resize = true

		backend.service_show_window(window)!
		operations := backend.toplevel_replay_test.operations
		assert operations.count(it == 'commit') == 1
		assert operations.count(it == 'configure') == 1
		assert operations.index('configure') < operations.index('final_unmaximize')
		assert operations.index('final_unfullscreen') < operations.index('ack')
		assert backend.toplevel_replay_test.show_acked_serials == [u32(21)]
		assert backend.windows[0].pending_events.len == 0
		assert backend.windows[0].pending_egl_resize
	}
}

fn test_wayland_show_handshake_accepts_and_acks_wrapped_serial_zero() {
	$if linux && sokol_wayland ? {
		mut backend, window := new_wayland_show_handshake_test_backend([
			WaylandShowHandshakeTestObservation{
				present:    true
				serial_set: true
				serial:     0
			},
		], false, false, false, false)

		backend.service_show_window(window)!
		assert backend.toplevel_replay_test.show_acked_serials == [u32(0)]
		assert backend.toplevel_replay_test.operations.count(it == 'ack') == 1
		assert !backend.windows[0].show_configure_received
		assert backend.windows[0].requested_visible
		assert backend.windows[0].configured
	}
}

fn test_wayland_show_handshake_uses_bounded_second_probe_and_fullscreen_axis() {
	$if linux && sokol_wayland ? {
		mut fallback_backend, fallback_window := new_wayland_show_handshake_test_backend([
			WaylandShowHandshakeTestObservation{},
			WaylandShowHandshakeTestObservation{
				present:    true
				serial:     31
				fullscreen: true
			},
		], false, false, false, false)
		fallback_backend.service_show_window(fallback_window)!
		fallback_operations := fallback_backend.toplevel_replay_test.operations
		assert fallback_operations.count(it == 'commit') == 1
		assert fallback_operations.count(it == 'probe_maximize') == 1
		assert fallback_operations.count(it == 'probe_fullscreen') == 1
		assert fallback_operations.count(it == 'flush') == 3
		assert fallback_operations.count(it == 'roundtrip') == 3
		assert fallback_backend.toplevel_replay_test.show_acked_serials == [u32(31)]

		mut fullscreen_backend, fullscreen_window := new_wayland_show_handshake_test_backend([
			WaylandShowHandshakeTestObservation{
				present: true
				serial:  41
			},
		], false, true, false, true)
		fullscreen_backend.service_show_window(fullscreen_window)!
		fullscreen_operations := fullscreen_backend.toplevel_replay_test.operations
		assert fullscreen_operations.count(it == 'probe_unfullscreen') == 1
		assert fullscreen_operations.count(it == 'probe_maximize') == 0
		assert fullscreen_operations.count(it == 'commit') == 1
		assert fullscreen_backend.toplevel_replay_test.show_acked_serials == [u32(41)]
	}
}

fn test_wayland_show_handshake_zero_c1_and_transport_failures_are_hidden_and_retryable() {
	$if linux && sokol_wayland ? {
		mut missing_backend, missing_window := new_wayland_show_handshake_test_backend([], false,
			false, false, false)
		missing_backend.windows[0].pending_egl_resize = true
		mut missing_error := ''
		missing_backend.service_show_window(missing_window) or { missing_error = err.msg() }
		assert missing_error == err_wayland_show_configure_failed
		assert missing_backend.toplevel_replay_test.operations.count(it == 'commit') == 1
		assert missing_backend.toplevel_replay_test.show_roundtrip_count == 2
		assert_wayland_show_failure_left_hidden(missing_backend, missing_window)
		assert missing_backend.windows[0].pending_egl_resize
		missing_backend.toplevel_replay_test.operations.clear()
		missing_backend.toplevel_replay_test.show_flush_count = 0
		missing_backend.toplevel_replay_test.show_roundtrip_count = 0
		missing_backend.toplevel_replay_test.show_observations = [
			WaylandShowHandshakeTestObservation{
				present: true
				serial:  51
			},
		]
		missing_backend.service_show_window(missing_window)!
		assert missing_backend.toplevel_replay_test.show_acked_serials == [u32(51)]
		assert missing_backend.windows[0].pending_egl_resize

		mut drain_backend, drain_window := new_wayland_show_handshake_test_backend([], false,
			false, false, false)
		drain_backend.toplevel_replay_test.show_hidden_drain_failure = true
		mut drain_error := ''
		drain_backend.service_show_window(drain_window) or { drain_error = err.msg() }
		assert drain_error == err_wayland_dispatch_failed
		assert drain_backend.toplevel_replay_test.operations == ['hidden_drain']
		assert drain_backend.toplevel_replay_test.show_acked_serials.len == 0
		assert_wayland_show_failure_left_hidden(drain_backend, drain_window)

		mut flush_backend, flush_window := new_wayland_show_handshake_test_backend([], false,
			false, false, false)
		flush_backend.toplevel_replay_test.show_flush_failure_boundary = 1
		mut flush_error := ''
		flush_backend.service_show_window(flush_window) or { flush_error = err.msg() }
		assert flush_error == err_wayland_flush_failed
		assert_wayland_show_failure_left_hidden(flush_backend, flush_window)

		mut roundtrip_backend, roundtrip_window := new_wayland_show_handshake_test_backend([],
			false, false, false, false)
		roundtrip_backend.toplevel_replay_test.show_roundtrip_failure_boundary = 1
		mut roundtrip_error := ''
		roundtrip_backend.service_show_window(roundtrip_window) or { roundtrip_error = err.msg() }
		assert roundtrip_error == err_wayland_dispatch_failed
		assert_wayland_show_failure_left_hidden(roundtrip_backend, roundtrip_window)

		mut final_backend, final_window := new_wayland_show_handshake_test_backend([
			WaylandShowHandshakeTestObservation{
				present: true
				serial:  61
			},
		], false, false, false, false)
		final_backend.toplevel_replay_test.show_flush_failure_boundary = 2
		mut final_error := ''
		final_backend.service_show_window(final_window) or { final_error = err.msg() }
		assert final_error == err_wayland_flush_failed
		assert final_backend.toplevel_replay_test.operations.count(it == 'ack_cleanup') == 1
		assert final_backend.toplevel_replay_test.show_acked_serials == [u32(61)]
		assert_wayland_show_failure_left_hidden(final_backend, final_window)

		mut final_roundtrip_backend, final_roundtrip_window := new_wayland_show_handshake_test_backend([
			WaylandShowHandshakeTestObservation{
				present: true
				serial:  62
			},
		], false, false, false, false)
		final_roundtrip_backend.toplevel_replay_test.show_roundtrip_failure_boundary = 2
		mut final_roundtrip_error := ''
		final_roundtrip_backend.service_show_window(final_roundtrip_window) or {
			final_roundtrip_error = err.msg()
		}
		assert final_roundtrip_error == err_wayland_dispatch_failed
		assert final_roundtrip_backend.toplevel_replay_test.operations.count(it == 'ack_cleanup') == 1
		assert final_roundtrip_backend.toplevel_replay_test.show_acked_serials == [
			u32(62),
		]
		assert_wayland_show_failure_left_hidden(final_roundtrip_backend, final_roundtrip_window)

		mut ack_backend, ack_window := new_wayland_show_handshake_test_backend([
			WaylandShowHandshakeTestObservation{
				present: true
				serial:  71
			},
		], false, false, false, false)
		ack_backend.toplevel_replay_test.show_ack_flush_failure = true
		mut ack_error := ''
		ack_backend.service_show_window(ack_window) or { ack_error = err.msg() }
		assert ack_error == err_wayland_flush_failed
		assert ack_backend.toplevel_replay_test.operations.count(it == 'ack') == 1
		assert ack_backend.toplevel_replay_test.operations.count(it == 'ack_cleanup') == 0
		assert ack_backend.toplevel_replay_test.show_acked_serials == [u32(71)]
		assert_wayland_show_failure_left_hidden(ack_backend, ack_window)
	}
}

fn test_wayland_show_handshake_callback_keeps_latest_configure_without_ack_or_events() {
	$if linux && sokol_wayland ? {
		mut backend, _ := new_wayland_show_handshake_test_backend([], false, false, false, false)
		mut record := backend.windows[0]
		record.show_handshake_active = true
		record.show_handshake_boundary = 3
		record.pending_toplevel_width = 640
		record.pending_toplevel_height = 360
		record.pending_service_state_valid = true
		record.pending_maximized = true
		wayland_xdg_surface_configure(record.listener_data(), record.xdg_surface, 81)
		record.pending_toplevel_width = 800
		record.pending_toplevel_height = 450
		record.pending_service_state_valid = true
		record.pending_maximized = false
		record.pending_fullscreen = true
		wayland_xdg_surface_configure(record.listener_data(), record.xdg_surface, 82)
		// A legal xdg_surface.configure without another latched toplevel state
		// advances the ACK serial but must preserve C1's latest valid snapshot.
		wayland_xdg_surface_configure(record.listener_data(), record.xdg_surface, 83)
		assert record.show_configure_serial == 83
		assert record.show_configure_width == 800
		assert record.show_configure_height == 450
		assert !record.show_configure_maximized
		assert record.show_configure_fullscreen
		assert backend.toplevel_replay_test.show_acked_serials.len == 0
		assert record.pending_events.len == 0
		assert !record.configured

		backend.ack_window_show_configure(0, false)
		assert backend.toplevel_replay_test.show_acked_serials == [u32(83)]
		backend.toplevel_replay_test.operations.clear()
		backend.toplevel_replay_test.show_acked_serials.clear()
		record.pending_service_state_valid = true
		wayland_xdg_surface_configure(record.listener_data(), record.xdg_surface, 0)
		assert record.show_configure_received
		assert record.show_configure_serial == 0
		backend.ack_window_show_configure(0, false)
		backend.ack_window_show_configure(0, false)
		assert backend.toplevel_replay_test.show_acked_serials == [u32(0)]
		assert backend.toplevel_replay_test.operations.count(it == 'ack') == 1
		assert !record.show_configure_received
	}
}

fn test_wayland_hide_releases_only_egl_surface_and_invalidates_old_generation_once() {
	$if linux && sokol_wayland ? {
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut backend := &WaylandBackend{
			toplevel_replay_test: WaylandToplevelReplayTestSeam{
				hide_render_target_override: true
			}
		}
		mut record := &WaylandWindowRecord{
			id:                       window
			owner:                    backend
			width:                    80
			height:                   60
			frame_ready:              true
			pending_egl_resize:       true
			egl_surface:              voidptr(usize(0xe1))
			egl_surface_ticket:       91
			wl_egl_window:            voidptr(usize(0xe2))
			wl_egl_window_ticket:     92
			render_target_generation: 9
		}
		backend.windows << record
		backend.release_window_render_target_for_hide(0)!
		assert backend.toplevel_replay_test.operations == ['anchor', 'release_egl_surface']
		assert record.egl_surface == unsafe { nil }
		assert record.egl_surface_ticket == 0
		assert record.wl_egl_window == voidptr(usize(0xe2))
		assert record.wl_egl_window_ticket == 92
		assert record.render_target_generation == 10
		assert record.pending_egl_resize

		$if gg_multiwindow ? || x_multiwindow_render ? {
			stale := RenderFrame{
				window_id: window
				metrics:   RenderMetricsSnapshot{
					framebuffer_width:  80
					framebuffer_height: 60
				}
				target:    RenderTargetSnapshot{
					target_identity: 9
				}
			}
			attempt := backend.activate_render_frame(stale)
			assert !attempt.outcome.succeeded()
			assert attempt.outcome.disposition == .target_lost
			assert attempt.outcome.error_text == err_render_target_stale
		}

		mut anchor_failure, _ := new_wayland_show_handshake_test_backend([], false, false, false,
			false)
		anchor_failure.toplevel_replay_test.hide_render_target_override = true
		anchor_failure.toplevel_replay_test.hide_anchor_failure = true
		anchor_failure.windows[0].egl_surface = voidptr(usize(0xe3))
		anchor_failure.windows[0].egl_surface_ticket = 93
		mut anchor_error := ''
		anchor_failure.release_window_render_target_for_hide(0) or { anchor_error = err.msg() }
		assert anchor_error == err_render_native_renderer_unavailable
		assert anchor_failure.windows[0].egl_surface == voidptr(usize(0xe3))
		assert anchor_failure.windows[0].egl_surface_ticket == 93
		assert anchor_failure.windows[0].render_target_generation == 7

		mut release_failure, _ := new_wayland_show_handshake_test_backend([], false, false, false,
			false)
		release_failure.toplevel_replay_test.hide_render_target_override = true
		release_failure.toplevel_replay_test.hide_surface_release_failure = true
		release_failure.windows[0].egl_surface = voidptr(usize(0xe4))
		release_failure.windows[0].egl_surface_ticket = 94
		mut release_error := ''
		release_failure.release_window_render_target_for_hide(0) or { release_error = err.msg() }
		assert release_error == err_wayland_egl_surface_failed
		assert release_failure.windows[0].egl_surface == voidptr(usize(0xe4))
		assert release_failure.windows[0].egl_surface_ticket == 94
		assert release_failure.windows[0].render_target_generation == 7

		mut unclaimed_terminal, _ := new_wayland_show_handshake_test_backend([], false, false,
			false, false)
		unclaimed_terminal.toplevel_replay_test.hide_render_target_override = true
		unclaimed_terminal.toplevel_replay_test.hide_surface_unclaimed_terminal = true
		unclaimed_terminal.windows[0].egl_surface = voidptr(usize(0xe6))
		unclaimed_terminal.windows[0].egl_surface_ticket = 96
		mut unclaimed_error := ''
		unclaimed_terminal.release_window_render_target_for_hide(0) or {
			unclaimed_error = err.msg()
		}
		assert unclaimed_error == err_wayland_egl_surface_failed
		assert unclaimed_terminal.windows[0].egl_surface == voidptr(usize(0xe6))
		assert unclaimed_terminal.windows[0].egl_surface_ticket == 96
		assert unclaimed_terminal.windows[0].render_target_generation == 7

		mut terminal_failure, _ := new_wayland_show_handshake_test_backend([], false, false, false,
			false)
		terminal_failure.toplevel_replay_test.hide_render_target_override = true
		terminal_failure.toplevel_replay_test.hide_surface_terminal_failure = true
		terminal_failure.windows[0].egl_surface = voidptr(usize(0xe5))
		terminal_failure.windows[0].egl_surface_ticket = 95
		mut terminal_error := ''
		terminal_failure.release_window_render_target_for_hide(0) or { terminal_error = err.msg() }
		assert terminal_error == err_wayland_egl_surface_failed
		assert terminal_failure.windows[0].egl_surface == unsafe { nil }
		assert terminal_failure.windows[0].egl_surface_ticket == 0
		assert terminal_failure.windows[0].wl_egl_window == voidptr(usize(0xd5))
		assert terminal_failure.windows[0].render_target_generation == 8
	}
}

fn test_wayland_fallback_buffer_is_reused_only_for_the_exact_remap_extent() {
	$if linux && sokol_wayland ? {
		buffer := voidptr(usize(0xf1))
		record := &WaylandWindowRecord{
			fallback_current_buffer: buffer
			fallback_buffer_width:   360
			fallback_buffer_height:  260
		}
		assert record.fallback_buffer_for_extent(360, 260) == buffer
		assert record.fallback_buffer_for_extent(720, 520) == unsafe { nil }
		assert record.fallback_buffer_for_extent(360, 261) == unsafe { nil }
	}
}

fn test_wayland_fractional_scale_requires_both_protocol_objects() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		fractional_scale := voidptr(usize(0x41))
		mut record := &WaylandWindowRecord{
			id:               WindowId{
				app_instance: 1
				slot:         0
				generation:   1
			}
			owner:            backend
			high_dpi:         true
			width:            101
			height:           51
			fractional_scale: fractional_scale
		}
		backend.windows << record

		wayland_fractional_scale_preferred(record.listener_data(), fractional_scale, 180)

		assert record.fractional_scale_numerator == 0
		assert record.framebuffer_width() == 101
		assert record.framebuffer_height() == 51
		assert record.pending_events.len == 0
	}
}

fn test_wayland_xdg_configure_states_are_published_after_surface_configure() {
	$if linux && sokol_wayland ? {
		mut record := &WaylandWindowRecord{
			id:                WindowId{
				app_instance: 1
				slot:         0
				generation:   1
			}
			resizable:         true
			width:             64
			height:            48
			requested_visible: true
		}
		mut active_states := [u32(1), u32(4)]
		mut active_array := C.wl_array{
			size:  usize(active_states.len) * sizeof(u32)
			alloc: usize(active_states.len) * sizeof(u32)
			data:  active_states.data
		}
		wayland_xdg_toplevel_configure(record.listener_data(), unsafe { nil }, 64, 48,
			&active_array)
		assert record.pending_events.len == 0
		wayland_xdg_surface_configure(record.listener_data(), unsafe { nil }, 1)
		first :=
			record.pending_events.filter(it.event.kind == .service).map(it.event.service.operation)
		assert ServiceOperation.maximize in first
		assert ServiceOperation.focus in first
		assert ServiceOperation.restore !in first
		assert record.pending_events.filter(it.event.kind == .service).all(it.event.service.metrics.metrics_available)

		record.pending_events.clear()
		mut inactive_array := C.wl_array{}
		wayland_xdg_toplevel_configure(record.listener_data(), unsafe { nil }, 64, 48,
			&inactive_array)
		assert record.pending_events.len == 0
		wayland_xdg_surface_configure(record.listener_data(), unsafe { nil }, 2)
		second :=
			record.pending_events.filter(it.event.kind == .service).map(it.event.service.operation)
		assert ServiceOperation.restore in second
		assert ServiceOperation.focus in second
	}
}

fn test_wayland_state_operations_are_deferred_by_capability_until_observation() {
	$if linux && sokol_wayland ? {
		mut native_operations := &NativeOperationAuthority{}
		native_operations.bind_app_lifetime(1, 1)!
		native_operations.advance_renderer_attempt(1, 2)!
		mut backend := &WaylandBackend{
			started:                  true
			display:                  voidptr(usize(1))
			native_operations:        native_operations
			pointer:                  voidptr(usize(2))
			relative_pointer_manager: voidptr(usize(3))
			pointer_constraints:      voidptr(usize(4))
		}
		for operation in [ServiceOperation.show, .minimize, .maximize, .restore, .fullscreen,
			.mouse_lock] {
			assert backend.service_operation_capability(operation).asynchronous
		}
		assert !backend.service_operation_capability(.hide).asynchronous
		state_operations := [ServiceOperation.minimize, .maximize, .fullscreen, .restore, .mouse_lock]
		expected_live_support := [ServiceSupportLevel.available, .available, .available, .conditional,
			.conditional]
		expected_dead_support := [ServiceSupportLevel.unsupported, .unsupported, .unsupported,
			.unsupported, .unsupported]
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_live_support

		backend.started = false
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_dead_support
		backend.started = true
		backend.display = unsafe { nil }
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_dead_support
		backend.display = voidptr(usize(1))
		backend.wayland_display_unavailable = true
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_dead_support
		backend.wayland_display_unavailable = false
		backend.wayland_display_error = 1
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_dead_support
		backend.wayland_display_error = 0
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_live_support
		backend.render_health = .unavailable
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_dead_support
		assert backend.service_operation_capability(.show).support == .unsupported
		assert backend.service_operation_capability(.hide).support == .unsupported
		backend.render_health = .ready
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_live_support
		native_operations.next_ordinal = ~u64(0)
		assert state_operations.map(backend.service_operation_capability(it).support) == expected_dead_support
		native_operations.next_ordinal = 1

		mut app := new_app()!
		window := app.create_window(title: 'wayland-deferred-publication')!
		_ = app.drain_queued_events()!
		before := app.service_window_state(window)!
		app.backend.kind = .wayland
		app.publish_native_state(window, .maximize, ServiceWindowState{
			maximized: .on
		})!
		assert app.service_window_state(window)!.maximized == before.maximized
		assert app.drain_queued_events()!.len == 0

		acceptance := app.accept_backend_event_batch([
			queued_service_event(ServiceEvent{
				kind:      .state
				window:    window
				operation: .maximize
				state:     ServiceWindowState{
					maximized: .on
				}
			}),
		], 1)!
		assert acceptance.accepted == 1
		assert app.service_window_state(window)!.maximized == .on
		events := app.drain_queued_events()!
		assert events.filter(it.kind == .service && it.service.operation == .maximize).len == 1
		app.backend.kind = .mock
		app.stop()!
	}
}

fn test_wayland_existing_mouse_lock_can_be_released_after_capability_loss() {
	$if linux && sokol_wayland ? {
		mut app := new_app()!
		window := app.create_window(title: 'wayland dead-transport mouse unlock')!
		_ = app.drain_queued_events()!
		original_backend := app.backend.kind
		app.backend.kind = .wayland
		app.backend.wayland = WaylandBackend{
			started:                     true
			display:                     voidptr(usize(1))
			wayland_display_unavailable: true
			relative_pointer_manager:    voidptr(usize(2))
			pointer_constraints:         voidptr(usize(3))
		}
		mut record := &WaylandWindowRecord{
			id:    window
			owner: &app.backend.wayland
		}
		app.backend.wayland.windows << record
		assert app.service_operation_capability(window, .mouse_lock)!.support == .unsupported
		mut enable_error := ''
		app.service_set_mouse_lock(window, true) or { enable_error = err.msg() }
		assert enable_error == err_capability_unsupported
		assert !record.mouse_lock_requested
		assert !record.mouse_locked
		assert record.locked_pointer == unsafe { nil }
		assert record.relative_pointer == unsafe { nil }
		assert record.pending_events.len == 0

		record.mouse_lock_requested = true
		record.mouse_locked = true
		app.service_set_mouse_lock(window, false)!
		assert !record.mouse_lock_requested
		assert !record.mouse_locked
		state_events := record.pending_events.filter(it.event.kind == .service
			&& it.event.service.kind == .state && it.event.service.operation == .mouse_lock)
		assert state_events.len == 1
		assert state_events[0].event.service.state.mouse_locked == .off

		mut duplicate_error := ''
		app.service_set_mouse_lock(window, false) or { duplicate_error = err.msg() }
		assert duplicate_error == err_capability_unsupported
		assert record.pending_events.filter(it.event.kind == .service
			&& it.event.service.kind == .state && it.event.service.operation == .mouse_lock).len == 1

		app.backend.wayland.windows.clear()
		app.backend.kind = original_backend
		app.stop()!
	}
}

fn test_wayland_synchronous_clipboard_write_reserves_terminal_before_native_mutation() {
	$if linux && sokol_wayland ? {
		mut app := new_app()!
		window := app.create_window(title: 'Wayland clipboard delivery preflight')!
		_ = app.drain_queued_events()!
		original_backend := app.backend.kind
		app.backend.kind = .wayland
		app.backend.wayland.started = true
		app.backend.wayland.display = voidptr(usize(1))
		app.backend.wayland.data_device_manager = voidptr(usize(2))
		app.backend.wayland.data_device = voidptr(usize(3))
		app.backend.wayland.seat = voidptr(usize(4))
		request_before := app.services.next_request
		pending_before := app.services.pending.len
		source_before := app.backend.wayland.clipboard_source
		text_before := app.backend.wayland.clipboard_text

		app.state_mutex.lock()
		saved_delivery_token := app.next_event_delivery_token
		app.next_event_delivery_token = 0
		app.state_mutex.unlock()
		mut rejected := false
		app.service_set_clipboard_text(window, 'must-not-commit') or {
			assert err.msg() == err_event_delivery_exhausted
			rejected = true
			ServiceRequestId{}
		}
		assert rejected
		assert app.services.next_request == request_before
		assert app.services.pending.len == pending_before
		assert app.backend.wayland.clipboard_source == source_before
		assert app.backend.wayland.clipboard_text == text_before
		assert app.drain_queued_events()!.len == 0

		app.state_mutex.lock()
		app.next_event_delivery_token = saved_delivery_token
		app.state_mutex.unlock()
		app.backend.kind = original_backend
		app.stop()!
	}
}

fn test_wayland_relative_pointer_callbacks_publish_observed_state_and_motion() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		mut record := &WaylandWindowRecord{
			id:     WindowId{
				app_instance: 1
				slot:         0
				generation:   1
			}
			owner:  backend
			width:  64
			height: 48
		}
		locked_pointer := voidptr(usize(0x21))
		relative_pointer := voidptr(usize(0x22))
		record.locked_pointer = locked_pointer
		record.relative_pointer = relative_pointer
		record.mouse_lock_requested = true

		wayland_locked_pointer_locked(record.listener_data(), locked_pointer)
		assert record.mouse_locked
		assert record.pending_events.len == 1
		assert record.pending_events[0].event.kind == .service
		assert record.pending_events[0].event.service.operation == .mouse_lock
		assert record.pending_events[0].event.service.state.mouse_locked == .on

		record.pending_events.clear()
		wayland_relative_pointer_motion(record.listener_data(), relative_pointer, 0, 1, 3.25, -2.5,
			3.0, -2.0)
		assert record.pending_events.len == 1
		assert record.pending_events[0].event.kind == .input
		assert record.pending_events[0].event.input.kind == .mouse_move
		assert record.pending_events[0].event.input.mouse_dx == f32(3.25)
		assert record.pending_events[0].event.input.mouse_dy == f32(-2.5)

		record.pending_events.clear()
		wayland_locked_pointer_unlocked(record.listener_data(), locked_pointer)
		assert !record.mouse_locked
		assert record.pending_events.len == 1
		assert record.pending_events[0].event.kind == .service
		assert record.pending_events[0].event.service.operation == .mouse_lock
		assert record.pending_events[0].event.service.state.mouse_locked == .off
	}
}

fn test_wayland_seat_loss_releases_active_mouse_lock_and_publishes_off() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(title: 'package2-wayland-seat-loss-mouse-lock')!
		capability := app.service_operation_capability(window, .mouse_lock)!
		if capability.support == .unsupported {
			eprintln('SKIP Wayland active mouse-lock loss probe: relative-pointer protocols are unavailable')
			app.stop()!
			return
		}
		app.service_set_mouse_lock(window, true)!
		index := app.backend.wayland.window_record_index(window) or {
			app.stop()!
			panic(err_window_not_found)
		}
		mut record := app.backend.wayland.windows[index]
		assert record.locked_pointer != unsafe { nil }
		assert record.relative_pointer != unsafe { nil }
		wayland_locked_pointer_locked(record.listener_data(), record.locked_pointer)
		assert record.mouse_lock_requested
		assert record.mouse_locked
		record.pending_events.clear()
		seat_name := app.backend.wayland.seat_name
		assert seat_name != 0

		wayland_registry_handle_global_remove(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, seat_name)

		assert record.locked_pointer == unsafe { nil }
		assert record.relative_pointer == unsafe { nil }
		assert !record.mouse_lock_requested
		assert !record.mouse_locked
		state_events := record.pending_events.filter(it.event.kind == .service
			&& it.event.service.kind == .state && it.event.service.operation == .mouse_lock)
		assert state_events.len == 1
		assert state_events[0].event.service.state.mouse_locked == .off
		capability_events := wayland_capability_events_for(record, .mouse_lock)
		assert capability_events.len == 1
		assert capability_events[0].event.service.capability.support == .unsupported
		before_duplicate_remove := record.pending_events.len
		wayland_registry_handle_global_remove(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, seat_name)
		assert record.pending_events.len == before_duplicate_remove
		app.stop()!
	}
}

fn wayland_capability_events_for(record &WaylandWindowRecord, operation ServiceOperation) []WaylandNativeQueuedEvent {
	return record.pending_events.filter(it.event.kind == .service
		&& it.event.service.kind == .capability && it.event.service.operation == operation)
}

fn test_wayland_data_device_manager_remove_and_readd_publish_idempotent_capabilities() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(title: 'package2-wayland-data-manager-capabilities')!
		index := app.backend.wayland.window_record_index(window) or {
			app.stop()!
			panic(err_window_not_found)
		}
		mut record := app.backend.wayland.windows[index]
		name := app.backend.wayland.data_device_manager_name
		assert name != 0
		assert app.backend.wayland.data_device_manager != unsafe { nil }
		assert app.backend.wayland.data_device != unsafe { nil }
		record.pending_events.clear()

		wayland_registry_handle_global_remove(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name)
		assert wayland_capability_events_for(record, .clipboard_read).len == 1
		assert wayland_capability_events_for(record, .clipboard_write).len == 1
		assert wayland_capability_events_for(record, .clipboard_read)[0].event.service.capability.support == .unsupported
		before_duplicate_remove := record.pending_events.len
		wayland_registry_handle_global_remove(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name)
		assert record.pending_events.len == before_duplicate_remove

		record.pending_events.clear()
		wayland_registry_handle_global(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name, c'wl_data_device_manager', 3)
		assert app.backend.wayland.data_device_manager != unsafe { nil }
		assert app.backend.wayland.data_device != unsafe { nil }
		assert wayland_capability_events_for(record, .clipboard_read).len == 1
		assert wayland_capability_events_for(record, .clipboard_write).len == 1
		assert wayland_capability_events_for(record, .clipboard_read)[0].event.service.capability.support == .conditional
		before_duplicate_add := record.pending_events.len
		wayland_registry_handle_global(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name, c'wl_data_device_manager', 3)
		assert record.pending_events.len == before_duplicate_add
		app.stop()!
	}
}

fn test_wayland_relative_pointer_manager_remove_and_readd_publish_idempotent_capability() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(title: 'package2-wayland-pointer-manager-capability')!
		index := app.backend.wayland.window_record_index(window) or {
			app.stop()!
			panic(err_window_not_found)
		}
		mut record := app.backend.wayland.windows[index]
		name := app.backend.wayland.relative_pointer_manager_name
		if name == 0 || app.backend.wayland.relative_pointer_manager == unsafe { nil } {
			eprintln('SKIP Wayland relative-pointer capability transition: protocol is unavailable')
			app.stop()!
			return
		}
		record.pending_events.clear()

		wayland_registry_handle_global_remove(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name)
		assert wayland_capability_events_for(record, .mouse_lock).len == 1
		assert wayland_capability_events_for(record, .mouse_lock)[0].event.service.capability.support == .unsupported
		before_duplicate_remove := record.pending_events.len
		wayland_registry_handle_global_remove(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name)
		assert record.pending_events.len == before_duplicate_remove

		record.pending_events.clear()
		wayland_registry_handle_global(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name, c'zwp_relative_pointer_manager_v1', 1)
		assert wayland_capability_events_for(record, .mouse_lock).len == 1
		assert wayland_capability_events_for(record, .mouse_lock)[0].event.service.capability.support == .conditional
		before_duplicate_add := record.pending_events.len
		wayland_registry_handle_global(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name, c'zwp_relative_pointer_manager_v1', 1)
		assert record.pending_events.len == before_duplicate_add
		app.stop()!
	}
}

fn test_wayland_exporter_remove_and_readd_publish_idempotent_capability() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(title: 'package2-wayland-exporter-capability')!
		index := app.backend.wayland.window_record_index(window) or {
			app.stop()!
			panic(err_window_not_found)
		}
		mut record := app.backend.wayland.windows[index]
		name := app.backend.wayland.foreign_exporter_name
		if name == 0 || app.backend.wayland.foreign_exporter == unsafe { nil } {
			eprintln('SKIP Wayland exporter capability transition: xdg-foreign v2 is unavailable')
			app.stop()!
			return
		}
		record.pending_events.clear()

		wayland_registry_handle_global_remove(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name)
		assert wayland_capability_events_for(record, .portal_parent).len == 1
		assert wayland_capability_events_for(record, .portal_parent)[0].event.service.capability.support == .unsupported
		before_duplicate_remove := record.pending_events.len
		wayland_registry_handle_global_remove(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name)
		assert record.pending_events.len == before_duplicate_remove

		record.pending_events.clear()
		wayland_registry_handle_global(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name, c'zxdg_exporter_v2', 1)
		assert wayland_capability_events_for(record, .portal_parent).len == 1
		assert wayland_capability_events_for(record, .portal_parent)[0].event.service.capability.support == .available
		before_duplicate_add := record.pending_events.len
		wayland_registry_handle_global(unsafe { voidptr(&app.backend.wayland) }, unsafe {
			&C.wl_registry(app.backend.wayland.registry)
		}, name, c'zxdg_exporter_v2', 1)
		assert record.pending_events.len == before_duplicate_add
		app.stop()!
	}
}

fn test_wayland_xdg_foreign_callback_retains_and_releases_exact_lease() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		window := WindowId{
			app_instance: 7
			slot:         2
			generation:   3
		}
		request := ServiceRequestId{
			app_instance: 7
			serial:       11
		}
		lease := ServicePortalLeaseId{
			app_instance: 7
			serial:       11
		}
		exported := voidptr(usize(0x41))
		mut portal := &WaylandPortalExport{
			request:  request
			window:   window
			lease:    lease
			owner:    backend
			exported: exported
		}
		backend.portal_exports << portal

		wayland_exported_handle(portal.listener_data(), exported, c'portal-handle')
		assert portal.terminal
		assert portal.identifier == 'wayland:portal-handle'
		assert backend.pending_service_events.len == 1
		event := backend.pending_service_events[0].event.service
		assert event.kind == .portal_parent
		assert event.portal_parent.id == request
		assert event.portal_parent.lease == lease
		assert event.portal_parent.identifier == 'wayland:portal-handle'

		// The deterministic probe has no live Wayland proxy. The release path
		// still proves exact lease lookup/removal and stale replay rejection.
		portal.exported = unsafe { nil }
		backend.service_release_portal_parent(lease)!
		assert backend.portal_exports.len == 0
		if _ := backend.service_release_portal_parent(lease) {
			assert false
		} else {
			assert err.msg() == err_service_request_stale
		}
	}
}

fn test_wayland_xdg_foreign_runtime_capability_and_public_completion() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(
			title:  'package2-wayland-portal-parent'
			width:  48
			height: 32
		)!
		capability := app.service_operation_capability(window, .portal_parent)!
		if capability.support == .unsupported {
			before_pending := app.services.pending.len
			app.service_request_portal_parent(window) or {
				assert err.msg() == err_capability_unsupported
				assert app.services.pending.len == before_pending
				app.stop()!
				return
			}
			assert false
		}
		assert capability.support == .available
		assert capability.asynchronous
		request := app.service_request_portal_parent(window)!
		mut result := ServicePortalParentResult{}
		mut completed := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			for queued in app.drain_queued_events()! {
				if queued.kind == .service && queued.service.kind == .portal_parent
					&& queued.service.portal_parent.id == request {
					result = queued.service.portal_parent
					completed = true
				}
			}
			if completed {
				break
			}
			time.sleep(time.millisecond)
		}
		assert completed
		assert result.status == .ready
		assert result.identifier.starts_with('wayland:')
		app.service_release_portal_parent(result.lease)!
		app.service_release_portal_parent(result.lease) or {
			assert err.msg() == err_service_request_stale
			app.stop()!
			return
		}
		assert false
	}
}

fn test_wayland_service_capabilities_follow_runtime_authority() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut plain := new_app(backend: .wayland, require_renderer: false)!
		plain_window := plain.create_window(
			title:  'package2-wayland-capabilities-plain'
			width:  48
			height: 32
		)!
		mouse_lock := plain.service_operation_capability(plain_window, .mouse_lock)!
		if plain.backend.wayland.pointer != unsafe { nil }
			&& plain.backend.wayland.relative_pointer_manager != unsafe { nil }
			&& plain.backend.wayland.pointer_constraints != unsafe { nil } {
			assert mouse_lock.support == .conditional
			assert mouse_lock.state_observable
		} else {
			assert mouse_lock.support == .unsupported
		}
		assert plain.service_operation_capability(plain_window, .image_readback)!.support == .unsupported
		assert plain.service_operation_capability(plain_window, .window_capture)!.support == .unsupported
		if plain.backend.wayland.seat != unsafe { nil }
			&& plain.backend.wayland.data_device != unsafe { nil } {
			assert plain.service_operation_capability(plain_window, .clipboard_read)!.support == .conditional
			assert plain.service_operation_capability(plain_window, .clipboard_write)!.support == .conditional
			mut serial_required := false
			plain.service_set_clipboard_text(plain_window, 'serial-required') or {
				assert err.msg() == err_capability_unsupported
				serial_required = true
			}
			assert serial_required
			assert plain.services.pending.len == 0
		}
		plain.stop()!

		mut rendered := new_app(backend: .wayland, require_renderer: true)!
		rendered_window := rendered.create_window(
			title:  'package2-wayland-capabilities-rendered'
			width:  48
			height: 32
		)!
		assert rendered.service_operation_capability(rendered_window, .image_readback)!.support == .available
		assert rendered.service_operation_capability(rendered_window, .window_capture)!.support == .unsupported
		before_readbacks := rendered.services.readbacks.len
		rendered.service_request_window_readback(rendered_window, 4, 4, 1) or {
			assert err.msg() == err_capability_unsupported
			assert rendered.services.readbacks.len == before_readbacks
			rendered.stop()!
			return
		}
		assert false, 'Wayland core advertised compositor-owned window capture'
		rendered.stop()!
	}
}

fn test_wayland_surface_output_membership_updates_public_metrics() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(
			title:    'package2-wayland-output-membership'
			width:    48
			height:   32
			high_dpi: true
		)!
		index := app.backend.wayland.window_record_index(window) or {
			app.stop()!
			panic(err_window_not_found)
		}
		if app.backend.wayland.fractional_scale_manager_name != 0
			&& app.backend.wayland.viewporter_name != 0 {
			assert app.backend.wayland.windows[index].fractional_scale != unsafe { nil }
			assert app.backend.wayland.windows[index].viewport != unsafe { nil }
			mut saw_fractional_preference := false
			for _ in 0 .. 100 {
				_ = app.poll_events()!
				if app.backend.wayland.windows[index].fractional_scale_numerator > 0 {
					saw_fractional_preference = true
					break
				}
				time.sleep(time.millisecond)
			}
			assert saw_fractional_preference
			metrics := app.backend.wayland.windows[index].service_metrics_snapshot()
			assert metrics.dpi_scale > 0
			assert metrics.framebuffer_width > 0
			assert metrics.framebuffer_height > 0
		}
		mut saw_metrics := false
		for _ in 0 .. 100 {
			_ = app.poll_events()!
			events := app.drain_queued_events()!
			if events.any(it.kind == .service && it.service.kind == .metrics
				&& it.service.window == window && it.service.metrics.metrics_available)
			{
				saw_metrics = true
				break
			}
			time.sleep(time.millisecond)
		}
		monitors := app.service_monitor_ids()!
		if monitors.len > 0 {
			assert saw_metrics
			state := app.service_window_state(window)!
			assert state.monitor_ids.len > 0
			monitor := app.service_monitor_info(state.monitor_ids[0])!
			assert monitor.available
			assert !monitor.geometry.known
			assert monitor.scale.known
			assert monitor.scale.value >= 1
		}
		app.stop()!
	}
}

fn test_wayland_hidden_window_remaps_through_fresh_xdg_configure_cycle() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(
			title:   'package2-wayland-show-hide'
			width:   48
			height:  32
			visible: false
		)!
		_ = app.poll_events()!
		_ = app.drain_queued_events()!
		assert app.service_operation_capability(window, .show)!.support == .available
		assert app.service_operation_capability(window, .hide)!.support == .available
		assert app.service_window_state(window)!.mapping == .unmapped

		app.service_show_window(window)!
		assert app.service_window_state(window)!.mapping == .unmapped
		first_index := app.backend.wayland.window_record_index(window) or {
			assert false, 'Wayland initially hidden record disappeared'
			0
		}
		assert app.backend.wayland.windows[first_index].configured
		_ = app.poll_events()!
		first_show := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.window == window && it.service.kind == .state
			&& it.service.operation == .show)
		assert first_show.len == 1
		assert first_show[0].service.state.mapping == .mapped
		assert app.service_window_state(window)!.mapping == .mapped
		_ = app.poll_events()!
		assert app.drain_queued_events()!.filter(it.kind == .service && it.service.window == window
			&& it.service.kind == .state && it.service.operation == .show).len == 0
		app.service_hide_window(window)!
		assert app.service_window_state(window)!.mapping == .unmapped
		index_after_hide := app.backend.wayland.window_record_index(window) or {
			assert false, 'Wayland hidden window record disappeared'
			0
		}
		assert !app.backend.wayland.windows[index_after_hide].configured
		assert !app.backend.wayland.windows[index_after_hide].pending_service_state_valid
		hide_events := app.drain_queued_events()!
		assert hide_events.filter(it.kind == .service && it.service.window == window
			&& it.service.kind == .state && it.service.operation == .show).len == 0
		app.service_show_window(window)!
		assert app.service_window_state(window)!.mapping == .unmapped
		index := app.backend.wayland.window_record_index(window) or {
			assert false, 'Wayland remap window record disappeared'
			0
		}
		assert app.backend.wayland.windows[index].configured
		_ = app.poll_events()!
		second_show := app.drain_queued_events()!.filter(it.kind == .service
			&& it.service.window == window && it.service.kind == .state
			&& it.service.operation == .show)
		assert second_show.len == 1
		assert second_show[0].service.state.mapping == .mapped
		assert app.service_window_state(window)!.mapping == .mapped
		_ = app.poll_events()!
		assert app.drain_queued_events()!.filter(it.kind == .service && it.service.window == window
			&& it.service.kind == .state && it.service.operation == .show).len == 0
		app.stop()!
	}
}

fn test_wayland_unavailable_window_controls_are_not_advertised() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(
			title:  'package2-wayland-capability-honesty'
			width:  48
			height: 32
		)!
		for operation in [ServiceOperation.focus, .raise, .position, .titlebar_appearance] {
			assert app.service_operation_capability(window, operation)!.support == .unsupported
		}
		minimize := app.service_operation_capability(window, .minimize)!
		assert minimize.support == .available
		assert !minimize.state_observable
		assert app.service_operation_capability(window, .restore)!.support == .conditional
		app.stop()!
	}
}

fn test_wayland_clipboard_read_timeout_is_terminal_exactly_once() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:    window
			owner: backend
		}
		backend.windows << record
		mut fds := [-1, -1]!
		assert C.pipe(&fds[0]) == 0
		assert C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 1
		backend.clipboard_read = WaylandClipboardRead{
			request: ServiceRequestId{
				app_instance: 1
				serial:       7
			}
			window:  window
			fd:      fds[0]
			buffer:  []u8{}
		}
		backend.clipboard_read_active = true

		backend.drain_clipboard_read()
		C.close(fds[1])

		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		assert record.pending_events.len == 1
		result := record.pending_events[0].event.service.clipboard
		assert result.status == .failed
		assert result.error == err_clipboard_timeout
		backend.drain_clipboard_read()
		assert record.pending_events.len == 1
	}
}

fn test_wayland_fatal_dispatch_terminalizes_active_clipboard_read_exactly_once() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{
			display: voidptr(usize(0x11))
		}
		_, record, fds := wayland_test_begin_clipboard_read(mut backend, 86, []u8{})
		request := backend.clipboard_read.request
		nonfatal := NativeRenderResult{
			domain:      .wayland
			operation:   .display_dispatch
			disposition: .transient
			error_text:  'renderer-only failure'
		}
		backend.cleanup_after_fatal_dispatch(nonfatal)
		assert backend.clipboard_read_active
		assert backend.clipboard_read.fd == fds[0]
		assert record.pending_events.len == 0

		backend.wayland_display_unavailable = true
		backend.cleanup_after_fatal_dispatch(NativeRenderResult{
			domain:      .wayland
			operation:   .display_dispatch
			disposition: .ok
		})
		assert backend.clipboard_read_active
		assert record.pending_events.len == 0

		fatal := NativeRenderResult{
			domain:        .wayland
			operation:     .display_dispatch
			disposition:   .renderer_unavailable
			display_error: 32
			error_text:    'fatal display transport'
		}
		backend.cleanup_after_fatal_dispatch(fatal)
		C.close(fds[1])
		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		wayland_assert_clipboard_failure(record, request, 'fatal display transport')
		backend.cleanup_after_fatal_dispatch(fatal)
		assert record.pending_events.len == 1

		mut fallback_backend := &WaylandBackend{
			display:                     voidptr(usize(0x12))
			wayland_display_unavailable: true
		}
		_, fallback_record, fallback_fds := wayland_test_begin_clipboard_read(mut fallback_backend,
			87, []u8{})
		fallback_request := fallback_backend.clipboard_read.request
		fallback_backend.cleanup_after_fatal_dispatch(NativeRenderResult{
			domain:        .wayland
			operation:     .display_dispatch
			disposition:   .renderer_unavailable
			display_error: 32
		})
		C.close(fallback_fds[1])
		assert !fallback_backend.clipboard_read_active
		assert fallback_backend.clipboard_read.fd == -1
		wayland_assert_clipboard_failure(fallback_record, fallback_request,
			err_wayland_dispatch_failed)
		fallback_backend.cleanup_after_fatal_dispatch(fatal)
		assert fallback_record.pending_events.len == 1

		mut health_backend := &WaylandBackend{
			display:       voidptr(usize(0x13))
			render_health: .unavailable
		}
		assert health_backend.transport_can_marshal()
		_, health_record, health_fds := wayland_test_begin_clipboard_read(mut health_backend, 88,
			[]u8{})
		health_request := health_backend.clipboard_read.request
		health_failure := NativeRenderResult{
			domain:      .wayland
			operation:   .display_dispatch
			disposition: .renderer_unavailable
			error_text:  'fatal renderer health'
		}
		health_backend.cleanup_after_fatal_dispatch(health_failure)
		C.close(health_fds[1])
		assert !health_backend.clipboard_read_active
		assert health_backend.clipboard_read.fd == -1
		wayland_assert_clipboard_failure(health_record, health_request, 'fatal renderer health')
		health_backend.cleanup_after_fatal_dispatch(health_failure)
		assert health_record.pending_events.len == 1
	}
}

fn test_wayland_fatal_dispatch_cleans_nonterminal_resources_once_and_preserves_ready_portals() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{
			display:       voidptr(usize(0x21))
			render_health: .unavailable
			io_test:       WaylandIoTestSeam{
				pending_drop_native_bypass: true
			}
		}
		window, record, read_fds := wayland_test_begin_clipboard_read(mut backend, 89, []u8{})
		read_request := backend.clipboard_read.request
		pending_request := ServiceRequestId{
			app_instance: 1
			serial:       90
		}
		pending_lease := ServicePortalLeaseId{
			app_instance: 1
			serial:       90
		}
		ready_lease := ServicePortalLeaseId{
			app_instance: 1
			serial:       91
		}
		backend.portal_exports << &WaylandPortalExport{
			request: pending_request
			window:  window
			lease:   pending_lease
			owner:   backend
		}
		ready_exported := voidptr(usize(0x25))
		mut ready_portal := &WaylandPortalExport{
			request:  ServiceRequestId{
				app_instance: 1
				serial:       91
			}
			window:   window
			lease:    ready_lease
			owner:    backend
			exported: ready_exported
		}
		backend.portal_exports << ready_portal
		wayland_exported_handle(ready_portal.listener_data(), ready_exported,
			c'ready-before-failure')
		assert ready_portal.terminal
		assert backend.pending_service_events.len == 1
		assert backend.pending_service_events[0].event.service.portal_parent.status == .ready
		ready_portal.exported = unsafe { nil }
		mut send_a := [-1, -1]!
		mut send_b := [-1, -1]!
		assert C.pipe(&send_a[0]) == 0
		assert C.pipe(&send_b[0]) == 0
		backend.clipboard_sends << WaylandClipboardSend{
			fd:      send_a[1]
			payload: 'a'
		}
		backend.clipboard_sends << WaylandClipboardSend{
			fd:      send_b[1]
			payload: 'bc'
		}
		backend.clipboard_send_cursor = 1
		backend.clipboard_send_snapshot_bytes = 3
		mut drop_fds := [-1, -1]!
		assert C.pipe(&drop_fds[0]) == 0
		offer := voidptr(usize(0x22))
		backend.data_offer = offer
		backend.data_offer_has_uri_list = true
		backend.data_offer_window = window
		backend.data_offer_window_valid = true
		backend.pending_drop_offer = offer
		backend.pending_drop_fd = drop_fds[0]
		backend.pending_drop_window = window
		backend.pending_drop_window_valid = true
		backend.pending_drop_source_actions = wayland_dnd_action_copy
		backend.pending_drop_selected_action = wayland_dnd_action_copy
		backend.pending_drop_action_received = true
		backend.pending_drop_poll_cycles = 3
		backend.pending_drop_buffer = 'file:///tmp/partial'.bytes()

		fatal := NativeRenderResult{
			domain:      .wayland
			operation:   .display_dispatch
			disposition: .renderer_unavailable
			error_text:  'fatal shared dispatch'
		}
		backend.cleanup_after_fatal_dispatch(fatal)
		C.close(read_fds[1])

		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		wayland_assert_clipboard_failure(record, read_request, 'fatal shared dispatch')
		assert backend.pending_service_events.len == 2
		ready_event := backend.pending_service_events[0]
		assert ready_event.event.service.portal_parent.status == .ready
		assert ready_event.event.service.portal_parent.lease == ready_lease
		portal_failure := backend.pending_service_events[1]
		assert ready_event.sequence < portal_failure.sequence
		portal_event := portal_failure.event.service
		assert portal_event.kind == .portal_parent
		assert portal_event.operation == .portal_parent
		assert portal_event.portal_parent.id == pending_request
		assert portal_event.portal_parent.window == window
		assert portal_event.portal_parent.lease == pending_lease
		assert portal_event.portal_parent.status == .failed
		assert portal_event.portal_parent.error == 'fatal shared dispatch'
		assert backend.portal_exports.len == 1
		assert backend.portal_exports[0].lease == ready_lease
		assert backend.portal_exports[0].terminal
		assert backend.portal_exports[0].identifier == 'wayland:ready-before-failure'
		assert backend.clipboard_sends.len == 0
		assert backend.clipboard_send_cursor == 0
		assert backend.clipboard_send_snapshot_bytes == 0
		assert C.write(send_a[1], c'x', usize(1)) == -1
		assert C.write(send_b[1], c'x', usize(1)) == -1
		C.close(send_a[0])
		C.close(send_b[0])
		assert backend.data_offer == unsafe { nil }
		assert !backend.data_offer_has_uri_list
		assert !backend.data_offer_window_valid
		assert backend.pending_drop_offer == unsafe { nil }
		assert backend.pending_drop_fd == -1
		assert !backend.pending_drop_window_valid
		assert backend.pending_drop_buffer.len == 0
		assert C.close(drop_fds[0]) == -1
		C.close(drop_fds[1])
		assert record.pending_events.filter(it.event.kind == .input
			&& it.event.input.kind == .files_dropped).len == 0

		backend.cleanup_after_fatal_dispatch(fatal)
		assert record.pending_events.len == 1
		assert backend.pending_service_events.len == 2
		assert backend.pending_service_events[0].event.service.portal_parent.status == .ready
		assert backend.pending_service_events[1].event.service.portal_parent.status == .failed
		assert backend.portal_exports.len == 1
		backend.service_release_portal_parent(ready_lease)!
		assert backend.portal_exports.len == 0

		mut nonfatal_backend := &WaylandBackend{
			display: voidptr(usize(0x23))
			io_test: WaylandIoTestSeam{
				pending_drop_native_bypass: true
			}
		}
		nonfatal_window, nonfatal_record, nonfatal_read_fds := wayland_test_begin_clipboard_read(mut nonfatal_backend,
			92, []u8{})
		nonfatal_backend.portal_exports << &WaylandPortalExport{
			request: ServiceRequestId{
				app_instance: 1
				serial:       93
			}
			window:  nonfatal_window
			lease:   ServicePortalLeaseId{
				app_instance: 1
				serial:       93
			}
			owner:   nonfatal_backend
		}
		mut nonfatal_send := [-1, -1]!
		mut nonfatal_drop := [-1, -1]!
		assert C.pipe(&nonfatal_send[0]) == 0
		assert C.pipe(&nonfatal_drop[0]) == 0
		nonfatal_backend.clipboard_sends << WaylandClipboardSend{
			fd:      nonfatal_send[1]
			payload: 'retained'
		}
		nonfatal_offer := voidptr(usize(0x24))
		nonfatal_backend.data_offer = nonfatal_offer
		nonfatal_backend.pending_drop_offer = nonfatal_offer
		nonfatal_backend.pending_drop_fd = nonfatal_drop[0]
		nonfatal_backend.pending_drop_buffer = 'retained'.bytes()
		nonfatal := NativeRenderResult{
			domain:      .wayland
			operation:   .display_dispatch
			disposition: .transient
			error_text:  'retryable dispatch'
		}
		nonfatal_backend.cleanup_after_fatal_dispatch(nonfatal)
		assert nonfatal_backend.clipboard_read_active
		assert nonfatal_record.pending_events.len == 0
		assert nonfatal_backend.portal_exports.len == 1
		assert nonfatal_backend.clipboard_sends.len == 1
		assert nonfatal_backend.pending_drop_offer == nonfatal_offer
		assert nonfatal_backend.pending_drop_fd == nonfatal_drop[0]
		assert nonfatal_backend.pending_drop_buffer.bytestr() == 'retained'
		assert wayland_dispatch_failure_message(NativeRenderResult{
			domain:      .wayland
			operation:   .display_dispatch
			disposition: .renderer_unavailable
		}) == err_wayland_dispatch_failed

		nonfatal_backend.cancel_clipboard_read()
		C.close(nonfatal_read_fds[1])
		nonfatal_backend.close_all_clipboard_sends()
		C.close(nonfatal_send[0])
		nonfatal_backend.clear_data_offer(true)
		C.close(nonfatal_drop[1])
		nonfatal_backend.destroy_all_portal_exports()
	}
}

fn test_wayland_clipboard_queued_eof_wins_over_expired_deadline_exactly_once() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		_, record, fds := wayland_test_begin_clipboard_read(mut backend, 79, []u8{})
		payload := 'queued-after-deadline'
		assert C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 1
		assert C.write(fds[1], payload.str, usize(payload.len)) == payload.len
		C.close(fds[1])
		backend.clipboard_read.deadline_ns = 0
		backend.io_test.clipboard_read_interruptions = 1

		backend.drain_clipboard_read()

		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		assert record.pending_events.len == 1
		result := record.pending_events[0].event.service.clipboard
		assert result.status == .ready
		assert result.error == ''
		assert result.text == payload
		backend.drain_clipboard_read()
		assert record.pending_events.len == 1
	}
}

fn test_wayland_clipboard_chunk_quota_keeps_progress_active_past_deadline() {
	$if linux && sokol_wayland ? {
		path := os.join_path(os.temp_dir(), 'v_wayland_clipboard_quota_${os.getpid()}')
		payload := 'q'.repeat(
			wayland_clipboard_io_chunk_size * wayland_clipboard_max_io_chunks_per_poll + 1)
		os.write_file(path, payload)!
		defer {
			os.rm(path) or {}
		}
		mut file := os.open(path)!
		read_fd := os.fd_dup(file.fd)
		assert read_fd >= 0
		file.close()
		mut backend := &WaylandBackend{}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:    window
			owner: backend
		}
		backend.windows << record
		backend.clipboard_read = WaylandClipboardRead{
			request:     ServiceRequestId{
				app_instance: 1
				serial:       78
			}
			window:      window
			fd:          read_fd
			buffer:      []u8{}
			deadline_ns: 0
		}
		backend.clipboard_read_active = true

		backend.drain_clipboard_read()

		assert backend.clipboard_read_active
		assert backend.clipboard_read.buffer.len == wayland_clipboard_io_chunk_size * wayland_clipboard_max_io_chunks_per_poll
		assert record.pending_events.len == 0
		backend.drain_clipboard_read()
		assert !backend.clipboard_read_active
		assert record.pending_events.len == 1
		result := record.pending_events[0].event.service.clipboard
		assert result.status == .ready
		assert result.text == payload
	}
}

fn test_wayland_clipboard_read_progress_refreshes_inactivity_deadline_before_eagain() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		_, record, fds := wayland_test_begin_clipboard_read(mut backend, 85, []u8{})
		assert C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 1
		payload := 'progress-before-read-eagain'
		assert C.write(fds[1], payload.str, usize(payload.len)) == payload.len
		backend.clipboard_read.deadline_ns = 0

		backend.drain_clipboard_read()

		assert backend.clipboard_read_active
		assert backend.clipboard_read.buffer.bytestr() == payload
		assert backend.clipboard_read.deadline_ns > 0
		assert record.pending_events.len == 0

		backend.clipboard_read.deadline_ns = 0
		backend.drain_clipboard_read()
		assert !backend.clipboard_read_active
		assert record.pending_events.len == 1
		result := record.pending_events[0].event.service.clipboard
		assert result.status == .failed
		assert result.error == err_clipboard_timeout
		backend.drain_clipboard_read()
		assert record.pending_events.len == 1
		C.close(fds[1])
	}
}

fn wayland_assert_clipboard_replacement_preflight_failure(fail_listener bool) {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{
			started:             true
			display:             voidptr(usize(0x11))
			data_device_manager: voidptr(usize(0x12))
			data_device:         voidptr(usize(0x13))
			seat:                voidptr(usize(0x14))
			poll_generation:     1
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:    window
			owner: backend
		}
		record.store_user_action_serial(19, backend.poll_generation)
		backend.windows << record
		old_source := voidptr(usize(0x21))
		mut new_source := voidptr(unsafe { nil })
		if fail_listener {
			new_source = voidptr(usize(0x22))
		}
		mut fds := [-1, -1]!
		assert C.pipe(&fds[0]) == 0
		backend.clipboard_source = old_source
		backend.clipboard_text = 'previous clipboard'
		backend.clipboard_sends << WaylandClipboardSend{
			fd:          fds[1]
			payload:     'previous clipboard'
			deadline_ns: i64(0x7fffffffffffffff)
		}
		backend.clipboard_send_snapshot_bytes = u64('previous clipboard'.len)
		backend.clipboard_source_test = WaylandClipboardSourceTestSeam{
			active:          true
			create_override: true
			next_source:     new_source
			fail_listener:   fail_listener
			bypass_protocol: true
		}
		mut failure := ''
		backend.service_set_clipboard_text(window, ServiceRequestId{
			app_instance: 1
			serial:       23
		}, 'replacement clipboard') or { failure = err.msg() }

		assert failure == err_capability_unsupported
		assert backend.clipboard_source == old_source
		assert backend.clipboard_text == 'previous clipboard'
		assert backend.clipboard_sends.len == 1
		assert backend.clipboard_sends[0].fd == fds[1]
		assert backend.clipboard_sends[0].payload == 'previous clipboard'
		assert record.pending_events.len == 0
		if fail_listener {
			assert backend.clipboard_source_test.destroyed == [usize(new_source)]
		} else {
			assert backend.clipboard_source_test.destroyed.len == 0
		}
		assert C.write(fds[1], c'x', usize(1)) == 1
		backend.clipboard_source = unsafe { nil }
		backend.clipboard_source_test.active = false
		backend.close_all_clipboard_sends()
		C.close(fds[0])
	}
}

fn test_wayland_clipboard_replacement_create_failure_preserves_previous_source() {
	wayland_assert_clipboard_replacement_preflight_failure(false)
}

fn test_wayland_clipboard_replacement_listener_failure_preserves_previous_source() {
	wayland_assert_clipboard_replacement_preflight_failure(true)
}

fn test_wayland_clipboard_transport_plan_exhaustion_has_no_ghost_and_flush_failure_is_terminal() {
	$if linux && sokol_wayland ? {
		mut native_operations := &NativeOperationAuthority{}
		native_operations.bind_app_lifetime(1, 1)!
		native_operations.advance_renderer_attempt(1, 2)!
		old_source := voidptr(usize(0x31))
		new_source := voidptr(usize(0x32))
		mut backend := &WaylandBackend{
			started:               true
			display:               voidptr(usize(0x33))
			data_device_manager:   voidptr(usize(0x34))
			data_device:           voidptr(usize(0x35))
			seat:                  voidptr(usize(0x36))
			poll_generation:       1
			native_operations:     native_operations
			clipboard_source:      old_source
			clipboard_text:        'old clipboard'
			clipboard_source_test: WaylandClipboardSourceTestSeam{
				active:                  true
				create_override:         true
				next_source:             new_source
				bypass_protocol:         true
				exercise_transport_plan: true
			}
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:    window
			owner: backend
		}
		record.store_user_action_serial(41, backend.poll_generation)
		backend.windows << record
		mut fds := [-1, -1]!
		assert C.pipe(&fds[0]) == 0
		backend.clipboard_sends << WaylandClipboardSend{
			fd:          fds[1]
			payload:     'old clipboard'
			deadline_ns: i64(0x7fffffffffffffff)
		}
		backend.clipboard_send_snapshot_bytes = u64('old clipboard'.len)

		native_operations.next_ordinal = ~u64(0)
		mut exhausted_error := ''
		backend.service_set_clipboard_text(window, ServiceRequestId{
			app_instance: 1
			serial:       51
		}, 'new clipboard') or { exhausted_error = err.msg() }
		assert exhausted_error == err_capability_unsupported
		assert record.user_action_serial_valid
		assert record.user_action_serial == 41
		assert backend.clipboard_source_test.selection_requests == 0
		assert backend.clipboard_source_test.destroyed.len == 0
		assert backend.clipboard_source == old_source
		assert backend.clipboard_text == 'old clipboard'
		assert backend.clipboard_sends.len == 1

		native_operations.next_ordinal = 1
		backend.clipboard_source_test.fail_flush_after_selection = true
		result := backend.service_set_clipboard_text(window, ServiceRequestId{
			app_instance: 1
			serial:       52
		}, 'new clipboard')!
		assert result.completed
		assert result.status == .failed
		assert result.error == err_wayland_flush_failed
		assert !record.user_action_serial_valid
		assert backend.clipboard_source_test.selection_requests == 1
		assert backend.clipboard_source_test.destroyed == [usize(new_source)]
		assert backend.clipboard_source_test.destroyed_local == [true]
		assert backend.clipboard_source == old_source
		assert backend.clipboard_text == 'old clipboard'
		assert backend.clipboard_sends.len == 1
		assert backend.render_health.blocks_graphics()
		assert backend.wayland_display_unavailable

		mut retry_error := ''
		backend.service_set_clipboard_text(window, ServiceRequestId{
			app_instance: 1
			serial:       53
		}, 'retry') or { retry_error = err.msg() }
		assert retry_error == err_capability_unsupported
		assert backend.clipboard_source_test.selection_requests == 1
		assert backend.clipboard_source_test.destroyed.len == 1
		backend.clipboard_source = unsafe { nil }
		backend.clipboard_source_test.active = false
		backend.close_all_clipboard_sends()
		C.close(fds[0])
	}
}

fn test_wayland_interactive_move_resize_preflight_preserves_serial_and_ordinals() {
	$if linux && sokol_wayland ? {
		mut native_operations := &NativeOperationAuthority{}
		native_operations.bind_app_lifetime(1, 1)!
		native_operations.advance_renderer_attempt(1, 2)!
		mut backend := &WaylandBackend{
			started:              true
			display:              voidptr(usize(0x41))
			seat:                 voidptr(usize(0x42))
			poll_generation:      1
			native_operations:    native_operations
			toplevel_replay_test: WaylandToplevelReplayTestSeam{
				interactive_request_override: true
			}
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:           window
			owner:        backend
			resizable:    true
			xdg_toplevel: voidptr(usize(0x43))
		}
		backend.windows << record

		ordinal_without_serial := native_operations.next_ordinal
		for resize in [false, true] {
			mut failure := ''
			if resize {
				backend.begin_window_resize(window, .bottom_right) or { failure = err.msg() }
			} else {
				backend.begin_window_move(window) or { failure = err.msg() }
			}
			assert failure == err_capability_unsupported
			assert native_operations.next_ordinal == ordinal_without_serial
		}

		record.store_user_action_serial(61, backend.poll_generation)
		native_operations.next_ordinal = ~u64(0)
		mut exhausted_move := ''
		backend.begin_window_move(window) or { exhausted_move = err.msg() }
		assert exhausted_move == err_render_native_renderer_unavailable
		assert record.user_action_serial_valid
		assert record.user_action_serial == 61
		assert backend.toplevel_replay_test.interactive_move_requests == 0

		native_operations.next_ordinal = 1
		backend.begin_window_move(window)!
		assert !record.user_action_serial_valid
		assert native_operations.next_ordinal == 3
		assert backend.toplevel_replay_test.interactive_move_requests == 1
		assert backend.toplevel_replay_test.interactive_transport_finishes == 1

		record.store_user_action_serial(62, backend.poll_generation)
		native_operations.next_ordinal = ~u64(0)
		mut exhausted_resize := ''
		backend.begin_window_resize(window, .bottom_right) or { exhausted_resize = err.msg() }
		assert exhausted_resize == err_render_native_renderer_unavailable
		assert record.user_action_serial_valid
		assert record.user_action_serial == 62
		assert backend.toplevel_replay_test.interactive_resize_requests == 0

		native_operations.next_ordinal = 1
		backend.begin_window_resize(window, .bottom_right)!
		assert !record.user_action_serial_valid
		assert native_operations.next_ordinal == 3
		assert backend.toplevel_replay_test.interactive_resize_requests == 1
		assert backend.toplevel_replay_test.interactive_transport_finishes == 2
	}
}

fn test_wayland_visible_create_preplans_lifecycle_transport_before_native_creation() {
	$if linux && sokol_wayland ? {
		mut native_operations := &NativeOperationAuthority{}
		native_operations.bind_app_lifetime(1, 1)!
		native_operations.advance_renderer_attempt(1, 2)!
		// Six ordinals can cover F/R/F, but not the lifecycle F required by a visible
		// window while the renderer is unavailable.
		native_operations.next_ordinal = ~u64(0) - 5
		mut backend := &WaylandBackend{
			started:              true
			display:              voidptr(usize(0x61))
			compositor:           voidptr(usize(0x62))
			compositor_name:      1
			wm_base:              voidptr(usize(0x63))
			wm_base_name:         2
			native_operations:    native_operations
			toplevel_replay_test: WaylandToplevelReplayTestSeam{
				create_request_override: true
			}
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		ordinal_before := native_operations.next_ordinal
		mut failure := ''
		backend.create_window(window, WindowConfig{
			title:   'preplanned lifecycle'
			visible: true
		}) or { failure = err.msg() }
		assert failure == err_render_native_renderer_unavailable
		assert native_operations.next_ordinal == ordinal_before
		assert backend.toplevel_replay_test.create_requests == 0
		assert backend.windows.len == 0
	}
}

fn test_wayland_live_cleanup_plan_exhaustion_preserves_mouse_and_portal_handles() {
	$if linux && sokol_wayland ? {
		mut native_operations := &NativeOperationAuthority{}
		native_operations.bind_app_lifetime(1, 1)!
		native_operations.advance_renderer_attempt(1, 2)!
		native_operations.next_ordinal = ~u64(0)
		mut backend := &WaylandBackend{
			started:           true
			display:           voidptr(usize(0x51))
			native_operations: native_operations
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		locked := voidptr(usize(0x52))
		relative := voidptr(usize(0x53))
		mut record := &WaylandWindowRecord{
			id:                   window
			owner:                backend
			locked_pointer:       locked
			relative_pointer:     relative
			mouse_lock_requested: true
			mouse_locked:         true
		}
		backend.windows << record
		lease := ServicePortalLeaseId{
			app_instance: 1
			serial:       7
		}
		exported := voidptr(usize(0x54))
		backend.portal_exports << &WaylandPortalExport{
			window:   window
			lease:    lease
			owner:    backend
			exported: exported
		}

		mut mouse_error := ''
		backend.service_set_mouse_lock(window, false) or { mouse_error = err.msg() }
		assert mouse_error == err_render_native_renderer_unavailable
		assert record.locked_pointer == locked
		assert record.relative_pointer == relative
		assert record.mouse_lock_requested
		assert record.mouse_locked
		assert record.pending_events.len == 0

		mut portal_error := ''
		backend.service_release_portal_parent(lease) or { portal_error = err.msg() }
		assert portal_error == err_render_native_renderer_unavailable
		assert backend.portal_exports.len == 1
		assert backend.portal_exports[0].exported == exported
		assert backend.portal_exports[0].owner == backend
	}
}

fn test_wayland_clipboard_selection_replacement_cancels_read_exactly_once() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:    window
			owner: backend
		}
		backend.windows << record
		mut fds := [-1, -1]!
		assert C.pipe(&fds[0]) == 0
		backend.clipboard_read = WaylandClipboardRead{
			request:     ServiceRequestId{
				app_instance: 1
				serial:       8
			}
			window:      window
			fd:          fds[0]
			buffer:      []u8{}
			deadline_ns: i64(0x7fffffffffffffff)
		}
		backend.clipboard_read_active = true
		replacement := voidptr(usize(0x51))

		backend.set_selection_offer(replacement)
		C.close(fds[1])

		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		assert backend.selection_offer == replacement
		assert record.pending_events.len == 1
		result := record.pending_events[0].event.service.clipboard
		assert result.status == .failed
		assert result.error == err_clipboard_selection_lost
		backend.drain_clipboard_read()
		assert record.pending_events.len == 1
		// The deterministic replacement is not a live wl_data_offer proxy.
		backend.selection_offer = unsafe { nil }
	}
}

fn wayland_test_begin_clipboard_read(mut backend WaylandBackend, serial u64, buffer []u8) (WindowId, &WaylandWindowRecord, [2]int) {
	window := WindowId{
		app_instance: 1
		slot:         0
		generation:   1
	}
	mut record := &WaylandWindowRecord{
		id:    window
		owner: backend
	}
	backend.windows << record
	mut fds := [-1, -1]!
	assert C.pipe(&fds[0]) == 0
	backend.clipboard_read = WaylandClipboardRead{
		request:     ServiceRequestId{
			app_instance: 1
			serial:       serial
		}
		window:      window
		fd:          fds[0]
		buffer:      buffer
		deadline_ns: i64(0x7fffffffffffffff)
	}
	backend.clipboard_read_active = true
	return window, record, fds
}

fn wayland_assert_clipboard_failure(record &WaylandWindowRecord, request ServiceRequestId, message string) {
	assert record.pending_events.len == 1
	event := record.pending_events[0].event
	assert event.kind == .service
	assert event.service.kind == .clipboard
	assert event.service.operation == .clipboard_read
	result := event.service.clipboard
	assert result.id == request
	assert result.status == .failed
	assert result.error == message
}

fn test_wayland_seat_removal_finishes_pending_clipboard_read_exactly_once() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		_, record, fds := wayland_test_begin_clipboard_read(mut backend, 80, []u8{})
		request := backend.clipboard_read.request
		backend.seat_name = 71

		wayland_registry_handle_global_remove(voidptr(backend), unsafe { nil }, 71)
		C.close(fds[1])

		assert backend.seat_name == 0
		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		assert backend.clipboard_read.buffer.len == 0
		wayland_assert_clipboard_failure(record, request, err_clipboard_selection_lost)
		wayland_registry_handle_global_remove(voidptr(backend), unsafe { nil }, 71)
		assert record.pending_events.len == 1
	}
}

fn test_wayland_data_device_manager_removal_finishes_pending_clipboard_read_exactly_once() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		_, record, fds := wayland_test_begin_clipboard_read(mut backend, 81, []u8{})
		request := backend.clipboard_read.request
		backend.data_device_manager_name = 72

		wayland_registry_handle_global_remove(voidptr(backend), unsafe { nil }, 72)
		C.close(fds[1])

		assert backend.data_device_manager_name == 0
		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		assert backend.clipboard_read.buffer.len == 0
		wayland_assert_clipboard_failure(record, request, err_clipboard_selection_lost)
		wayland_registry_handle_global_remove(voidptr(backend), unsafe { nil }, 72)
		assert record.pending_events.len == 1
	}
}

fn test_wayland_clipboard_exact_capacity_succeeds_at_eof() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		payload := []u8{len: wayland_clipboard_max_bytes, init: `a`}
		_, record, fds := wayland_test_begin_clipboard_read(mut backend, 82, payload)
		C.close(fds[1])
		backend.io_test.clipboard_read_interruptions = 1

		backend.drain_clipboard_read()

		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		assert record.pending_events.len == 1
		result := record.pending_events[0].event.service.clipboard
		assert result.status == .ready
		assert result.error == ''
		assert result.text.len == wayland_clipboard_max_bytes
	}
}

fn test_wayland_clipboard_capacity_plus_one_fails() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		payload := []u8{len: wayland_clipboard_max_bytes, init: `b`}
		_, record, fds := wayland_test_begin_clipboard_read(mut backend, 83, payload)
		assert C.write(fds[1], c'x', usize(1)) == 1
		C.close(fds[1])

		backend.drain_clipboard_read()

		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		assert record.pending_events.len == 1
		result := record.pending_events[0].event.service.clipboard
		assert result.status == .failed
		assert result.error == err_clipboard_capacity
		assert result.text == ''
	}
}

fn test_wayland_clipboard_exact_capacity_waits_on_eagain_then_succeeds_at_eof() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		payload := []u8{len: wayland_clipboard_max_bytes, init: `c`}
		_, record, fds := wayland_test_begin_clipboard_read(mut backend, 84, payload)
		assert C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 1

		backend.drain_clipboard_read()
		assert backend.clipboard_read_active
		assert backend.clipboard_read.fd == fds[0]
		assert record.pending_events.len == 0

		C.close(fds[1])
		backend.drain_clipboard_read()
		assert !backend.clipboard_read_active
		assert backend.clipboard_read.fd == -1
		assert record.pending_events.len == 1
		result := record.pending_events[0].event.service.clipboard
		assert result.status == .ready
		assert result.error == ''
		assert result.text.len == wayland_clipboard_max_bytes
	}
}

fn test_wayland_file_drop_poll_and_read_eintr_preserve_transaction_until_exact_terminal() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{
			display: voidptr(usize(0x91))
			io_test: WaylandIoTestSeam{
				pending_drop_poll_interruptions: 1
				pending_drop_read_interruptions: 1
				pending_drop_native_bypass:      true
			}
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:     window
			owner:  backend
			width:  40
			height: 30
		}
		backend.windows << record
		mut fds := [-1, -1]!
		assert C.pipe(&fds[0]) == 0
		assert C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 1
		payload := 'file:///tmp/eintr-proof\n'
		assert C.write(fds[1], payload.str, usize(payload.len)) == payload.len
		C.close(fds[1])
		offer := voidptr(usize(0x92))
		backend.data_offer = offer
		backend.pending_drop_offer = offer
		backend.pending_drop_fd = fds[0]
		backend.pending_drop_window = window
		backend.pending_drop_window_valid = true
		backend.pending_drop_source_actions = wayland_dnd_action_copy
		backend.pending_drop_selected_action = wayland_dnd_action_copy
		backend.pending_drop_action_received = true

		backend.drain_pending_data_offer_drop()
		assert backend.pending_drop_offer == offer
		assert backend.pending_drop_fd == fds[0]
		assert backend.pending_drop_poll_cycles == 0
		assert record.pending_events.len == 0

		backend.pending_drop_poll_cycles = wayland_data_offer_max_pending_poll_cycles
		backend.drain_pending_data_offer_drop()
		assert backend.pending_drop_offer == unsafe { nil }
		assert backend.pending_drop_fd == -1
		assert record.pending_events.len == 1
		event := record.pending_events[0].event
		assert event.kind == .input
		assert event.input.kind == .files_dropped
		assert event.input.dropped_files == ['/tmp/eintr-proof']
		backend.drain_pending_data_offer_drop()
		assert record.pending_events.len == 1
	}
}

fn test_wayland_file_drop_empty_open_pipe_expires_without_terminal_event() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{
			display: voidptr(usize(0x93))
			io_test: WaylandIoTestSeam{
				pending_drop_native_bypass: true
			}
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:    window
			owner: backend
		}
		backend.windows << record
		mut fds := [-1, -1]!
		assert C.pipe(&fds[0]) == 0
		assert C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 1
		offer := voidptr(usize(0x94))
		backend.data_offer = offer
		backend.pending_drop_offer = offer
		backend.pending_drop_fd = fds[0]
		backend.pending_drop_window = window
		backend.pending_drop_window_valid = true
		backend.pending_drop_source_actions = wayland_dnd_action_copy
		backend.pending_drop_selected_action = wayland_dnd_action_copy
		backend.pending_drop_action_received = true
		backend.pending_drop_poll_cycles = wayland_data_offer_max_pending_poll_cycles

		backend.drain_pending_data_offer_drop()
		assert backend.pending_drop_offer == unsafe { nil }
		assert backend.pending_drop_fd == -1
		assert record.pending_events.len == 0
		C.close(fds[1])
	}
}

fn test_wayland_file_drop_exact_capacity_requires_eof_and_rejects_one_extra_byte() {
	$if linux && sokol_wayland ? {
		for extra in 0 .. 2 {
			mut backend := &WaylandBackend{
				display: voidptr(usize(0x95 + extra))
				io_test: WaylandIoTestSeam{
					pending_drop_native_bypass: true
				}
			}
			window := WindowId{
				app_instance: 1
				slot:         0
				generation:   1
			}
			mut record := &WaylandWindowRecord{
				id:    window
				owner: backend
			}
			backend.windows << record
			mut fds := [-1, -1]!
			assert C.pipe(&fds[0]) == 0
			assert C.v_multiwindow_wayland_fd_set_nonblocking(fds[0]) == 1
			prefix := 'file:///tmp/'
			payload := prefix + 'a'.repeat(wayland_uri_list_buffer_size - prefix.len - 1) + '\n'
			assert payload.len == wayland_uri_list_buffer_size
			if extra == 1 {
				assert C.write(fds[1], c'x', usize(1)) == 1
			}
			C.close(fds[1])
			offer := voidptr(usize(0x97 + extra))
			backend.data_offer = offer
			backend.pending_drop_offer = offer
			backend.pending_drop_fd = fds[0]
			backend.pending_drop_window = window
			backend.pending_drop_window_valid = true
			backend.pending_drop_source_actions = wayland_dnd_action_copy
			backend.pending_drop_selected_action = wayland_dnd_action_copy
			backend.pending_drop_action_received = true
			backend.pending_drop_buffer = payload.bytes()

			backend.drain_pending_data_offer_drop()

			assert backend.pending_drop_offer == unsafe { nil }
			assert backend.pending_drop_fd == -1
			if extra == 0 {
				assert record.pending_events.len == 1
				event := record.pending_events[0].event
				assert event.kind == .input
				assert event.input.kind == .files_dropped
				assert event.input.dropped_files.len == 1
				assert event.input.dropped_files[0].len == payload.len - 'file://'.len - 1
			} else {
				assert record.pending_events.len == 0
			}
		}
	}
}

fn wayland_read_exact_pipe_payload(fd int, expected_len int) string {
	mut payload := []u8{len: expected_len}
	mut offset := 0
	for offset < expected_len {
		n := C.read(fd, unsafe { &payload[offset] }, usize(expected_len - offset))
		assert n > 0
		offset += int(n)
	}
	mut eof := [1]u8{}
	assert C.read(fd, unsafe { &eof[0] }, usize(1)) == 0
	return payload.bytestr()
}

fn test_wayland_clipboard_send_is_fair_when_first_consumer_is_backpressured() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		source := voidptr(usize(0x61))
		backend.clipboard_source = source
		backend.clipboard_text = 'bounded clipboard payload'
		mut first := [-1, -1]!
		mut second := [-1, -1]!
		assert C.pipe(&first[0]) == 0
		assert C.pipe(&second[0]) == 0
		assert C.v_multiwindow_wayland_fd_set_nonblocking(first[1]) == 1
		mut fill := [4096]u8{}
		mut saturated := false
		for _ in 0 .. 4096 {
			if C.write(first[1], unsafe { &fill[0] }, usize(fill.len)) < 0 {
				assert C.v_multiwindow_wayland_read_would_block() != 0
				saturated = true
				break
			}
		}
		assert saturated

		wayland_data_source_send(backend, source, c'text/plain;charset=utf-8', first[1])
		wayland_data_source_send(backend, source, c'text/plain', second[1])
		assert backend.clipboard_sends.len == 2
		backend.drain_clipboard_send()
		assert backend.clipboard_sends.len == 1
		assert backend.clipboard_sends[0].fd == first[1]
		assert wayland_read_exact_pipe_payload(second[0], backend.clipboard_text.len) == backend.clipboard_text
		backend.close_all_clipboard_sends()
		assert backend.clipboard_sends.len == 0
		C.close(first[0])
		C.close(second[0])
		backend.clipboard_source = unsafe { nil }
	}
}

fn test_wayland_clipboard_send_progress_refreshes_inactivity_deadline_before_eagain() {
	$if linux && sokol_wayland ? {
		mut fds := [-1, -1]!
		assert C.pipe(&fds[0]) == 0
		assert C.v_multiwindow_wayland_fd_set_nonblocking(fds[1]) == 1
		mut fill := [4096]u8{}
		mut saturated := false
		for _ in 0 .. 4096 {
			if C.write(fds[1], unsafe { &fill[0] }, usize(fill.len)) < 0 {
				assert C.v_multiwindow_wayland_read_would_block() != 0
				saturated = true
				break
			}
		}
		assert saturated
		mut drained := [4096]u8{}
		assert C.read(fds[0], unsafe { &drained[0] }, usize(drained.len)) == drained.len
		payload := 'p'.repeat(wayland_clipboard_io_chunk_size * 2)
		mut backend := &WaylandBackend{
			clipboard_sends:               [
				WaylandClipboardSend{
					fd:          fds[1]
					payload:     payload
					deadline_ns: 0
				},
			]
			clipboard_send_snapshot_bytes: u64(payload.len)
		}

		backend.drain_clipboard_send()

		assert backend.clipboard_sends.len == 1
		assert backend.clipboard_sends[0].offset > 0
		assert backend.clipboard_sends[0].deadline_ns > 0

		backend.clipboard_sends[0].deadline_ns = 0
		backend.drain_clipboard_send()
		assert backend.clipboard_sends.len == 0
		assert backend.clipboard_send_snapshot_bytes == 0
		C.close(fds[0])
	}
}

fn test_wayland_clipboard_send_snapshots_survive_replacement_and_cancel() {
	$if linux && sokol_wayland ? {
		old_source := voidptr(usize(0x71))
		new_source := voidptr(usize(0x72))
		mut backend := &WaylandBackend{
			started:               true
			display:               voidptr(usize(0x73))
			data_device_manager:   voidptr(usize(0x74))
			data_device:           voidptr(usize(0x75))
			seat:                  voidptr(usize(0x76))
			poll_generation:       1
			clipboard_source:      old_source
			clipboard_text:        'old payload'
			clipboard_source_test: WaylandClipboardSourceTestSeam{
				active:          true
				create_override: true
				next_source:     new_source
				bypass_protocol: true
			}
		}
		window := WindowId{
			app_instance: 1
			slot:         0
			generation:   1
		}
		mut record := &WaylandWindowRecord{
			id:    window
			owner: backend
		}
		record.store_user_action_serial(31, backend.poll_generation)
		backend.windows << record
		mut old_pipe := [-1, -1]!
		mut new_pipe := [-1, -1]!
		assert C.pipe(&old_pipe[0]) == 0
		assert C.pipe(&new_pipe[0]) == 0
		wayland_data_source_send(backend, old_source, c'text/plain', old_pipe[1])
		assert backend.clipboard_sends.len == 1

		result := backend.service_set_clipboard_text(window, ServiceRequestId{
			app_instance: 1
			serial:       91
		}, 'new payload')!
		assert result.completed
		assert backend.clipboard_source == new_source
		wayland_data_source_send(backend, new_source, c'text/plain;charset=utf-8', new_pipe[1])
		assert backend.clipboard_sends.len == 2
		wayland_data_source_cancelled(backend, new_source)
		assert backend.clipboard_source == unsafe { nil }
		assert backend.clipboard_sends.len == 2
		assert backend.clipboard_source_test.destroyed == [usize(old_source), usize(new_source)]
		assert backend.clipboard_source_test.destroyed_local == [false, false]

		backend.io_test.clipboard_write_interruptions = 1
		backend.drain_clipboard_send()
		assert backend.clipboard_sends.len == 0
		assert backend.clipboard_send_snapshot_bytes == 0
		assert wayland_read_exact_pipe_payload(old_pipe[0], 'old payload'.len) == 'old payload'
		assert wayland_read_exact_pipe_payload(new_pipe[0], 'new payload'.len) == 'new payload'
		C.close(old_pipe[0])
		C.close(new_pipe[0])
	}
}

fn test_wayland_clipboard_send_expired_writable_progress_and_teardown_are_independent() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		source := voidptr(usize(0x81))
		backend.clipboard_source = source
		backend.clipboard_text = 'still delivered'
		mut expired := [-1, -1]!
		mut live := [-1, -1]!
		assert C.pipe(&expired[0]) == 0
		assert C.pipe(&live[0]) == 0
		wayland_data_source_send(backend, source, c'text/plain', expired[1])
		wayland_data_source_send(backend, source, c'text/plain', live[1])
		assert backend.clipboard_sends.len == 2
		backend.clipboard_sends[0].deadline_ns = 0
		backend.drain_clipboard_send()
		assert backend.clipboard_sends.len == 0
		assert wayland_read_exact_pipe_payload(expired[0], 'still delivered'.len) == 'still delivered'
		assert wayland_read_exact_pipe_payload(live[0], 'still delivered'.len) == 'still delivered'
		C.close(expired[0])
		C.close(live[0])

		mut teardown := [-1, -1]!
		assert C.pipe(&teardown[0]) == 0
		wayland_data_source_send(backend, source, c'text/plain', teardown[1])
		assert backend.clipboard_sends.len == 1
		backend.close_all_clipboard_sends()
		assert backend.clipboard_sends.len == 0
		assert backend.clipboard_send_snapshot_bytes == 0
		mut eof := [1]u8{}
		assert C.read(teardown[0], unsafe { &eof[0] }, usize(1)) == 0
		C.close(teardown[0])
		backend.clipboard_source = unsafe { nil }
	}
}

fn test_wayland_clipboard_send_expired_backpressure_closes_only_that_transfer() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		source := voidptr(usize(0x82))
		backend.clipboard_source = source
		backend.clipboard_text = 'independent live payload'
		mut expired := [-1, -1]!
		mut live := [-1, -1]!
		assert C.pipe(&expired[0]) == 0
		assert C.pipe(&live[0]) == 0
		assert C.v_multiwindow_wayland_fd_set_nonblocking(expired[1]) == 1
		mut fill := [4096]u8{}
		mut saturated := false
		for _ in 0 .. 4096 {
			if C.write(expired[1], unsafe { &fill[0] }, usize(fill.len)) < 0 {
				assert C.v_multiwindow_wayland_read_would_block() != 0
				saturated = true
				break
			}
		}
		assert saturated

		wayland_data_source_send(backend, source, c'text/plain', expired[1])
		wayland_data_source_send(backend, source, c'text/plain', live[1])
		assert backend.clipboard_sends.len == 2
		backend.clipboard_sends[0].deadline_ns = 0
		backend.drain_clipboard_send()
		assert backend.clipboard_sends.len == 0
		assert wayland_read_exact_pipe_payload(live[0], backend.clipboard_text.len) == backend.clipboard_text
		C.close(expired[0])
		C.close(live[0])
		backend.clipboard_source = unsafe { nil }
	}
}

fn test_wayland_clipboard_safe_write_contains_sigpipe_in_child_process() {
	$if linux && sokol_wayland ? {
		assert C.v_multiwindow_wayland_safe_write_broken_pipe_probe() == 1
	}
}

fn test_wayland_clipboard_send_rejects_invalid_and_bounded_admissions_only() {
	$if linux && sokol_wayland ? {
		mut backend := &WaylandBackend{}
		source := voidptr(usize(0xa1))
		backend.clipboard_source = source
		backend.clipboard_text = 'x'
		mut invalid_source := [-1, -1]!
		mut invalid_mime := [-1, -1]!
		mut byte_limit := [-1, -1]!
		mut count_limit := [-1, -1]!
		assert C.pipe(&invalid_source[0]) == 0
		assert C.pipe(&invalid_mime[0]) == 0
		assert C.pipe(&byte_limit[0]) == 0
		assert C.pipe(&count_limit[0]) == 0

		wayland_data_source_send(backend, voidptr(usize(0xa2)), c'text/plain', invalid_source[1])
		wayland_data_source_send(backend, source, c'application/octet-stream', invalid_mime[1])
		backend.clipboard_send_snapshot_bytes = wayland_clipboard_max_outgoing_snapshot_bytes
		wayland_data_source_send(backend, source, c'text/plain', byte_limit[1])
		backend.clipboard_send_snapshot_bytes = 0
		for _ in 0 .. wayland_clipboard_max_outgoing_transfers {
			backend.clipboard_sends << WaylandClipboardSend{}
		}
		wayland_data_source_send(backend, source, c'text/plain', count_limit[1])

		assert backend.clipboard_sends.len == wayland_clipboard_max_outgoing_transfers
		assert C.write(invalid_source[1], c'x', usize(1)) == -1
		assert C.write(invalid_mime[1], c'x', usize(1)) == -1
		assert C.write(byte_limit[1], c'x', usize(1)) == -1
		assert C.write(count_limit[1], c'x', usize(1)) == -1
		backend.close_all_clipboard_sends()
		C.close(invalid_source[0])
		C.close(invalid_mime[0])
		C.close(byte_limit[0])
		C.close(count_limit[0])
		backend.clipboard_source = unsafe { nil }
	}
}

fn test_wayland_native_borrow_copy_is_stale_after_callback() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut app := new_app(backend: .wayland, require_renderer: false)!
		window := app.create_window(title: 'package2-wayland-borrow-stale')!
		_ = app.drain_queued_events()!
		mut copied := NativeWindowBorrow{}
		callback := fn [mut copied] (borrow NativeWindowBorrow) ! {
			assert borrow.backend_for_gg() == .wayland
			assert borrow.primary_for_gg() != unsafe { nil }
			assert borrow.secondary_for_gg() != 0
			copied = borrow
		}
		app.with_native_window_for_gg(window, callback)!
		app.validate_native_borrow_for_gg(window, copied.epoch_for_gg()) or {
			assert err.msg() == err_native_borrow_stale
			app.stop()!
			return
		}
		assert false, 'copied Wayland native borrow remained valid after its callback'
	}
}

fn test_wayland_native_borrow_defers_destroy_and_stop_until_callback_return() {
	$if linux && sokol_wayland ? {
		if !wayland_runtime_probe_available() {
			return
		}
		mut destroy_app := new_app(backend: .wayland, require_renderer: false)!
		destroy_window := destroy_app.create_window(title: 'package2-wayland-borrow-destroy')!
		_ = destroy_app.drain_queued_events()!
		destroy_ptr := unsafe { voidptr(destroy_app) }
		destroy_callback := fn [destroy_ptr, destroy_window] (_ NativeWindowBorrow) ! {
			mut owner := unsafe { &App(destroy_ptr) }
			owner.destroy_window(destroy_window)!
			assert owner.window_exists(destroy_window)
			assert owner.backend.wayland.window_record_index(destroy_window) != none
		}
		destroy_app.with_native_window_for_gg(destroy_window, destroy_callback)!
		assert !destroy_app.window_exists(destroy_window)
		destroy_app.stop()!

		mut stop_app := new_app(backend: .wayland, require_renderer: false)!
		stop_window := stop_app.create_window(title: 'package2-wayland-borrow-stop')!
		_ = stop_app.drain_queued_events()!
		stop_ptr := unsafe { voidptr(stop_app) }
		stop_callback := fn [stop_ptr] (_ NativeWindowBorrow) ! {
			mut owner := unsafe { &App(stop_ptr) }
			owner.stop()!
			assert owner.status() == .running
		}
		stop_app.with_native_window_for_gg(stop_window, stop_callback)!
		assert stop_app.status() == .stopped
	}
}
