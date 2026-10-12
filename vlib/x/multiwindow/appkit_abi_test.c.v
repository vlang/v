module multiwindow

#flag darwin @VMODROOT/vlib/x/multiwindow/appkit_abi_test.o

fn C.v_appkit_native_layout(typ i32, member i32) i64

fn test_native_layout_vmultiwindownativeprimitive() {
	$if darwin {
		assert i64(sizeof(C.VMultiwindowNativePrimitive)) == C.v_appkit_native_layout(0, -1), 'VMultiwindowNativePrimitive size'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, valid_mask)) == C.v_appkit_native_layout(0, 0), 'VMultiwindowNativePrimitive.valid_mask offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, return_value)) == C.v_appkit_native_layout(0, 1), 'VMultiwindowNativePrimitive.return_value offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, handle)) == C.v_appkit_native_layout(0, 2), 'VMultiwindowNativePrimitive.handle offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, egl_error)) == C.v_appkit_native_layout(0, 3), 'VMultiwindowNativePrimitive.egl_error offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, native_errno)) == C.v_appkit_native_layout(0, 4), 'VMultiwindowNativePrimitive.native_errno offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, wayland_display_error)) == C.v_appkit_native_layout(0, 5), 'VMultiwindowNativePrimitive.wayland_display_error offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, dxgi_removal_reason)) == C.v_appkit_native_layout(0, 6), 'VMultiwindowNativePrimitive.dxgi_removal_reason offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, observed_count)) == C.v_appkit_native_layout(0, 7), 'VMultiwindowNativePrimitive.observed_count offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, observed_flags)) == C.v_appkit_native_layout(0, 8), 'VMultiwindowNativePrimitive.observed_flags offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, selected_value)) == C.v_appkit_native_layout(0, 9), 'VMultiwindowNativePrimitive.selected_value offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, object_identity_0)) == C.v_appkit_native_layout(0, 10), 'VMultiwindowNativePrimitive.object_identity_0 offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, object_identity_1)) == C.v_appkit_native_layout(0, 11), 'VMultiwindowNativePrimitive.object_identity_1 offset'
		assert i64(__offsetof(C.VMultiwindowNativePrimitive, object_identity_2)) == C.v_appkit_native_layout(0, 12), 'VMultiwindowNativePrimitive.object_identity_2 offset'
	}
}

fn test_native_layout_vmultiwindowappkitqueuedevent() {
	$if darwin {
		assert i64(sizeof(C.VMultiwindowAppKitQueuedEvent)) == C.v_appkit_native_layout(1, -1), 'VMultiwindowAppKitQueuedEvent size'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, sequence)) == C.v_appkit_native_layout(1, 0), 'VMultiwindowAppKitQueuedEvent.sequence offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, event_kind)) == C.v_appkit_native_layout(1, 1), 'VMultiwindowAppKitQueuedEvent.event_kind offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, lifecycle_kind)) == C.v_appkit_native_layout(1, 2), 'VMultiwindowAppKitQueuedEvent.lifecycle_kind offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, input_kind)) == C.v_appkit_native_layout(1, 3), 'VMultiwindowAppKitQueuedEvent.input_kind offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, key_code)) == C.v_appkit_native_layout(1, 4), 'VMultiwindowAppKitQueuedEvent.key_code offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, char_code)) == C.v_appkit_native_layout(1, 5), 'VMultiwindowAppKitQueuedEvent.char_code offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, key_repeat)) == C.v_appkit_native_layout(1, 6), 'VMultiwindowAppKitQueuedEvent.key_repeat offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, modifiers)) == C.v_appkit_native_layout(1, 7), 'VMultiwindowAppKitQueuedEvent.modifiers offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, mouse_button)) == C.v_appkit_native_layout(1, 8), 'VMultiwindowAppKitQueuedEvent.mouse_button offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, mouse_x)) == C.v_appkit_native_layout(1, 9), 'VMultiwindowAppKitQueuedEvent.mouse_x offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, mouse_y)) == C.v_appkit_native_layout(1, 10), 'VMultiwindowAppKitQueuedEvent.mouse_y offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, mouse_dx)) == C.v_appkit_native_layout(1, 11), 'VMultiwindowAppKitQueuedEvent.mouse_dx offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, mouse_dy)) == C.v_appkit_native_layout(1, 12), 'VMultiwindowAppKitQueuedEvent.mouse_dy offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, scroll_x)) == C.v_appkit_native_layout(1, 13), 'VMultiwindowAppKitQueuedEvent.scroll_x offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, scroll_y)) == C.v_appkit_native_layout(1, 14), 'VMultiwindowAppKitQueuedEvent.scroll_y offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, window_width)) == C.v_appkit_native_layout(1, 15), 'VMultiwindowAppKitQueuedEvent.window_width offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, window_height)) == C.v_appkit_native_layout(1, 16), 'VMultiwindowAppKitQueuedEvent.window_height offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, framebuffer_width)) == C.v_appkit_native_layout(1, 17), 'VMultiwindowAppKitQueuedEvent.framebuffer_width offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, framebuffer_height)) == C.v_appkit_native_layout(1, 18), 'VMultiwindowAppKitQueuedEvent.framebuffer_height offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_snapshot_valid)) == C.v_appkit_native_layout(1, 19), 'VMultiwindowAppKitQueuedEvent.service_snapshot_valid offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_kind)) == C.v_appkit_native_layout(1, 20), 'VMultiwindowAppKitQueuedEvent.service_kind offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_operation)) == C.v_appkit_native_layout(1, 21), 'VMultiwindowAppKitQueuedEvent.service_operation offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_mapping)) == C.v_appkit_native_layout(1, 22), 'VMultiwindowAppKitQueuedEvent.service_mapping offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_visibility)) == C.v_appkit_native_layout(1, 23), 'VMultiwindowAppKitQueuedEvent.service_visibility offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_active)) == C.v_appkit_native_layout(1, 24), 'VMultiwindowAppKitQueuedEvent.service_active offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_focused)) == C.v_appkit_native_layout(1, 25), 'VMultiwindowAppKitQueuedEvent.service_focused offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_minimized)) == C.v_appkit_native_layout(1, 26), 'VMultiwindowAppKitQueuedEvent.service_minimized offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_maximized)) == C.v_appkit_native_layout(1, 27), 'VMultiwindowAppKitQueuedEvent.service_maximized offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_fullscreen)) == C.v_appkit_native_layout(1, 28), 'VMultiwindowAppKitQueuedEvent.service_fullscreen offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_mouse_locked)) == C.v_appkit_native_layout(1, 29), 'VMultiwindowAppKitQueuedEvent.service_mouse_locked offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_position_known)) == C.v_appkit_native_layout(1, 30), 'VMultiwindowAppKitQueuedEvent.service_position_known offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_x)) == C.v_appkit_native_layout(1, 31), 'VMultiwindowAppKitQueuedEvent.service_x offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_y)) == C.v_appkit_native_layout(1, 32), 'VMultiwindowAppKitQueuedEvent.service_y offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_monitor_native_id)) == C.v_appkit_native_layout(1, 33), 'VMultiwindowAppKitQueuedEvent.service_monitor_native_id offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, service_scale)) == C.v_appkit_native_layout(1, 34), 'VMultiwindowAppKitQueuedEvent.service_scale offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, dropped_file_count)) == C.v_appkit_native_layout(1, 35), 'VMultiwindowAppKitQueuedEvent.dropped_file_count offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, dropped_files)) == C.v_appkit_native_layout(1, 36), 'VMultiwindowAppKitQueuedEvent.dropped_files offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, touch_count)) == C.v_appkit_native_layout(1, 37), 'VMultiwindowAppKitQueuedEvent.touch_count offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, touch_ids)) == C.v_appkit_native_layout(1, 38), 'VMultiwindowAppKitQueuedEvent.touch_ids offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, touch_x)) == C.v_appkit_native_layout(1, 39), 'VMultiwindowAppKitQueuedEvent.touch_x offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, touch_y)) == C.v_appkit_native_layout(1, 40), 'VMultiwindowAppKitQueuedEvent.touch_y offset'
		assert i64(__offsetof(C.VMultiwindowAppKitQueuedEvent, touch_changed)) == C.v_appkit_native_layout(1, 41), 'VMultiwindowAppKitQueuedEvent.touch_changed offset'
	}
}

fn test_native_layout_sg_mtl_image_info() {
	$if darwin {
		$if gg_multiwindow ?|| x_multiwindow_render ? {
			assert i64(sizeof(C.sg_mtl_image_info)) == C.v_appkit_native_layout(2, -1), 'sg_mtl_image_info size'
			assert i64(__offsetof(C.sg_mtl_image_info, tex)) == C.v_appkit_native_layout(2, 0), 'sg_mtl_image_info.tex offset'
			assert i64(__offsetof(C.sg_mtl_image_info, active_slot)) == C.v_appkit_native_layout(2, 1), 'sg_mtl_image_info.active_slot offset'
		}
	}
}
