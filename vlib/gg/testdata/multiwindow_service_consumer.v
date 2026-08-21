module main

import gg

fn win32_native(hwnd voidptr) ! {
	_ = hwnd
}

fn appkit_native(ns_window voidptr) ! {
	_ = ns_window
}

fn x11_native(display voidptr, window u64) ! {
	_ = display
	_ = window
}

fn wayland_native(display voidptr, surface voidptr) ! {
	_ = display
	_ = surface
}

fn borrow_native(mut lease gg.NativeWindowLease) ! {
	lease.with_win32(win32_native)!
	lease.with_appkit(appkit_native)!
	lease.with_x11(x11_native)!
	lease.with_wayland(wayland_native)!
}

fn consume_service_surface(mut app gg.App, id gg.WindowId, monitor gg.WindowMonitorId, portal gg.PortalParentLeaseId, mut frame gg.WindowContext) ! {
	_ = gg.WindowMappingState.unknown
	_ = gg.AppConfig{
		app_id: 'org.vlang.multiwindow.contract'
	}
	_ = gg.WindowConfig{
		owner: id
		modal: true
	}
	_ = app.window_state(id)!
	_ = app.monitor_ids()!
	_ = app.monitor_info(monitor)!
	for operation in [gg.WindowOperation.native_borrow, .image_readback, .window_capture] {
		_ = app.window_operation_capability(id, operation)!
	}
	_ = app.supports_window_cursor(id, .text)!
	app.with_native_window(id, borrow_native)!
	app.show_window(id)!
	app.hide_window(id)!
	app.request_window_focus(id)!
	app.raise_window(id)!
	app.set_window_position(id, 1, 2)!
	app.minimize_window(id)!
	app.maximize_window(id)!
	app.restore_window(id)!
	app.set_window_fullscreen(id, true)!
	_ = app.request_clipboard_text(id)!
	_ = app.set_clipboard_text(id, 'text')!
	_ = app.request_portal_parent(id)!
	app.release_portal_parent(portal)!
	app.set_window_mouse_lock(id, true)!
	app.set_window_titlebar_appearance(id, .system)!
	_ = app.window_readback_capabilities(id)!
	_ = app.request_window_capture(id, gg.WindowReadbackConfig{})!
	_ = frame.request_image_readback(gg.WindowImageId{}, gg.WindowReadbackConfig{})!
	_ = app.drain_window_service_events()!
	_ = app.drain_window_queued_events()!
}

fn main() {
	mut app := gg.App{}
	mut frame := gg.WindowContext{}
	consume_service_surface(mut app, gg.WindowId{}, gg.WindowMonitorId{}, gg.PortalParentLeaseId{}, mut
		frame) or {}
}
