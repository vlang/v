// Run with `v -d gg_multiwindow run examples/gg/multiwindow_services.v`.
// Add `-d x_multiwindow_x11` or `-d sokol_wayland` on Linux to enable that
// native backend. The capability checks also make the mock fallback useful.
module main

import gg

struct ServiceExampleState {
	backend gg.MultiWindowBackend
	mock    bool
mut:
	pending           int
	clipboard_request ?gg.ClipboardRequestId
	portal_request    ?gg.PortalParentRequestId
	readback_request  ?gg.WindowReadbackId
	completed         map[string]bool
}

fn main() {
	run_services_example() or {
		eprintln('multi-window services example failed: ${err.msg()}')
		exit(1)
	}
}

fn run_services_example() ! {
	mut app := gg.new_app(
		backend: .auto
		app_id:  'org.vlang.gg.multiwindow-services'
	)!
	defer {
		app.stop() or {}
	}

	window := app.create_window(
		title:   'GG multi-window services'
		width:   640
		height:  360
		visible: false
	)!
	caps := app.capabilities()
	mut state := &ServiceExampleState{
		backend: caps.backend
		mock:    caps.mock
	}
	println('backend=${caps.backend} native=${caps.native} readback=${caps.readback}')

	window_state := app.window_state(window)!
	println('window mapping=${window_state.mapping} visibility=${window_state.visibility}')
	for monitor_id in app.monitor_ids()! {
		monitor := app.monitor_info(monitor_id)!
		println('monitor ${monitor.id}: ${monitor.name} available=${monitor.available} scale_known=${monitor.scale.known}')
	}

	show_capability := app.window_operation_capability(window, .show)!
	println('show support=${show_capability.support} async=${show_capability.asynchronous}')
	if show_capability.support != .unsupported {
		app.show_window(window) or { eprintln('show request rejected: ${err.msg()}') }
	}
	inspect_native_window(mut app, window) or {
		eprintln('native borrow unavailable at runtime: ${err.msg()}')
	}
	request_clipboard(mut app, window, mut state)!
	request_portal_parent(mut app, window, mut state)!
	request_optional_readback(mut app, window, mut state)!

	if state.mock && state.pending == 0 {
		app.post(fn (mut app gg.App) ! {
			app.stop()!
		})!
	}

	app.run(
		event_fn:          fn (event gg.WindowEvent, mut app gg.App) ! {
			match event.kind {
				.window_close_requested {
					app.destroy_window(event.window)!
				}
				.window_destroyed {
					if app.window_ids()!.len == 0 {
						app.stop()!
					}
				}
				else {}
			}
		}
		input_fn:          fn (event gg.WindowInputEvent, mut _ gg.App) ! {
			println('input window=${event.window} kind=${event.event.typ}')
		}
		window_service_fn: fn [mut state] (event gg.WindowServiceEvent, mut app gg.App) ! {
			match event.kind {
				.clipboard {
					if !state.matches_clipboard(event.clipboard.id) {
						return
					}
					println('clipboard ${event.clipboard.id}: ${event.clipboard.status}')
					stop_example_if_complete(mut app, state.complete_one('clipboard'))
				}
				.portal_parent {
					if !state.matches_portal(event.portal_parent.id) {
						return
					}
					println('portal ${event.portal_parent.id}: ${event.portal_parent.status} ${event.portal_parent.identifier}')
					if event.portal_parent.status == .ready {
						release_portal_parent_bounded(mut app, event.portal_parent.lease)
					}
					// No fallible operation is propagated after the bounded release. Marking
					// the correlated event makes an unexpected replay idempotent.
					stop_example_if_complete(mut app, state.complete_one('portal'))
				}
				.state {
					println('state operation=${event.operation} mapping=${event.state.mapping} visibility=${event.state.visibility}')
				}
				.monitor {
					println('monitor membership changed: ${event.monitors.len} available')
				}
				else {}
			}
		}
		readback_fn:       fn [mut state] (result gg.WindowReadbackResult, mut app gg.App) ! {
			if !state.matches_readback(result.id) {
				return
			}
			println('readback status=${result.status} size=${result.width}x${result.height} stride=${result.stride}')
			stop_example_if_complete(mut app, state.complete_one('readback'))
		}
	)!
}

fn request_clipboard(mut app gg.App, window gg.WindowId, mut state ServiceExampleState) ! {
	capability := app.window_operation_capability(window, .clipboard_read)!
	println('clipboard_read support=${capability.support} async=${capability.asynchronous}')
	if capability.support == .unsupported {
		return
	}
	request := app.request_clipboard_text(window) or {
		eprintln('clipboard request rejected: ${err.msg()}')
		return
	}
	state.clipboard_request = request
	state.pending++
	println('clipboard request accepted: ${request}')
}

fn request_portal_parent(mut app gg.App, window gg.WindowId, mut state ServiceExampleState) ! {
	capability := app.window_operation_capability(window, .portal_parent)!
	println('portal_parent support=${capability.support} async=${capability.asynchronous}')
	if capability.support == .unsupported {
		return
	}
	request := app.request_portal_parent(window) or {
		eprintln('portal-parent request rejected: ${err.msg()}')
		return
	}
	state.portal_request = request
	state.pending++
	println('portal-parent request accepted: ${request}')
}

fn request_optional_readback(mut app gg.App, window gg.WindowId, mut state ServiceExampleState) ! {
	capabilities := app.window_readback_capabilities(window) or {
		eprintln('readback capability query failed: ${err.msg()}')
		return
	}
	println('readback image=${capabilities.offscreen_image} window=${capabilities.window_capture}')
	if !capabilities.window_capture {
		return
	}
	request := app.request_window_capture(window, gg.WindowReadbackConfig{}) or {
		eprintln('window capture request rejected: ${err.msg()}')
		return
	}
	state.readback_request = request
	state.pending++
	println('window capture accepted: ${request}')
}

fn inspect_native_window(mut app gg.App, window gg.WindowId) ! {
	capability := app.window_operation_capability(window, .native_borrow)!
	println('native_borrow support=${capability.support}')
	if capability.support == .unsupported {
		return
	}
	backend := app.capabilities().backend
	app.with_native_window(window, fn [backend] (mut lease gg.NativeWindowLease) ! {
		match backend {
			.win32 {
				lease.with_win32(fn (hwnd voidptr) ! {
					println('borrowed HWND=${usize(hwnd):x}')
				})!
			}
			.appkit {
				lease.with_appkit(fn (ns_window voidptr) ! {
					println('borrowed NSWindow=${usize(ns_window):x}')
				})!
			}
			.x11 {
				lease.with_x11(fn (display voidptr, xwindow u64) ! {
					println('borrowed Display=${usize(display):x} Window=${xwindow:x}')
				})!
			}
			.wayland {
				lease.with_wayland(fn (display voidptr, surface voidptr) ! {
					println('borrowed wl_display=${usize(display):x} wl_surface=${usize(surface):x}')
				})!
			}
			.auto, .mock {}
		}
	})!
}

fn (state &ServiceExampleState) matches_clipboard(id gg.ClipboardRequestId) bool {
	expected := state.clipboard_request or { return false }
	return expected == id && !state.completed['clipboard']
}

fn (state &ServiceExampleState) matches_portal(id gg.PortalParentRequestId) bool {
	expected := state.portal_request or { return false }
	return expected == id && !state.completed['portal']
}

fn (state &ServiceExampleState) matches_readback(id gg.WindowReadbackId) bool {
	expected := state.readback_request or { return false }
	return expected == id && !state.completed['readback']
}

fn release_portal_parent_bounded(mut app gg.App, lease gg.PortalParentLeaseId) {
	app.release_portal_parent(lease) or {
		if err.msg().contains('service request is stale') || err.msg().contains('app is stopped') {
			eprintln('portal-parent lease was already invalidated by teardown: ${err.msg()}')
		} else {
			eprintln('portal-parent lease release failed: ${err.msg()}')
		}
	}
}

fn (mut state ServiceExampleState) complete_one(kind string) bool {
	if state.completed[kind] {
		return false
	}
	state.completed[kind] = true
	if state.pending > 0 {
		state.pending--
	}
	return state.mock && state.pending == 0
}

fn stop_example_if_complete(mut app gg.App, complete bool) {
	if complete {
		app.stop() or {
			eprintln('example stop failed after acknowledged terminal event: ${err.msg()}')
		}
	}
}
