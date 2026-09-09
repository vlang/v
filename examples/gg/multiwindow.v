// vtest build: gg_multiwindow?
// Run with `v -d gg_multiwindow run examples/gg/multiwindow.v`.
// For Linux X11/Xvfb native rendering, add `-d x_multiwindow_x11`.
// For Linux Wayland native rendering, add `-d sokol_wayland`.
// Without enabled native display support, `backend: .auto` can fall back to `.mock`.
// Without `-d gg_multiwindow`, the source still compiles and reports the opt-in error.
// CI also compiles platform render variants with `-d x_multiwindow_x11`,
// `-d sokol_wayland`, `-d sokol_metal` on macOS, and `-d sokol_d3d11` on Windows.
module main

import gg
import os
import sokol.gfx
import time

const unattended_environment = 'VGG_MULTIWINDOW_EXAMPLE_UNATTENDED'
const unattended_render_timeout = 10 * time.second
const visible_last_event_limit = 4
const visible_title_limit = 220
const visual_margin = 18
const visual_badge_size = 16
const visual_counter_height = 8
const dashboard_control_height = 24
const dashboard_control_gap = 6
const dashboard_control_text_scale = 1
const dashboard_control_horizontal_padding = 7
const client_chrome_titlebar_height = 36
const client_chrome_frame_thickness = 5
const client_chrome_resize_margin = 18
const client_chrome_close_button_size = 18
const client_chrome_close_button_margin = 9
const client_chrome_control_gap = 7
const client_chrome_title_text_scale = 2

enum DashboardControl {
	inactive
	new_window
	close
	focus
	resize
	show_hide
}

struct DashboardControlRect {
	control DashboardControl
	label   string
	x       int
	y       int
	width   int
	height  int
}

struct WindowDashboard {
	id gg.WindowId
mut:
	label              string
	live               bool
	width              int
	height             int
	created            bool
	lifecycle          int
	inputs             int
	key                int
	text               int
	mouse              int
	scroll             int
	focus              int
	drop               int
	touch              int
	clipboard          int
	window             int
	other              int
	native_decorations bool
	last_events        []string
	last_families      []string
}

struct EventState {
	mock        bool
	unattended  bool
	input_limit int = 12
mut:
	caps                  gg.Capabilities
	resized               int
	printed_inputs        int
	unattended_close_sent bool
	hovered_window        string
	hovered_control       DashboardControl
	pressed_window        string
	pressed_control       DashboardControl
	last_action           string = 'READY'
	last_action_target    string = 'DASHBOARD'
	last_action_result    string = 'WAITING'
	last_action_detail    string
	next_window_number    int = 3
	action_attempts       int
	hidden_windows        map[string]bool
	last_created_window   gg.WindowId
	has_last_created      bool
	render_signaled       map[string]bool
	windows               []WindowDashboard
}

fn main() {
	run_example() or {
		eprintln('multi-window example failed: ${err.msg()}')
		exit(1)
	}
}

fn new_app_prefer_renderer() !&gg.App {
	return gg.new_app(require_renderer: true) or {
		println('render dashboard fallback: require_renderer unavailable')
		return gg.new_app()!
	}
}

fn requested_unattended_backend() !string {
	backend := os.getenv(unattended_environment)
	if backend == '' {
		return ''
	}
	if backend !in ['mock', 'x11', 'wayland', 'appkit', 'win32'] {
		return error('${unattended_environment} must select mock, x11, wayland, appkit, or win32')
	}
	return backend
}

fn run_example() ! {
	mut app := new_app_prefer_renderer()!
	mut failures := []string{}
	mut cleanup_needed := false
	run_example_session(mut app) or {
		failures << err.msg()
		cleanup_needed = true
	}
	live_windows := app.window_ids() or {
		failures << 'cleanup inspection failed: ${err.msg()}'
		cleanup_needed = true
		[]gg.WindowId{}
	}
	if live_windows.len > 0 {
		cleanup_needed = true
	}
	if cleanup_needed {
		app.stop() or { failures << 'cleanup failed: ${err.msg()}' }
	}
	remaining_windows := app.window_ids() or {
		failures << 'cleanup validation failed: ${err.msg()}'
		[]gg.WindowId{}
	}
	if remaining_windows.len > 0 {
		failures << 'cleanup incomplete: ${remaining_windows.len} live window(s) remain'
	}
	if failures.len > 0 {
		return error(failures.join('; '))
	}
	println('{"example":"multiwindow","status":"PASS","cleanup":"complete"}')
}

fn run_example_session(mut app gg.App) ! {
	unattended_backend := requested_unattended_backend()!
	mut caps := app.capabilities()
	if unattended_backend != '' && '${caps.backend}' != unattended_backend {
		return error('unattended multi-window example expected backend `${unattended_backend}`, got `${caps.backend}`')
	}
	println('gg multi-window backend: ${caps.backend}')
	if unattended_backend != '' {
		println('unattended lifecycle enabled for backend: ${unattended_backend}')
	}
	print_capability_families(caps)
	if caps.mock {
		println('render dashboard fallback: explicit swapchain unavailable')
		println('mock backend selected; stopping after initial lifecycle events')
	} else if caps.explicit_swapchain {
		println('render dashboard enabled: explicit swapchain available')
		println('visual dashboards enabled in each window; titles mirror exact state and counters')
	} else {
		println('render dashboard fallback: explicit swapchain unavailable')
		println('renderer unavailable; titles show per-window state, backend, capabilities, last events, and counters')
	}
	if !caps.mock {
		println('input logging enabled for key/mouse/focus/scroll/drop/touch events; first 12 events will be printed')
	}

	mut state := &EventState{
		mock:       caps.mock
		unattended: unattended_backend != ''
		caps:       caps
	}
	frames := chan string{cap: 2}
	main_window := if caps.explicit_swapchain {
		app.create_window(
			title:       'GG Multi-Window'
			width:       640
			height:      360
			redraw_mode: .continuous
			frame_fn:    fn [frames, mut state] (mut window gg.WindowContext) ! {
				state.draw_window(mut window, frames)!
			}
		)!
	} else {
		app.create_window(
			title:  'GG Multi-Window'
			width:  640
			height: 360
		)!
	}
	tools_window := if caps.explicit_swapchain {
		app.create_window(
			title:       'Tools'
			width:       320
			height:      240
			redraw_mode: .continuous
			frame_fn:    fn [frames, mut state] (mut window gg.WindowContext) ! {
				state.draw_window(mut window, frames)!
			}
		)!
	} else {
		app.create_window(
			title:  'Tools'
			width:  320
			height: 240
		)!
	}

	app.set_window_title(tools_window, 'Tools - updated')!
	resize_or_ignore_unsupported(mut app, main_window, 720, 420)!
	resize_or_ignore_unsupported(mut app, tools_window, 360, 260)!

	state.track_window(main_window, 'Main', 720, 420)
	state.track_window(tools_window, 'Tools', 360, 260)
	state.sync_runtime(mut app)!
	caps = state.caps
	print_interactive_chrome_help(state.caps, state.has_client_chrome())
	state.update_all_titles(mut app)!
	if state.unattended && !caps.explicit_swapchain {
		verify_dashboard_logical_layout()!
		state.exercise_unattended_controls(mut app, frames, main_window, tools_window)!
		state.sync_runtime(mut app)!
		state.update_all_titles(mut app)!
	}

	println('live windows:')
	for info in app.window_infos()! {
		println('  ${info.title}: ${info.width}x${info.height} native_decorations=${info.native_decorations}')
	}

	if state.unattended && caps.explicit_swapchain {
		verify_dashboard_logical_layout()!
		driver_result := chan string{cap: 1}
		action_result := chan string{cap: 1}
		action_job := fn [frames, mut state, main_window, tools_window, action_result] (mut queued_app gg.App) ! {
			state.exercise_unattended_controls(mut queued_app, frames, main_window, tools_window) or {
				action_result <- err.msg()
				return
			}
			action_result <- ''
		}
		driver := spawn drive_unattended_render(mut app, frames, [
			main_window.str(),
			tools_window.str(),
		], tools_window.str(), action_job, action_result, driver_result)
		mut run_error := ''
		run_example_event_loop(mut app, mut state, frames) or { run_error = err.msg() }
		driver.wait()
		driver_error := <-driver_result
		if run_error != '' {
			return error('unattended render owner loop: ${run_error}')
		}
		if driver_error != '' {
			return error('unattended render driver: ${driver_error}')
		}
		return
	}
	run_example_event_loop(mut app, mut state, frames)!
}

fn run_example_event_loop(mut app gg.App, mut state EventState, frames chan string) ! {
	app.run(
		event_fn: fn [mut state] (event gg.WindowEvent, mut app gg.App) ! {
			handle_window_event(event, mut app, mut state)!
		}
		input_fn: fn [frames, mut state] (event gg.WindowInputEvent, mut app gg.App) ! {
			handle_input_event(event, mut app, mut state, frames)!
		}
	)!
}

fn drive_unattended_render(mut app gg.App, frames chan string, expected []string, remapped_window string, action_job gg.AppJobFn, action_result chan string, result chan string) {
	mut seen := map[string]bool{}
	for seen.len < expected.len {
		select {
			window := <-frames {
				if window in expected {
					seen[window] = true
				}
			}
			unattended_render_timeout {
				post_unattended_render_stop(mut app) or {
					result <- ('render barrier timeout; stop admission failed: ' + err.msg())
					return
				}
				result <- ('render barrier timeout with ' + seen.len.str() + '/' +
					expected.len.str() + ' windows rendered')
				return
			}
		}
	}
	app.post(action_job) or {
		result <- ('dashboard action admission failed: ' + err.msg())
		return
	}
	select {
		action_error := <-action_result {
			if action_error != '' {
				post_unattended_render_stop(mut app) or {}
				result <- ('dashboard actions failed: ' + action_error)
				return
			}
		}
		unattended_render_timeout {
			post_unattended_render_stop(mut app) or {}
			result <- 'dashboard action timeout'
			return
		}
	}
	for {
		select {
			window := <-frames {
				if window == remapped_window {
					break
				}
			}
			unattended_render_timeout {
				post_unattended_render_stop(mut app) or {}
				result <- 'shown window did not render after remap'
				return
			}
		}
	}
	post_unattended_render_stop(mut app) or {
		result <- ('stop admission failed: ' + err.msg())
		return
	}
	result <- ''
}

fn post_unattended_render_stop(mut app gg.App) ! {
	app.post(fn (mut app gg.App) ! {
		app.stop()!
	})!
}

fn print_capability_families(caps gg.Capabilities) {
	println('capability families:')
	println('  windows=${caps.multi_window} owner_queue=${caps.owner_queue} native=${caps.native}')
	println('  render explicit_swapchain=${caps.explicit_swapchain} gl=${caps.gl} metal=${caps.metal} d3d11=${caps.d3d11}')
	println('  input=${caps.input_events} mouse=${caps.mouse_events} keyboard=${caps.keyboard_events} text=${caps.text_events} focus=${caps.focus_events} drop=${caps.drop_events} touch=${caps.touch_events}')
	println('  chrome interactive_move_resize=${caps.interactive_move_resize} native_decorations=${caps.native_decorations} cursor_shapes=${caps.cursor_shapes}')
}

fn print_interactive_chrome_help(caps gg.Capabilities, has_client_chrome bool) {
	println('application controls: NEW, CLOSE, FOCUS, RESIZE, and SHOW-HIDE; hover and click inside any dashboard')
	if has_client_chrome {
		println('Wayland client-side titlebar/frame enabled as fallback: drag the titlebar to move; drag frame edges/corners to resize; click the close button')
		println('move/resize is started from gg.App input_fn using the current native user-action serial')
		return
	}
	if caps.interactive_move_resize && caps.native_decorations {
		println('interactive move/resize available through native decorations; client chrome demo disabled')
		return
	}
	println('interactive move/resize unavailable for this backend; application dashboard controls remain available')
}

fn handle_window_event(event gg.WindowEvent, mut app gg.App, mut state EventState) ! {
	state.note_lifecycle(event)
	state.update_window_title(mut app, event.window)!
	match event.kind {
		.window_created {
			println('window created: ${event.window}')
			state.schedule_unattended_close(mut app)!
		}
		.window_resized {
			state.resized++
			println('window resized: ${event.window} -> ${event.width}x${event.height}')
			if state.mock && !state.unattended && state.resized >= 2 {
				app.stop()!
			}
		}
		.window_close_requested {
			println('window close requested: ${event.window}')
			if app.window_exists(event.window) {
				app.destroy_window(event.window)!
			}
		}
		.window_destroyed {
			println('window destroyed: ${event.window}')
			if app.window_ids()!.len == 0 {
				app.stop()!
			}
		}
	}
}

fn (mut state EventState) schedule_unattended_close(mut app gg.App) ! {
	if !state.unattended || state.caps.explicit_swapchain || state.unattended_close_sent {
		return
	}
	for dashboard in state.windows {
		if !dashboard.created {
			return
		}
	}
	state.unattended_close_sent = true
	app.post(fn (mut app gg.App) ! {
		for window in app.window_ids()! {
			app.destroy_window(window)!
		}
	})!
}

fn handle_input_event(event gg.WindowInputEvent, mut app gg.App, mut state EventState, frames chan string) ! {
	message := input_event_summary(event)
	if message == '' {
		return
	}
	state.note_input(event, message)
	state.sync_runtime(mut app) or {}
	native_decorations := state.window_native_decorations(event.window)
	hovered_control := state.update_dashboard_control_hover(event, native_decorations)
	update_dashboard_cursor(event, mut app, state.caps, native_decorations, hovered_control) or {}
	if !state.unattended && hovered_control != .inactive && event.event.typ == .mouse_down
		&& event.event.mouse_button == .left
		&& state.admit_dashboard_control_press(event.window, hovered_control) {
		state.perform_dashboard_control(hovered_control, event.window, mut app, frames)
		// Dashboard action failures are reflected in persistent status. Nothing fallible
		// is propagated after the action, so callback replay cannot repeat the effect.
		state.sync_runtime(mut app) or {}
		state.update_all_titles(mut app) or {}
		state.print_input(message)
		return
	}
	action_message := maybe_begin_client_chrome_action(event, mut app, state.caps,
		native_decorations) or { 'interactive chrome action failed: ${err.msg()}' }
	if action_message != '' {
		println(action_message)
		state.note_chrome_action(event.window, action_message)
	}
	state.update_window_title(mut app, event.window)!
	state.print_input(message)
}

fn (mut state EventState) admit_dashboard_control_press(window gg.WindowId, control DashboardControl) bool {
	key := window.str()
	if state.pressed_window == key && state.pressed_control == control {
		return false
	}
	state.pressed_window = key
	state.pressed_control = control
	return true
}

fn (mut state EventState) print_input(message string) {
	if state.printed_inputs >= state.input_limit {
		return
	}
	state.printed_inputs++
	println('input ${state.printed_inputs}/${state.input_limit}: ${message}')
}

fn (mut state EventState) exercise_unattended_controls(mut app gg.App, frames chan string, main_window gg.WindowId, tools_window gg.WindowId) ! {
	start_attempts := state.action_attempts
	state.perform_dashboard_control(.focus, main_window, mut app, frames)
	state.perform_dashboard_control(.resize, main_window, mut app, frames)
	state.perform_dashboard_control(.show_hide, main_window, mut app, frames)
	if state.last_action_result != 'OK' {
		return error('unattended HIDE control failed: ${state.last_action_detail}')
	}
	state.render_signaled[tools_window.str()] = false
	state.perform_dashboard_control(.show_hide, main_window, mut app, frames)
	if state.last_action_result != 'OK' {
		return error('unattended SHOW control failed: ${state.last_action_detail}')
	}
	state.perform_dashboard_control(.new_window, tools_window, mut app, frames)
	if !state.has_last_created || !app.window_exists(state.last_created_window) {
		return error('unattended NEW control did not create a live window')
	}
	state.perform_dashboard_control(.close, state.last_created_window, mut app, frames)
	if state.last_action_result != 'OK' {
		return error('unattended CLOSE control was not admitted')
	}
	if state.action_attempts != start_attempts + 6 {
		return error('unattended dashboard control count mismatch: expected=6 actual=${state.action_attempts - start_attempts}')
	}
	println('MULTIWINDOW_EXAMPLE_CONTROLS actions=6 focus=attempted resize=attempted hide=attempted show=attempted new=attempted close=attempted')
}

fn (mut state EventState) perform_dashboard_control(control DashboardControl, source gg.WindowId, mut app gg.App, frames chan string) {
	match control {
		.new_window {
			label := 'Window ${state.next_window_number}'
			mut created := gg.WindowId{}
			if state.caps.explicit_swapchain {
				created = app.create_window(
					title:       label
					width:       480
					height:      300
					redraw_mode: .continuous
					frame_fn:    fn [frames, mut state] (mut window gg.WindowContext) ! {
						state.draw_window(mut window, frames)!
					}
				) or {
					state.record_dashboard_action('NEW', label, false, err.msg())
					return
				}
			} else {
				created = app.create_window(
					title:  label
					width:  480
					height: 300
				) or {
					state.record_dashboard_action('NEW', label, false, err.msg())
					return
				}
			}
			state.next_window_number++
			state.track_window(created, label, 480, 300)
			state.last_created_window = created
			state.has_last_created = true
			state.record_dashboard_action('NEW', label, true, created.str())
		}
		.close {
			target := state.window_label(source)
			app.destroy_window(source) or {
				state.record_dashboard_action('CLOSE', target, false, err.msg())
				return
			}
			state.hidden_windows.delete(source.str())
			state.record_dashboard_action('CLOSE', target, true, 'destroy admitted')
		}
		.focus {
			target := state.window_label(source)
			app.request_window_focus(source) or {
				state.record_dashboard_action('FOCUS', target, false, err.msg())
				return
			}
			state.record_dashboard_action('FOCUS', target, true, 'focus requested')
		}
		.resize {
			target := state.window_label(source)
			mut width := 640
			mut height := 360
			if index := state.window_index(source) {
				dashboard := state.windows[index]
				width = if dashboard.width >= 760 { 560 } else { dashboard.width + 80 }
				height = if dashboard.height >= 500 { 320 } else { dashboard.height + 50 }
			}
			app.resize_window(source, width, height) or {
				state.record_dashboard_action('RESIZE', target, false, err.msg())
				return
			}
			state.record_dashboard_action('RESIZE', target, true, '${width}x${height}')
		}
		.show_hide {
			target_id := state.show_hide_target(source, &app) or {
				state.record_dashboard_action('SHOW-HIDE', state.window_label(source), false,
					'no live target')
				return
			}
			target := state.window_label(target_id)
			key := target_id.str()
			if state.hidden_windows[key] {
				app.show_window(target_id) or {
					state.record_dashboard_action('SHOW', target, false, err.msg())
					return
				}
				state.hidden_windows[key] = false
				state.record_dashboard_action('SHOW', target, true, 'show requested')
			} else {
				app.hide_window(target_id) or {
					state.record_dashboard_action('HIDE', target, false, err.msg())
					return
				}
				state.hidden_windows[key] = true
				state.record_dashboard_action('HIDE', target, true, 'hide requested')
			}
		}
		.inactive {}
	}
}

fn (state &EventState) show_hide_target(source gg.WindowId, app &gg.App) ?gg.WindowId {
	for dashboard in state.windows {
		if dashboard.id.str() != source.str() && app.window_exists(dashboard.id) {
			return dashboard.id
		}
	}
	if app.window_exists(source) {
		return source
	}
	return none
}

fn (state &EventState) window_label(id gg.WindowId) string {
	if index := state.window_index(id) {
		return state.windows[index].label
	}
	return id.str()
}

fn (mut state EventState) record_dashboard_action(action string, target string, success bool, detail string) {
	state.action_attempts++
	state.last_action = action
	state.last_action_target = target
	state.last_action_result = if success { 'OK' } else { 'ERROR' }
	state.last_action_detail = detail
	println('dashboard control action=${action} target=${target} result=${state.last_action_result} detail=${detail}')
}

fn (mut state EventState) track_window(id gg.WindowId, label string, width int, height int) {
	if index := state.window_index(id) {
		mut dashboard := state.windows[index]
		dashboard.label = label
		dashboard.live = true
		dashboard.width = width
		dashboard.height = height
		state.windows[index] = dashboard
		return
	}
	state.windows << WindowDashboard{
		id:     id
		label:  label
		live:   true
		width:  width
		height: height
	}
}

fn (state &EventState) window_index(id gg.WindowId) ?int {
	target := id.str()
	for i, dashboard in state.windows {
		if dashboard.id.str() == target {
			return i
		}
	}
	return none
}

fn (mut state EventState) sync_runtime(mut app gg.App) ! {
	state.caps = app.capabilities()
	for info in app.window_infos()! {
		index := state.ensure_window(info.id)
		mut dashboard := state.windows[index]
		dashboard.width = info.width
		dashboard.height = info.height
		dashboard.native_decorations = info.native_decorations
		state.windows[index] = dashboard
	}
}

fn (state &EventState) window_native_decorations(id gg.WindowId) bool {
	if index := state.window_index(id) {
		return state.windows[index].native_decorations
	}
	return state.caps.native_decorations
}

fn (state &EventState) has_client_chrome() bool {
	for dashboard in state.windows {
		if dashboard.live && client_chrome_enabled(state.caps, dashboard.native_decorations) {
			return true
		}
	}
	return false
}

fn (mut state EventState) ensure_window(id gg.WindowId) int {
	if index := state.window_index(id) {
		return index
	}
	state.windows << WindowDashboard{
		id:    id
		label: 'Window ${state.windows.len + 1}'
		live:  true
	}
	return state.windows.len - 1
}

fn (mut state EventState) note_lifecycle(event gg.WindowEvent) {
	index := state.ensure_window(event.window)
	mut dashboard := state.windows[index]
	dashboard.lifecycle++
	dashboard.window++
	match event.kind {
		.window_created {
			dashboard.live = true
			dashboard.created = true
			dashboard.width = event.width
			dashboard.height = event.height
			dashboard.add_last_event('created ${event.width}x${event.height}', 'window')
		}
		.window_resized {
			dashboard.live = true
			dashboard.width = event.width
			dashboard.height = event.height
			dashboard.add_last_event('resized ${event.width}x${event.height}', 'window')
		}
		.window_close_requested {
			dashboard.add_last_event('close requested', 'window')
		}
		.window_destroyed {
			dashboard.live = false
			dashboard.add_last_event('destroyed', 'window')
			state.hidden_windows.delete(event.window.str())
		}
	}

	state.windows[index] = dashboard
}

fn (mut state EventState) note_input(event gg.WindowInputEvent, message string) {
	index := state.ensure_window(event.window)
	mut dashboard := state.windows[index]
	dashboard.inputs++
	family := input_event_family(event.event)
	match family {
		'key' { dashboard.key++ }
		'text' { dashboard.text++ }
		'mouse' { dashboard.mouse++ }
		'scroll' { dashboard.scroll++ }
		'focus' { dashboard.focus++ }
		'drop' { dashboard.drop++ }
		'touch' { dashboard.touch++ }
		'clipboard' { dashboard.clipboard++ }
		'window' { dashboard.window++ }
		else { dashboard.other++ }
	}

	dashboard.add_last_event(short_event_message(event.window, message), family)
	state.windows[index] = dashboard
}

fn (mut state EventState) note_chrome_action(id gg.WindowId, message string) {
	index := state.ensure_window(id)
	mut dashboard := state.windows[index]
	dashboard.window++
	dashboard.add_last_event(short_event_message(id, message), 'window')
	state.windows[index] = dashboard
}

fn input_event_family(input gg.Event) string {
	return match input.typ {
		.key_down, .key_up { 'key' }
		.char { 'text' }
		.mouse_down, .mouse_up, .mouse_move, .mouse_enter, .mouse_leave { 'mouse' }
		.mouse_scroll { 'scroll' }
		.focused, .unfocused { 'focus' }
		.files_dropped { 'drop' }
		.touches_began, .touches_moved, .touches_ended, .touches_cancelled { 'touch' }
		.clipboard_pasted { 'clipboard' }
		.resized, .iconified, .restored, .suspended, .resumed, .quit_requested { 'window' }
		else { 'other' }
	}
}

fn (mut dashboard WindowDashboard) add_last_event(message string, family string) {
	dashboard.last_events << message
	dashboard.last_families << family
	for dashboard.last_events.len > visible_last_event_limit {
		dashboard.last_events.delete(0)
	}
	for dashboard.last_families.len > visible_last_event_limit {
		dashboard.last_families.delete(0)
	}
}

fn short_event_message(window gg.WindowId, message string) string {
	prefix := '${window}: '
	if message.starts_with(prefix) {
		return message[prefix.len..]
	}
	return message
}

fn (mut state EventState) update_all_titles(mut app gg.App) ! {
	for dashboard in state.windows {
		state.update_window_title(mut app, dashboard.id)!
	}
}

fn (mut state EventState) update_window_title(mut app gg.App, id gg.WindowId) ! {
	if !app.window_exists(id) {
		return
	}
	index := state.window_index(id) or { return }
	app.set_window_title(id, state.window_title(state.windows[index]))!
}

fn (state &EventState) window_title(dashboard WindowDashboard) string {
	live := if dashboard.live { 'live ${dashboard.width}x${dashboard.height}' } else { 'closed' }
	caps := compact_capabilities(state.caps, dashboard.native_decorations)
	chrome := state.chrome_title_hint(dashboard)
	last := if dashboard.last_events.len == 0 {
		'last: waiting'
	} else {
		'last: ' + dashboard.last_events.join(' | ')
	}
	counters := 'events l=${dashboard.lifecycle} in=${dashboard.inputs} key=${dashboard.key} text=${dashboard.text} mouse=${dashboard.mouse}+${dashboard.scroll} focus=${dashboard.focus} drop=${dashboard.drop} touch=${dashboard.touch} clip=${dashboard.clipboard} win=${dashboard.window} other=${dashboard.other}'
	action := 'action ${state.last_action}/${state.last_action_target}/${state.last_action_result}'
	return truncate_title('${dashboard.label} | ${state.caps.backend} ${caps} | ${chrome} | ${live} | ${action} | ${counters} | ${last}')
}

fn (state &EventState) chrome_title_hint(dashboard WindowDashboard) string {
	if client_chrome_enabled(state.caps, dashboard.native_decorations) {
		return 'chrome: Wayland client-side titlebar/frame fallback'
	}
	if state.caps.interactive_move_resize && dashboard.native_decorations {
		return 'chrome: native decorations'
	}
	return 'chrome: move/resize unavailable'
}

fn compact_capabilities(caps gg.Capabilities, native_decorations bool) string {
	mut tags := []string{}
	if caps.native {
		tags << 'native'
	} else {
		tags << 'mock'
	}
	if caps.explicit_swapchain {
		tags << 'render'
	}
	if caps.mouse_events {
		tags << 'mouse'
	}
	if caps.keyboard_events {
		tags << 'key'
	}
	if caps.text_events {
		tags << 'text'
	}
	if caps.focus_events {
		tags << 'focus'
	}
	if caps.drop_events {
		tags << 'drop'
	}
	if caps.touch_events {
		tags << 'touch'
	}
	if caps.interactive_move_resize {
		tags << 'move-resize'
	}
	if native_decorations {
		tags << 'native-decor'
	} else if caps.wayland && caps.interactive_move_resize {
		tags << 'client-chrome'
	}
	return '[' + tags.join(',') + ']'
}

fn truncate_title(title string) string {
	if title.len <= visible_title_limit {
		return title
	}
	return title[..visible_title_limit] + '...'
}

fn (mut state EventState) draw_window(mut window gg.WindowContext, frames chan string) ! {
	info := window.frame_info()
	id := info.window
	index := state.window_index(id) or {
		return error('missing dashboard state for render window ${id}')
	}
	dashboard := state.windows[index]
	if !dashboard.live {
		return
	}
	width := max_int(1, int(info.metrics.logical_size.width))
	height := max_int(1, int(info.metrics.logical_size.height))
	caps := state.caps
	hovered_control := if state.hovered_window == id.str() {
		state.hovered_control
	} else {
		DashboardControl.inactive
	}
	last_action := state.last_action
	last_action_target := state.last_action_target
	last_action_result := state.last_action_result
	background := dashboard_background(caps)
	action := gfx.create_clear_pass_action(background.r, background.g, background.b, background.a)
	window.with_swapchain_sgl(action, fn [dashboard, caps, width, height, hovered_control, last_action, last_action_target, last_action_result] (mut drawing gg.WindowSglContext) ! {
		drawing.defaults()
		drawing.matrix_mode_projection()
		drawing.load_identity()
		drawing.ortho(0, f32(width), f32(height), 0, -1, 1)
		draw_window_dashboard(mut drawing, dashboard, caps, width, height, hovered_control,
			last_action, last_action_target, last_action_result)
	})!
	key := id.str()
	if state.unattended && !state.render_signaled[key] {
		state.render_signaled[key] = true
		frames <- key
	}
}

fn draw_rect_filled(mut window gg.WindowSglContext, x f32, y f32, width f32, height f32, color gg.Color) {
	if width <= 0 || height <= 0 {
		return
	}
	window.c4b(color.r, color.g, color.b, color.a)
	window.begin_quads()
	window.v2f(x, y)
	window.v2f(x + width, y)
	window.v2f(x + width, y + height)
	window.v2f(x, y + height)
	window.end()
}

fn draw_rect_empty(mut window gg.WindowSglContext, x f32, y f32, width f32, height f32, color gg.Color) {
	if width <= 0 || height <= 0 {
		return
	}
	window.c4b(color.r, color.g, color.b, color.a)
	window.begin_lines()
	window.v2f(x, y)
	window.v2f(x + width, y)
	window.v2f(x + width, y)
	window.v2f(x + width, y + height)
	window.v2f(x + width, y + height)
	window.v2f(x, y + height)
	window.v2f(x, y + height)
	window.v2f(x, y)
	window.end()
}

fn draw_window_dashboard(mut window gg.WindowSglContext, dashboard WindowDashboard, caps gg.Capabilities, framebuffer_width int, framebuffer_height int, hovered_control DashboardControl, last_action string, last_action_target string, last_action_result string) {
	width := max_int(1, framebuffer_width)
	height := max_int(1, framebuffer_height)
	inner_width := max_int(64, width - 2 * visual_margin)
	client_chrome := client_chrome_enabled(caps, dashboard.native_decorations)
	content_top := if client_chrome {
		client_chrome_titlebar_height + client_chrome_frame_thickness
	} else {
		0
	}
	draw_rect_filled(mut window, 0, 0, f32(width), f32(height), dashboard_background(caps))
	draw_client_chrome_zones(mut window, dashboard, caps, width, height)
	if client_chrome {
		draw_rect_empty(mut window, f32(client_chrome_frame_thickness + 6), f32(content_top + 6), f32(
			width - 2 * client_chrome_frame_thickness - 12), f32(height - content_top -
			client_chrome_frame_thickness - 12), gg.rgb(88, 101, 118))
	} else {
		draw_rect_filled(mut window, 0, 0, f32(width), 10, backend_color(caps))
		draw_rect_empty(mut window, 8, 8, f32(width - 16), f32(height - 16), gg.rgb(96, 116, 142))
	}

	control_bottom := draw_dashboard_control_bar(mut window, width, content_top, hovered_control)
	status_color := if last_action_result == 'ERROR' {
		gg.rgb(242, 112, 112)
	} else {
		gg.rgb(182, 214, 232)
	}
	status_y := control_bottom + 8
	draw_tiny_text(mut window, 'LAST ${last_action} ${last_action_target} ${last_action_result}',
		visual_margin, status_y, inner_width, 1, status_color)
	badge_y := status_y + 18
	draw_capability_badges(mut window, caps, visual_margin, badge_y)
	draw_counter_bars(mut window, dashboard, visual_margin, badge_y + 30, inner_width)
	draw_last_event_strip(mut window, dashboard, visual_margin, height - 54, inner_width)
}

fn draw_dashboard_control_bar(mut window gg.WindowSglContext, width int, content_top int, hovered DashboardControl) int {
	mut bottom := content_top
	for control in dashboard_control_rects(width, content_top) {
		active := control.control == hovered
		fill := if active { gg.rgb(66, 139, 214) } else { gg.rgb(45, 57, 72) }
		border := if active { gg.rgb(224, 240, 255) } else { gg.rgb(112, 132, 154) }
		text_color := if active { gg.rgb(255, 255, 255) } else { gg.rgb(214, 224, 236) }
		draw_rect_filled(mut window, f32(control.x), f32(control.y), f32(control.width),
			f32(control.height), fill)
		draw_rect_empty(mut window, f32(control.x), f32(control.y), f32(control.width),
			f32(control.height), border)
		text_width := control.label.len * 6 * dashboard_control_text_scale
		text_x := control.x + max_int(0, (control.width - text_width) / 2)
		text_y := control.y + (control.height - 7 * dashboard_control_text_scale) / 2
		draw_tiny_text(mut window, control.label, text_x, text_y, control.width - 4,
			dashboard_control_text_scale, text_color)
		bottom = max_int(bottom, control.y + control.height)
	}
	return bottom
}

fn dashboard_control_label(control DashboardControl) string {
	return match control {
		.new_window { 'NEW' }
		.close { 'CLOSE' }
		.focus { 'FOCUS' }
		.resize { 'RESIZE' }
		.show_hide { 'SHOW-HIDE' }
		.inactive { '' }
	}
}

fn dashboard_control_rects(width int, content_top int) []DashboardControlRect {
	controls := [DashboardControl.new_window, .close, .focus, .resize, .show_hide]
	mut rects := []DashboardControlRect{cap: controls.len}
	mut x := visual_margin
	mut y := content_top + 14
	for control in controls {
		label := dashboard_control_label(control)
		button_width := max_int(32, label.len * 6 * dashboard_control_text_scale +
			2 * dashboard_control_horizontal_padding)
		if x > visual_margin && x + button_width > width - visual_margin {
			x = visual_margin
			y += dashboard_control_height + dashboard_control_gap
		}
		rects << DashboardControlRect{
			control: control
			label:   label
			x:       x
			y:       y
			width:   button_width
			height:  dashboard_control_height
		}
		x += button_width + dashboard_control_gap
	}
	return rects
}

fn dashboard_control_at(x f32, y f32, width int, content_top int) DashboardControl {
	for control in dashboard_control_rects(width, content_top) {
		if x >= f32(control.x) && x <= f32(control.x + control.width) && y >= f32(control.y)
			&& y <= f32(control.y + control.height) {
			return control.control
		}
	}
	return .inactive
}

fn verify_dashboard_logical_layout() ! {
	baseline := dashboard_control_rects(360, client_chrome_titlebar_height +
		client_chrome_frame_thickness)
	for dpi_scale in [f32(1), 2] {
		framebuffer_width := int(360 * dpi_scale)
		logical_width := int(f32(framebuffer_width) / dpi_scale)
		rects := dashboard_control_rects(logical_width, client_chrome_titlebar_height +
			client_chrome_frame_thickness)
		if rects != baseline {
			return error('dashboard logical layout changed at dpi scale ${dpi_scale}')
		}
		for rect in rects {
			control := dashboard_control_at(f32(rect.x + rect.width / 2), f32(rect.y +
				rect.height / 2), logical_width, client_chrome_titlebar_height +
				client_chrome_frame_thickness)
			if control != rect.control {
				return error('dashboard hit-test mismatch at dpi scale ${dpi_scale}')
			}
		}
	}
}

fn (mut state EventState) update_dashboard_control_hover(event gg.WindowInputEvent, native_decorations bool) DashboardControl {
	input := event.event
	if input.typ == .mouse_leave {
		if state.hovered_window == event.window.str() {
			state.hovered_window = ''
			state.hovered_control = .inactive
		}
		if state.pressed_window == event.window.str() {
			state.pressed_window = ''
			state.pressed_control = .inactive
		}
		return .inactive
	}
	if input.typ == .mouse_up && state.pressed_window == event.window.str() {
		state.pressed_window = ''
		state.pressed_control = .inactive
	}
	if input.typ !in [.mouse_enter, .mouse_move, .mouse_down, .mouse_up] {
		if state.hovered_window == event.window.str() {
			return state.hovered_control
		}
		return .inactive
	}
	content_top := if client_chrome_enabled(state.caps, native_decorations) {
		client_chrome_titlebar_height + client_chrome_frame_thickness
	} else {
		0
	}
	control := dashboard_control_at(input.mouse_x, input.mouse_y, input.window_width, content_top)
	state.hovered_window = event.window.str()
	state.hovered_control = control
	return control
}

fn draw_client_chrome_zones(mut window gg.WindowSglContext, dashboard WindowDashboard, caps gg.Capabilities, width int, height int) {
	if !client_chrome_enabled(caps, dashboard.native_decorations) {
		return
	}
	titlebar := client_chrome_titlebar_height
	frame := client_chrome_frame_thickness
	inner_width := max_int(0, width - 2 * frame)
	inner_height := max_int(0, height - titlebar - frame)
	draw_rect_filled(mut window, 0, 0, f32(width), f32(height), gg.rgb(44, 50, 60))
	draw_rect_filled(mut window, f32(frame), f32(titlebar), f32(inner_width), f32(inner_height),
		dashboard_background(caps))
	draw_rect_filled(mut window, f32(frame), f32(frame), f32(inner_width), f32(titlebar - frame), gg.rgb(48,
		56, 68))
	draw_rect_filled(mut window, f32(frame), f32(frame), f32(inner_width), 1, gg.rgb(89, 101, 118))
	draw_rect_filled(mut window, f32(frame), f32(titlebar - 1), f32(inner_width), 1, gg.rgb(24, 30,
		38))
	draw_rect_empty(mut window, 0, 0, f32(width), f32(height), gg.rgb(20, 25, 32))
	draw_rect_empty(mut window, 1, 1, f32(width - 2), f32(height - 2), gg.rgb(82, 94, 110))
	draw_client_chrome_title(mut window, dashboard.label, width)
	draw_client_chrome_titlebar_separators(mut window, width)
	draw_client_chrome_minimize_button(mut window, width)
	draw_client_chrome_maximize_button(mut window, width)
	draw_client_chrome_close_button(mut window, width)
}

fn draw_client_chrome_minimize_button(mut window gg.WindowSglContext, width int) {
	x := window_control_button_x(width, 2)
	y := close_button_y()
	size := f32(client_chrome_close_button_size)
	draw_client_chrome_control_button(mut window, x, y, gg.rgb(68, 78, 92), gg.rgb(126, 140, 158))
	draw_rect_filled(mut window, x + 5, y + size - 6, size - 10, 2, gg.rgb(218, 224, 232))
}

fn draw_client_chrome_maximize_button(mut window gg.WindowSglContext, width int) {
	x := window_control_button_x(width, 1)
	y := close_button_y()
	draw_client_chrome_control_button(mut window, x, y, gg.rgb(68, 78, 92), gg.rgb(126, 140, 158))
	draw_rect_empty(mut window, x + 5, y + 5, 8, 8, gg.rgb(218, 224, 232))
	draw_rect_filled(mut window, x + 6, y + 5, 6, 1, gg.rgb(218, 224, 232))
}

fn draw_client_chrome_close_button(mut window gg.WindowSglContext, width int) {
	x := close_button_x(width)
	y := close_button_y()
	size := f32(client_chrome_close_button_size)
	draw_client_chrome_control_button(mut window, x, y, gg.rgb(178, 71, 72), gg.rgb(234, 150, 145))
	for offset in 0 .. 5 {
		step := f32(offset * 2)
		draw_rect_filled(mut window, x + 5 + step, y + 5 + step, 2, 2, gg.rgb(250, 236, 232))
		draw_rect_filled(mut window, x + 5 + step, y + size - 7 - step, 2, 2, gg.rgb(250, 236, 232))
	}
}

fn draw_client_chrome_control_button(mut window gg.WindowSglContext, x f32, y f32, fill gg.Color, border gg.Color) {
	size := f32(client_chrome_close_button_size)
	draw_rect_filled(mut window, x, y, size, size, fill)
	draw_rect_empty(mut window, x, y, size, size, border)
}

fn draw_client_chrome_titlebar_separators(mut window gg.WindowSglContext, width int) {
	frame := client_chrome_frame_thickness
	inner_width := max_int(0, width - 2 * frame)
	titlebar := client_chrome_titlebar_height
	separator_x := int(window_control_button_x(width, 2)) - client_chrome_control_gap
	draw_rect_filled(mut window, f32(frame), f32(titlebar), f32(inner_width), 1, gg.rgb(14, 18, 24))
	draw_rect_filled(mut window, f32(frame), f32(titlebar + 1), f32(inner_width), 1, gg.rgb(77, 88,
		104))
	if separator_x > frame + 80 {
		draw_rect_filled(mut window, f32(separator_x), f32(frame + 6), 1,
			f32(titlebar - frame - 12), gg.rgb(28, 34, 43))
		draw_rect_filled(mut window, f32(separator_x + 1), f32(frame + 6), 1, f32(titlebar - frame -
			12), gg.rgb(83, 94, 110))
	}
}

fn draw_client_chrome_title(mut window gg.WindowSglContext, title string, width int) {
	x := client_chrome_frame_thickness + 14
	y := (client_chrome_titlebar_height - 7 * client_chrome_title_text_scale) / 2
	max_width := int(window_control_button_x(width, 2)) - x - 18
	draw_tiny_text(mut window, title, x, y, max_width, client_chrome_title_text_scale, gg.rgb(230,
		235, 240))
}

fn draw_tiny_text(mut window gg.WindowSglContext, text string, x int, y int, max_width int, scale int, color gg.Color) {
	if max_width <= 0 || scale <= 0 {
		return
	}
	upper := text.to_upper()
	mut cursor_x := x
	for i in 0 .. upper.len {
		if cursor_x + 5 * scale > x + max_width {
			return
		}
		ch := upper[i]
		if ch == ` ` {
			cursor_x += 4 * scale
			continue
		}
		glyph := tiny_title_glyph(ch)
		for row, bits in glyph {
			for col in 0 .. bits.len {
				if bits[col] == `1` {
					draw_rect_filled(mut window, f32(cursor_x + col * scale), f32(y + row * scale),
						f32(scale), f32(scale), color)
				}
			}
		}
		cursor_x += 6 * scale
	}
}

fn tiny_title_glyph(ch u8) []string {
	return match ch {
		`A` { ['01110', '10001', '10001', '11111', '10001', '10001', '10001'] }
		`B` { ['11110', '10001', '10001', '11110', '10001', '10001', '11110'] }
		`C` { ['01111', '10000', '10000', '10000', '10000', '10000', '01111'] }
		`D` { ['11110', '10001', '10001', '10001', '10001', '10001', '11110'] }
		`E` { ['11111', '10000', '10000', '11110', '10000', '10000', '11111'] }
		`F` { ['11111', '10000', '10000', '11110', '10000', '10000', '10000'] }
		`G` { ['01111', '10000', '10000', '10011', '10001', '10001', '01110'] }
		`H` { ['10001', '10001', '10001', '11111', '10001', '10001', '10001'] }
		`I` { ['11111', '00100', '00100', '00100', '00100', '00100', '11111'] }
		`J` { ['00111', '00010', '00010', '00010', '10010', '10010', '01100'] }
		`K` { ['10001', '10010', '10100', '11000', '10100', '10010', '10001'] }
		`L` { ['10000', '10000', '10000', '10000', '10000', '10000', '11111'] }
		`M` { ['10001', '11011', '10101', '10101', '10001', '10001', '10001'] }
		`N` { ['10001', '11001', '10101', '10011', '10001', '10001', '10001'] }
		`O` { ['01110', '10001', '10001', '10001', '10001', '10001', '01110'] }
		`P` { ['11110', '10001', '10001', '11110', '10000', '10000', '10000'] }
		`Q` { ['01110', '10001', '10001', '10001', '10101', '10010', '01101'] }
		`R` { ['11110', '10001', '10001', '11110', '10100', '10010', '10001'] }
		`S` { ['01111', '10000', '10000', '01110', '00001', '00001', '11110'] }
		`T` { ['11111', '00100', '00100', '00100', '00100', '00100', '00100'] }
		`U` { ['10001', '10001', '10001', '10001', '10001', '10001', '01110'] }
		`V` { ['10001', '10001', '10001', '10001', '01010', '01010', '00100'] }
		`W` { ['10001', '10001', '10001', '10101', '10101', '10101', '01010'] }
		`X` { ['10001', '01010', '00100', '00100', '00100', '01010', '10001'] }
		`Y` { ['10001', '01010', '00100', '00100', '00100', '00100', '00100'] }
		`Z` { ['11111', '00001', '00010', '00100', '01000', '10000', '11111'] }
		`0` { ['01110', '10001', '10011', '10101', '11001', '10001', '01110'] }
		`1` { ['00100', '01100', '00100', '00100', '00100', '00100', '01110'] }
		`2` { ['01110', '10001', '00001', '00010', '00100', '01000', '11111'] }
		`3` { ['11110', '00001', '00001', '01110', '00001', '00001', '11110'] }
		`4` { ['10010', '10010', '10010', '11111', '00010', '00010', '00010'] }
		`5` { ['11111', '10000', '10000', '11110', '00001', '00001', '11110'] }
		`6` { ['01110', '10000', '10000', '11110', '10001', '10001', '01110'] }
		`7` { ['11111', '00001', '00010', '00100', '01000', '01000', '01000'] }
		`8` { ['01110', '10001', '10001', '01110', '10001', '10001', '01110'] }
		`9` { ['01110', '10001', '10001', '01111', '00001', '00001', '01110'] }
		`-` { ['00000', '00000', '00000', '01110', '00000', '00000', '00000'] }
		else { ['11111', '00001', '00010', '00100', '00100', '00000', '00100'] }
	}
}

fn draw_capability_badges(mut window gg.WindowSglContext, caps gg.Capabilities, x int, y int) {
	mut badge_x := x
	draw_capability_badge(mut window, badge_x, y, caps.native, gg.rgb(46, 180, 125))
	badge_x += visual_badge_size + 8
	draw_capability_badge(mut window, badge_x, y, caps.explicit_swapchain, gg.rgb(86, 160, 255))
	badge_x += visual_badge_size + 8
	draw_capability_badge(mut window, badge_x, y, caps.mouse_events, gg.rgb(244, 172, 68))
	badge_x += visual_badge_size + 8
	draw_capability_badge(mut window, badge_x, y, caps.keyboard_events, gg.rgb(238, 98, 98))
	badge_x += visual_badge_size + 8
	draw_capability_badge(mut window, badge_x, y, caps.text_events, gg.rgb(184, 116, 255))
	badge_x += visual_badge_size + 8
	draw_capability_badge(mut window, badge_x, y, caps.focus_events, gg.rgb(94, 217, 236))
	badge_x += visual_badge_size + 8
	draw_capability_badge(mut window, badge_x, y, caps.drop_events, gg.rgb(238, 214, 92))
	badge_x += visual_badge_size + 8
	draw_capability_badge(mut window, badge_x, y, caps.touch_events, gg.rgb(255, 137, 196))
}

fn draw_capability_badge(mut window gg.WindowSglContext, x int, y int, enabled bool, color gg.Color) {
	fill := if enabled { color } else { gg.rgb(42, 48, 58) }
	border := if enabled { gg.rgb(220, 230, 240) } else { gg.rgb(80, 88, 100) }
	draw_rect_filled(mut window, f32(x), f32(y), visual_badge_size, visual_badge_size, fill)
	draw_rect_empty(mut window, f32(x), f32(y), visual_badge_size, visual_badge_size, border)
}

fn draw_counter_bars(mut window gg.WindowSglContext, dashboard WindowDashboard, x int, y int, width int) {
	max_value := dashboard.max_counter()
	mut row_y := y
	draw_counter_bar(mut window, x, row_y, width, dashboard.lifecycle, max_value,
		event_family_color('window'))
	row_y += visual_counter_height + 4
	draw_counter_bar(mut window, x, row_y, width, dashboard.key, max_value,
		event_family_color('key'))
	row_y += visual_counter_height + 4
	draw_counter_bar(mut window, x, row_y, width, dashboard.mouse + dashboard.scroll, max_value,
		event_family_color('mouse'))
	row_y += visual_counter_height + 4
	draw_counter_bar(mut window, x, row_y, width, dashboard.focus, max_value,
		event_family_color('focus'))
	row_y += visual_counter_height + 4
	draw_counter_bar(mut window, x, row_y, width, dashboard.drop + dashboard.touch +
		dashboard.clipboard, max_value, event_family_color('drop'))
	row_y += visual_counter_height + 4
	draw_counter_bar(mut window, x, row_y, width, dashboard.text + dashboard.other, max_value,
		event_family_color('text'))
}

fn draw_counter_bar(mut window gg.WindowSglContext, x int, y int, width int, value int, max_value int, color gg.Color) {
	draw_rect_filled(mut window, f32(x), f32(y), f32(width), visual_counter_height, gg.rgb(38, 45,
		56))
	if value <= 0 {
		draw_rect_empty(mut window, f32(x), f32(y), f32(width), visual_counter_height, gg.rgb(64,
			73, 88))
		return
	}
	bar_width := scaled_width(value, max_value, width)
	draw_rect_filled(mut window, f32(x), f32(y), f32(bar_width), visual_counter_height, color)
	draw_rect_empty(mut window, f32(x), f32(y), f32(width), visual_counter_height, gg.rgb(132, 148,
		168))
}

fn draw_last_event_strip(mut window gg.WindowSglContext, dashboard WindowDashboard, x int, y int, width int) {
	slot_width := max_int(18, width / visible_last_event_limit)
	for slot in 0 .. visible_last_event_limit {
		family := if slot < dashboard.last_families.len {
			dashboard.last_families[slot]
		} else {
			'empty'
		}
		color := event_family_color(family)
		slot_x := x + slot * slot_width
		draw_rect_filled(mut window, f32(slot_x), f32(y), f32(slot_width - 6), 22, color)
		draw_rect_empty(mut window, f32(slot_x), f32(y), f32(slot_width - 6), 22, gg.rgb(156, 170,
			188))
	}
}

fn (dashboard WindowDashboard) max_counter() int {
	mut maximum := 1
	maximum = max_int(maximum, dashboard.lifecycle)
	maximum = max_int(maximum, dashboard.key)
	maximum = max_int(maximum, dashboard.text)
	maximum = max_int(maximum, dashboard.mouse + dashboard.scroll)
	maximum = max_int(maximum, dashboard.focus)
	maximum = max_int(maximum, dashboard.drop + dashboard.touch + dashboard.clipboard)
	maximum = max_int(maximum, dashboard.window)
	maximum = max_int(maximum, dashboard.other)
	return maximum
}

fn scaled_width(value int, max_value int, width int) int {
	if value <= 0 || width <= 0 {
		return 0
	}
	raw := value * width / max_int(1, max_value)
	return max_int(8, min_int(width, raw))
}

fn dashboard_background(caps gg.Capabilities) gg.Color {
	if caps.mock {
		return gg.rgb(28, 34, 42)
	}
	if caps.win32 {
		return gg.rgb(24, 36, 54)
	}
	if caps.backend == .appkit {
		return gg.rgb(38, 31, 45)
	}
	if caps.x11 {
		return gg.rgb(28, 42, 34)
	}
	if caps.wayland {
		return gg.rgb(38, 39, 28)
	}
	return gg.rgb(30, 32, 38)
}

fn backend_color(caps gg.Capabilities) gg.Color {
	if caps.mock {
		return gg.rgb(104, 120, 140)
	}
	if caps.win32 {
		return gg.rgb(66, 150, 255)
	}
	if caps.backend == .appkit {
		return gg.rgb(236, 112, 164)
	}
	if caps.x11 {
		return gg.rgb(82, 196, 126)
	}
	if caps.wayland {
		return gg.rgb(230, 196, 76)
	}
	return gg.rgb(150, 160, 176)
}

fn event_family_color(family string) gg.Color {
	return match family {
		'window' { gg.rgb(88, 166, 255) }
		'key' { gg.rgb(238, 98, 98) }
		'text' { gg.rgb(184, 116, 255) }
		'mouse' { gg.rgb(244, 172, 68) }
		'scroll' { gg.rgb(245, 196, 82) }
		'focus' { gg.rgb(94, 217, 236) }
		'drop' { gg.rgb(238, 214, 92) }
		'touch' { gg.rgb(255, 137, 196) }
		'clipboard' { gg.rgb(116, 222, 164) }
		'other' { gg.rgb(170, 180, 196) }
		else { gg.rgb(52, 60, 72) }
	}
}

fn max_int(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

fn min_int(a int, b int) int {
	if a < b {
		return a
	}
	return b
}

fn input_event_summary(event gg.WindowInputEvent) string {
	input := event.event
	match input.typ {
		.key_down {
			return '${event.window}: key down ${input.key_code} repeat=${input.key_repeat} modifiers=${input.modifiers}'
		}
		.key_up {
			return '${event.window}: key up ${input.key_code} modifiers=${input.modifiers}'
		}
		.char {
			return '${event.window}: char code ${input.char_code}'
		}
		.mouse_down {
			return '${event.window}: mouse down ${input.mouse_button} at ${input.mouse_x},${input.mouse_y}'
		}
		.mouse_up {
			return '${event.window}: mouse up ${input.mouse_button} at ${input.mouse_x},${input.mouse_y}'
		}
		.mouse_move {
			return '${event.window}: mouse move ${input.mouse_x},${input.mouse_y} delta=${input.mouse_dx},${input.mouse_dy}'
		}
		.mouse_scroll {
			return '${event.window}: scroll ${input.scroll_x},${input.scroll_y} at ${input.mouse_x},${input.mouse_y}'
		}
		.mouse_enter {
			return '${event.window}: mouse enter'
		}
		.mouse_leave {
			return '${event.window}: mouse leave'
		}
		.touches_began {
			return '${event.window}: touches began count=${input.num_touches}'
		}
		.touches_moved {
			return '${event.window}: touches moved count=${input.num_touches}'
		}
		.touches_ended {
			return '${event.window}: touches ended count=${input.num_touches}'
		}
		.touches_cancelled {
			return '${event.window}: touches cancelled count=${input.num_touches}'
		}
		.resized {
			return '${event.window}: resized ${input.window_width}x${input.window_height} framebuffer=${input.framebuffer_width}x${input.framebuffer_height}'
		}
		.iconified {
			return '${event.window}: iconified'
		}
		.restored {
			return '${event.window}: restored'
		}
		.focused {
			return '${event.window}: focused'
		}
		.unfocused {
			return '${event.window}: unfocused'
		}
		.suspended {
			return '${event.window}: suspended'
		}
		.resumed {
			return '${event.window}: resumed'
		}
		.quit_requested {
			return '${event.window}: quit requested'
		}
		.clipboard_pasted {
			return '${event.window}: clipboard pasted'
		}
		.files_dropped {
			return '${event.window}: ${event.dropped_files.len} file(s) dropped'
		}
		else {
			return ''
		}
	}
}

fn maybe_begin_client_chrome_action(event gg.WindowInputEvent, mut app gg.App, caps gg.Capabilities, native_decorations bool) !string {
	if !client_chrome_enabled(caps, native_decorations) {
		return ''
	}
	input := event.event
	if input.typ != .mouse_down || input.mouse_button != .left {
		return ''
	}
	if close_button_hit_at(input.mouse_x, input.mouse_y, input.window_width, input.window_height) {
		app.destroy_window(event.window)!
		return '${event.window}: client chrome close button destroyed window'
	}
	inactive_control := inactive_client_chrome_control_hit_at(input.mouse_x, input.mouse_y,
		input.window_width, input.window_height)
	if inactive_control {
		return ''
	}
	if edge := resize_edge_at(input.mouse_x, input.mouse_y, input.window_width, input.window_height) {
		app.begin_window_resize(event.window, edge)!
		return '${event.window}: interactive resize ${edge} started'
	}
	if move_hit_at(input.mouse_x, input.mouse_y, input.window_width, input.window_height) {
		app.begin_window_move(event.window)!
		return '${event.window}: interactive move started'
	}
	return ''
}

fn update_dashboard_cursor(event gg.WindowInputEvent, mut app gg.App, caps gg.Capabilities, native_decorations bool, hovered_control DashboardControl) ! {
	if !caps.cursor_shapes {
		return
	}
	input := event.event
	match input.typ {
		.mouse_leave {
			app.set_window_cursor(event.window, .default)!
		}
		.mouse_enter, .mouse_move {
			shape := if hovered_control != .inactive {
				gg.WindowCursorShape.pointer
			} else if client_chrome_enabled(caps, native_decorations) {
				client_chrome_cursor_shape_at(input.mouse_x, input.mouse_y, input.window_width,
					input.window_height)
			} else {
				gg.WindowCursorShape.default
			}
			app.set_window_cursor(event.window, shape)!
		}
		else {}
	}
}

fn client_chrome_cursor_shape_at(x f32, y f32, width int, height int) gg.WindowCursorShape {
	if close_button_hit_at(x, y, width, height) {
		return .pointer
	}
	if inactive_client_chrome_control_hit_at(x, y, width, height) {
		return .default
	}
	if edge := resize_edge_at(x, y, width, height) {
		return cursor_shape_for_resize_edge(edge)
	}
	if move_hit_at(x, y, width, height) {
		return .move
	}
	return .default
}

fn cursor_shape_for_resize_edge(edge gg.WindowResizeEdge) gg.WindowCursorShape {
	return match edge {
		.top, .bottom { .ns_resize }
		.left, .right { .ew_resize }
		.top_left, .bottom_right { .nwse_resize }
		.top_right, .bottom_left { .nesw_resize }
	}
}

fn client_chrome_enabled(caps gg.Capabilities, native_decorations bool) bool {
	return caps.wayland && caps.interactive_move_resize && !native_decorations
}

fn move_hit_at(x f32, y f32, width int, height int) bool {
	if width <= 2 * client_chrome_resize_margin || height <= client_chrome_titlebar_height {
		return false
	}
	if client_chrome_titlebar_control_hit_at(x, y, width, height) {
		return false
	}
	return x > f32(client_chrome_resize_margin) && x < f32(width - client_chrome_resize_margin)
		&& y >= 0 && y <= f32(client_chrome_titlebar_height)
}

fn inactive_client_chrome_control_hit_at(x f32, y f32, width int, height int) bool {
	return window_control_button_hit_at(x, y, width, height, 1)
		|| window_control_button_hit_at(x, y, width, height, 2)
}

fn client_chrome_titlebar_control_hit_at(x f32, y f32, width int, height int) bool {
	return close_button_hit_at(x, y, width, height)
		|| inactive_client_chrome_control_hit_at(x, y, width, height)
}

fn close_button_hit_at(x f32, y f32, width int, height int) bool {
	return window_control_button_hit_at(x, y, width, height, 0)
}

fn window_control_button_hit_at(x f32, y f32, width int, height int, index_from_right int) bool {
	if width <= 0 || height <= client_chrome_titlebar_height {
		return false
	}
	button_x := window_control_button_x(width, index_from_right)
	button_y := close_button_y()
	button_size := f32(client_chrome_close_button_size)
	return x >= button_x && x <= button_x + button_size && y >= button_y
		&& y <= button_y + button_size
}

fn close_button_x(width int) f32 {
	return window_control_button_x(width, 0)
}

fn window_control_button_x(width int, index_from_right int) f32 {
	step := client_chrome_close_button_size + client_chrome_control_gap
	return f32(width - client_chrome_close_button_margin - client_chrome_close_button_size -
		index_from_right * step)
}

fn close_button_y() f32 {
	return f32((client_chrome_titlebar_height - client_chrome_close_button_size) / 2)
}

fn resize_edge_at(x f32, y f32, width int, height int) ?gg.WindowResizeEdge {
	if width <= 0 || height <= 0 {
		return none
	}
	margin := f32(client_chrome_resize_margin)
	w := f32(width)
	h := f32(height)
	near_left := x >= 0 && x <= margin
	near_right := x >= w - margin && x <= w
	near_top := y >= 0 && y <= margin
	near_bottom := y >= h - margin && y <= h
	if near_top && near_left {
		return gg.WindowResizeEdge.top_left
	}
	if near_top && near_right {
		return gg.WindowResizeEdge.top_right
	}
	if near_top {
		return gg.WindowResizeEdge.top
	}
	if near_bottom && near_left {
		return gg.WindowResizeEdge.bottom_left
	}
	if near_bottom && near_right {
		return gg.WindowResizeEdge.bottom_right
	}
	if near_bottom {
		return gg.WindowResizeEdge.bottom
	}
	if near_left {
		return gg.WindowResizeEdge.left
	}
	if near_right {
		return gg.WindowResizeEdge.right
	}
	return none
}

fn resize_or_ignore_unsupported(mut app gg.App, window gg.WindowId, width int, height int) ! {
	app.resize_window(window, width, height) or {
		if err.msg() == 'multiwindow: backend capability is unsupported' {
			return
		}
		return err
	}
}
