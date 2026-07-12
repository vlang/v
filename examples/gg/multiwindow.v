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

const visible_last_event_limit = 4
const visible_title_limit = 220
const visual_margin = 18
const visual_badge_size = 16
const visual_counter_height = 12
const client_chrome_titlebar_height = 36
const client_chrome_frame_thickness = 5
const client_chrome_resize_margin = 18
const client_chrome_close_button_size = 18
const client_chrome_close_button_margin = 9
const client_chrome_control_gap = 7
const client_chrome_title_text_scale = 2

struct WindowDashboard {
	id gg.WindowId
mut:
	label              string
	live               bool
	width              int
	height             int
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
	input_limit int = 12
mut:
	caps           gg.Capabilities
	resized        int
	printed_inputs int
	windows        []WindowDashboard
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

fn run_example() ! {
	mut app := new_app_prefer_renderer()!
	defer {
		app.stop() or {}
	}

	mut caps := app.capabilities()
	println('gg multi-window backend: ${caps.backend}')
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

	main_window := app.create_window(
		title:  'GG Multi-Window'
		width:  640
		height: 360
	)!
	tools_window := app.create_window(
		title:  'Tools'
		width:  320
		height: 240
	)!

	app.set_window_title(tools_window, 'Tools - updated')!
	resize_or_ignore_unsupported(mut app, main_window, 720, 420)!
	resize_or_ignore_unsupported(mut app, tools_window, 360, 260)!

	mut state := &EventState{
		mock: caps.mock
		caps: caps
	}
	state.track_window(main_window, 'Main', 720, 420)
	state.track_window(tools_window, 'Tools', 360, 260)
	state.sync_runtime(mut app)!
	caps = state.caps
	print_interactive_chrome_help(state.caps, state.has_client_chrome())
	state.update_all_titles(mut app)!

	println('live windows:')
	for info in app.window_infos()! {
		println('  ${info.title}: ${info.width}x${info.height} native_decorations=${info.native_decorations}')
	}

	if caps.explicit_swapchain {
		app.run(
			frame_fn: fn [mut state] (mut app gg.App) ! {
				state.draw(mut app)!
			}
			event_fn: fn [mut state] (event gg.WindowEvent, mut app gg.App) ! {
				handle_window_event(event, mut app, mut state)!
			}
			input_fn: fn [mut state] (event gg.WindowInputEvent, mut app gg.App) ! {
				handle_input_event(event, mut app, mut state)!
			}
		)!
	} else {
		app.run(
			event_fn: fn [mut state] (event gg.WindowEvent, mut app gg.App) ! {
				handle_window_event(event, mut app, mut state)!
			}
			input_fn: fn [mut state] (event gg.WindowInputEvent, mut app gg.App) ! {
				handle_input_event(event, mut app, mut state)!
			}
		)!
	}
}

fn print_capability_families(caps gg.Capabilities) {
	println('capability families:')
	println('  windows=${caps.multi_window} owner_queue=${caps.owner_queue} native=${caps.native}')
	println('  render explicit_swapchain=${caps.explicit_swapchain} gl=${caps.gl} metal=${caps.metal} d3d11=${caps.d3d11}')
	println('  input=${caps.input_events} mouse=${caps.mouse_events} keyboard=${caps.keyboard_events} text=${caps.text_events} focus=${caps.focus_events} drop=${caps.drop_events} touch=${caps.touch_events}')
	println('  chrome interactive_move_resize=${caps.interactive_move_resize} native_decorations=${caps.native_decorations} cursor_shapes=${caps.cursor_shapes}')
}

fn print_interactive_chrome_help(caps gg.Capabilities, has_client_chrome bool) {
	if has_client_chrome {
		println('Wayland client-side titlebar/frame enabled as fallback: drag the titlebar to move; drag frame edges/corners to resize; click the close button')
		println('move/resize is started from gg.App input_fn using the current native user-action serial')
		return
	}
	if caps.interactive_move_resize && caps.native_decorations {
		println('interactive move/resize available through native decorations; client chrome demo disabled')
		return
	}
	println('interactive move/resize unavailable for this backend; dashboard remains event-only')
}

fn handle_window_event(event gg.WindowEvent, mut app gg.App, mut state EventState) ! {
	state.note_lifecycle(event)
	state.update_window_title(mut app, event.window)!
	match event.kind {
		.window_created {
			println('window created: ${event.window}')
		}
		.window_resized {
			state.resized++
			println('window resized: ${event.window} -> ${event.width}x${event.height}')
			if state.mock && state.resized >= 2 {
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

fn handle_input_event(event gg.WindowInputEvent, mut app gg.App, mut state EventState) ! {
	message := input_event_summary(event)
	if message == '' {
		return
	}
	state.note_input(event, message)
	state.sync_runtime(mut app) or {}
	native_decorations := state.window_native_decorations(event.window)
	update_client_chrome_cursor(event, mut app, state.caps, native_decorations) or {}
	action_message := maybe_begin_client_chrome_action(event, mut app, state.caps,
		native_decorations) or { 'interactive chrome action failed: ${err.msg()}' }
	if action_message != '' {
		println(action_message)
		state.note_chrome_action(event.window, action_message)
	}
	state.update_window_title(mut app, event.window)!
	if state.printed_inputs >= state.input_limit {
		return
	}
	state.printed_inputs++
	println('input ${state.printed_inputs}/${state.input_limit}: ${message}')
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
	return truncate_title('${dashboard.label} | ${state.caps.backend} ${caps} | ${chrome} | ${live} | ${counters} | ${last}')
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

fn (mut state EventState) draw(mut app gg.App) ! {
	caps := state.caps
	for dashboard in state.windows {
		if !dashboard.live || !app.window_exists(dashboard.id) {
			continue
		}
		app.draw_window(dashboard.id, fn [dashboard, caps] (mut window gg.WindowContext) ! {
			draw_window_dashboard(mut window, dashboard, caps)
		})!
	}
}

fn draw_window_dashboard(mut window gg.WindowContext, dashboard WindowDashboard, caps gg.Capabilities) {
	size := window.size()
	width := max_int(220, size.width)
	height := max_int(180, size.height)
	inner_width := max_int(64, width - 2 * visual_margin)
	client_chrome := client_chrome_enabled(caps, dashboard.native_decorations)
	content_top := if client_chrome {
		client_chrome_titlebar_height + client_chrome_frame_thickness
	} else {
		0
	}
	window.draw_rect_filled(0, 0, f32(width), f32(height), dashboard_background(caps))
	draw_client_chrome_zones(mut window, dashboard, caps, width, height)
	if client_chrome {
		window.draw_rect_empty(f32(client_chrome_frame_thickness + 6), f32(content_top + 6), f32(
			width - 2 * client_chrome_frame_thickness - 12), f32(height - content_top -
			client_chrome_frame_thickness - 12), gg.rgb(88, 101, 118))
	} else {
		window.draw_rect_filled(0, 0, f32(width), 10, backend_color(caps))
		window.draw_rect_empty(8, 8, f32(width - 16), f32(height - 16), gg.rgb(96, 116, 142))
	}

	draw_lifecycle_panel(mut window, dashboard, caps, width, height, content_top)
	draw_capability_badges(mut window, caps, visual_margin, content_top + 28)
	draw_counter_bars(mut window, dashboard, visual_margin, content_top + 72, inner_width)
	draw_last_event_strip(mut window, dashboard, visual_margin, height - 54, inner_width)
}

fn draw_client_chrome_zones(mut window gg.WindowContext, dashboard WindowDashboard, caps gg.Capabilities, width int, height int) {
	if !client_chrome_enabled(caps, dashboard.native_decorations) {
		return
	}
	titlebar := client_chrome_titlebar_height
	frame := client_chrome_frame_thickness
	inner_width := max_int(0, width - 2 * frame)
	inner_height := max_int(0, height - titlebar - frame)
	window.draw_rect_filled(0, 0, f32(width), f32(height), gg.rgb(44, 50, 60))
	window.draw_rect_filled(f32(frame), f32(titlebar), f32(inner_width), f32(inner_height),
		dashboard_background(caps))
	window.draw_rect_filled(f32(frame), f32(frame), f32(inner_width), f32(titlebar - frame), gg.rgb(48,
		56, 68))
	window.draw_rect_filled(f32(frame), f32(frame), f32(inner_width), 1, gg.rgb(89, 101, 118))
	window.draw_rect_filled(f32(frame), f32(titlebar - 1), f32(inner_width), 1, gg.rgb(24, 30, 38))
	window.draw_rect_empty(0, 0, f32(width), f32(height), gg.rgb(20, 25, 32))
	window.draw_rect_empty(1, 1, f32(width - 2), f32(height - 2), gg.rgb(82, 94, 110))
	draw_client_chrome_title(mut window, dashboard.label, width)
	draw_client_chrome_titlebar_separators(mut window, width)
	draw_client_chrome_minimize_button(mut window, width)
	draw_client_chrome_maximize_button(mut window, width)
	draw_client_chrome_close_button(mut window, width)
}

fn draw_client_chrome_minimize_button(mut window gg.WindowContext, width int) {
	x := window_control_button_x(width, 2)
	y := close_button_y()
	size := f32(client_chrome_close_button_size)
	draw_client_chrome_control_button(mut window, x, y, gg.rgb(68, 78, 92), gg.rgb(126, 140, 158))
	window.draw_rect_filled(x + 5, y + size - 6, size - 10, 2, gg.rgb(218, 224, 232))
}

fn draw_client_chrome_maximize_button(mut window gg.WindowContext, width int) {
	x := window_control_button_x(width, 1)
	y := close_button_y()
	draw_client_chrome_control_button(mut window, x, y, gg.rgb(68, 78, 92), gg.rgb(126, 140, 158))
	window.draw_rect_empty(x + 5, y + 5, 8, 8, gg.rgb(218, 224, 232))
	window.draw_rect_filled(x + 6, y + 5, 6, 1, gg.rgb(218, 224, 232))
}

fn draw_client_chrome_close_button(mut window gg.WindowContext, width int) {
	x := close_button_x(width)
	y := close_button_y()
	size := f32(client_chrome_close_button_size)
	draw_client_chrome_control_button(mut window, x, y, gg.rgb(178, 71, 72), gg.rgb(234, 150, 145))
	for offset in 0 .. 5 {
		step := f32(offset * 2)
		window.draw_rect_filled(x + 5 + step, y + 5 + step, 2, 2, gg.rgb(250, 236, 232))
		window.draw_rect_filled(x + 5 + step, y + size - 7 - step, 2, 2, gg.rgb(250, 236, 232))
	}
}

fn draw_client_chrome_control_button(mut window gg.WindowContext, x f32, y f32, fill gg.Color, border gg.Color) {
	size := f32(client_chrome_close_button_size)
	window.draw_rect_filled(x, y, size, size, fill)
	window.draw_rect_empty(x, y, size, size, border)
}

fn draw_client_chrome_titlebar_separators(mut window gg.WindowContext, width int) {
	frame := client_chrome_frame_thickness
	inner_width := max_int(0, width - 2 * frame)
	titlebar := client_chrome_titlebar_height
	separator_x := int(window_control_button_x(width, 2)) - client_chrome_control_gap
	window.draw_rect_filled(f32(frame), f32(titlebar), f32(inner_width), 1, gg.rgb(14, 18, 24))
	window.draw_rect_filled(f32(frame), f32(titlebar + 1), f32(inner_width), 1, gg.rgb(77, 88, 104))
	if separator_x > frame + 80 {
		window.draw_rect_filled(f32(separator_x), f32(frame + 6), 1, f32(titlebar - frame - 12), gg.rgb(28,
			34, 43))
		window.draw_rect_filled(f32(separator_x + 1), f32(frame + 6), 1,
			f32(titlebar - frame - 12), gg.rgb(83, 94, 110))
	}
}

fn draw_client_chrome_title(mut window gg.WindowContext, title string, width int) {
	x := client_chrome_frame_thickness + 14
	y := (client_chrome_titlebar_height - 7 * client_chrome_title_text_scale) / 2
	max_width := int(window_control_button_x(width, 2)) - x - 18
	draw_tiny_text(mut window, title, x, y, max_width, client_chrome_title_text_scale, gg.rgb(230,
		235, 240))
}

fn draw_tiny_text(mut window gg.WindowContext, text string, x int, y int, max_width int, scale int, color gg.Color) {
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
					window.draw_rect_filled(f32(cursor_x + col * scale), f32(y + row * scale),
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

fn draw_lifecycle_panel(mut window gg.WindowContext, dashboard WindowDashboard, caps gg.Capabilities, width int, height int, content_top int) {
	status_color := if dashboard.live { gg.rgb(82, 196, 126) } else { gg.rgb(196, 82, 82) }
	activity_color := if dashboard.last_families.len == 0 {
		gg.rgb(78, 88, 104)
	} else {
		event_family_color(dashboard.last_families[dashboard.last_families.len - 1])
	}
	panel_y := content_top + 20
	window.draw_rect_filled(visual_margin, panel_y, 46, 8, status_color)
	if client_chrome_enabled(caps, dashboard.native_decorations) {
		window.draw_rect_filled(f32(width - 54), f32(panel_y), 34, 34, activity_color)
		window.draw_rect_empty(f32(width - 56), f32(panel_y - 2), 38, 38, gg.rgb(176, 188, 204))
	} else {
		window.draw_rect_filled(visual_margin + 56, panel_y, 72, 8, gg.rgb(78, 88, 104))
		window.draw_rect_empty(visual_margin + 56, panel_y, 72, 8, gg.rgb(122, 135, 152))
	}
	window.draw_rect_filled(visual_margin, f32(height - 20), f32(max_int(8, min_int(width - 2 * visual_margin,
		dashboard.width / 5))), 4, status_color)
}

fn draw_capability_badges(mut window gg.WindowContext, caps gg.Capabilities, x int, y int) {
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

fn draw_capability_badge(mut window gg.WindowContext, x int, y int, enabled bool, color gg.Color) {
	fill := if enabled { color } else { gg.rgb(42, 48, 58) }
	border := if enabled { gg.rgb(220, 230, 240) } else { gg.rgb(80, 88, 100) }
	window.draw_rect_filled(f32(x), f32(y), visual_badge_size, visual_badge_size, fill)
	window.draw_rect_empty(f32(x), f32(y), visual_badge_size, visual_badge_size, border)
}

fn draw_counter_bars(mut window gg.WindowContext, dashboard WindowDashboard, x int, y int, width int) {
	max_value := dashboard.max_counter()
	mut row_y := y
	draw_counter_bar(mut window, x, row_y, width, dashboard.lifecycle, max_value,
		event_family_color('window'))
	row_y += visual_counter_height + 7
	draw_counter_bar(mut window, x, row_y, width, dashboard.key, max_value,
		event_family_color('key'))
	row_y += visual_counter_height + 7
	draw_counter_bar(mut window, x, row_y, width, dashboard.mouse + dashboard.scroll, max_value,
		event_family_color('mouse'))
	row_y += visual_counter_height + 7
	draw_counter_bar(mut window, x, row_y, width, dashboard.focus, max_value,
		event_family_color('focus'))
	row_y += visual_counter_height + 7
	draw_counter_bar(mut window, x, row_y, width, dashboard.drop + dashboard.touch +
		dashboard.clipboard, max_value, event_family_color('drop'))
	row_y += visual_counter_height + 7
	draw_counter_bar(mut window, x, row_y, width, dashboard.text + dashboard.other, max_value,
		event_family_color('text'))
}

fn draw_counter_bar(mut window gg.WindowContext, x int, y int, width int, value int, max_value int, color gg.Color) {
	window.draw_rect_filled(f32(x), f32(y), f32(width), visual_counter_height, gg.rgb(38, 45, 56))
	if value <= 0 {
		window.draw_rect_empty(f32(x), f32(y), f32(width), visual_counter_height,
			gg.rgb(64, 73, 88))
		return
	}
	bar_width := scaled_width(value, max_value, width)
	window.draw_rect_filled(f32(x), f32(y), f32(bar_width), visual_counter_height, color)
	window.draw_rect_empty(f32(x), f32(y), f32(width), visual_counter_height, gg.rgb(132, 148, 168))
}

fn draw_last_event_strip(mut window gg.WindowContext, dashboard WindowDashboard, x int, y int, width int) {
	slot_width := max_int(18, width / visible_last_event_limit)
	for slot in 0 .. visible_last_event_limit {
		family := if slot < dashboard.last_families.len {
			dashboard.last_families[slot]
		} else {
			'empty'
		}
		color := event_family_color(family)
		slot_x := x + slot * slot_width
		window.draw_rect_filled(f32(slot_x), f32(y), f32(slot_width - 6), 22, color)
		window.draw_rect_empty(f32(slot_x), f32(y), f32(slot_width - 6), 22, gg.rgb(156, 170, 188))
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

fn update_client_chrome_cursor(event gg.WindowInputEvent, mut app gg.App, caps gg.Capabilities, native_decorations bool) ! {
	if !client_chrome_enabled(caps, native_decorations) || !caps.cursor_shapes {
		return
	}
	input := event.event
	match input.typ {
		.mouse_leave {
			app.set_window_cursor(event.window, .default)!
		}
		.mouse_enter, .mouse_move {
			shape := client_chrome_cursor_shape_at(input.mouse_x, input.mouse_y,
				input.window_width, input.window_height)
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
