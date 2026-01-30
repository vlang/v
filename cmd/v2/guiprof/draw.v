// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import gg
import v2.profiler

// draw_header renders the top stats bar
fn draw_header(mut app App) {
	ctx := app.gg
	w := f32(ctx.width)

	// Background
	ctx.draw_rect_filled(0, 0, w, f32(app.header_height), header_bg)

	// Get stats
	stats := if app.demo_mode { app.cached_stats } else { profiler.get_statistics() }

	// Format and draw stats
	heap_str := 'HEAP: ${profiler.format_bytes(stats.live_bytes)}'
	peak_str := 'Peak: ${profiler.format_bytes(stats.peak_bytes)}'
	allocs_str := '${stats.total_allocs} allocs'
	frees_str := '${stats.total_frees} frees'
	leaks_str := '${stats.leak_count} leaks'

	// Draw stats with spacing
	mut x := f32(20)
	y := f32(22)

	ctx.draw_text(int(x), int(y), heap_str, color: text_color, size: 20)
	x += 180

	ctx.draw_text(int(x), int(y), peak_str, color: text_color, size: 20)
	x += 160

	ctx.draw_text(int(x), int(y), allocs_str, color: new_alloc_color, size: 20)
	x += 140

	ctx.draw_text(int(x), int(y), frees_str, color: freed_color, size: 20)
	x += 120

	// Show leaks in red if there are any
	leak_clr := if stats.leak_count > 0 { leak_color } else { text_color }
	ctx.draw_text(int(x), int(y), leaks_str, color: leak_clr, size: 20)

	// Frame counter on right
	frame_str := 'Frame: ${stats.frame_count}'
	ctx.draw_text(int(w) - 150, int(y), frame_str, color: text_color, size: 20)

	// Separator line
	ctx.draw_line(0, f32(app.header_height), w, f32(app.header_height), grid_color)
}

// draw_histogram renders the allocation histogram
fn draw_histogram(mut app App) {
	ctx := app.gg
	w := f32(ctx.width)

	// Histogram area
	hist_y := f32(app.header_height)
	hist_h := f32(app.histogram_height)
	center_y := hist_y + hist_h / 2

	// Background
	ctx.draw_rect_filled(0, hist_y, w, hist_h, bg_color)

	// Center line
	ctx.draw_line(0, center_y, w, center_y, grid_color)

	// Get frame data
	frames := if app.demo_mode { app.cached_frames } else { profiler.get_frames() }
	if frames.len == 0 {
		ctx.draw_text(int(w / 2) - 80, int(center_y) - 10, 'No frame data',
			color: text_color
			size:  18
		)
		return
	}

	// Calculate max values for scaling
	mut max_new := u64(1)
	mut max_freed := u64(1)
	for frame in frames {
		if frame.new_bytes > max_new {
			max_new = frame.new_bytes
		}
		if frame.freed_bytes > max_freed {
			max_freed = frame.freed_bytes
		}
	}

	// Bar dimensions
	margin := f32(40)
	bar_area_width := w - margin * 2
	bar_width := bar_area_width / f32(frames.len)
	if bar_width < 1 {
		return
	}
	bar_gap := bar_width * 0.1
	actual_bar_w := bar_width - bar_gap

	max_bar_height := (hist_h / 2) - 10

	// Draw bars for each frame
	for i, frame in frames {
		x := margin + f32(i) * bar_width

		// New allocations (cyan, above center)
		if frame.new_bytes > 0 {
			new_height := f32(frame.new_bytes) / f32(max_new) * max_bar_height
			bar_color := if i == app.selected_frame { selected_color } else { new_alloc_color }
			ctx.draw_rect_filled(x, center_y - new_height, actual_bar_w, new_height, bar_color)
		}

		// Freed allocations (purple, below center)
		if frame.freed_bytes > 0 {
			freed_height := f32(frame.freed_bytes) / f32(max_freed) * max_bar_height
			bar_color := if i == app.selected_frame {
				gg.Color{150, 130, 255, 200}
			} else {
				freed_color
			}
			ctx.draw_rect_filled(x, center_y, actual_bar_w, freed_height, bar_color)
		}

		// Hover highlight
		if i == app.hover_frame && i != app.selected_frame {
			ctx.draw_rect_empty(x - 1, hist_y + 5, actual_bar_w + 2, hist_h - 10, timeline_cursor)
		}
	}

	// Legend
	ctx.draw_rect_filled(10, hist_y + 10, 12, 12, new_alloc_color)
	ctx.draw_text(26, int(hist_y) + 8, 'New', color: text_color, size: 14)

	ctx.draw_rect_filled(70, hist_y + 10, 12, 12, freed_color)
	ctx.draw_text(86, int(hist_y) + 8, 'Freed', color: text_color, size: 14)

	// Separator line
	ctx.draw_line(0, hist_y + hist_h, w, hist_y + hist_h, grid_color)
}

// draw_timeline renders the frame timeline with scrubber
fn draw_timeline(mut app App) {
	ctx := app.gg
	w := f32(ctx.width)

	// Timeline area
	timeline_y := f32(app.header_height + app.histogram_height)
	timeline_h := f32(app.timeline_height)

	// Background
	ctx.draw_rect_filled(0, timeline_y, w, timeline_h, header_bg)

	frames := if app.demo_mode { app.cached_frames } else { profiler.get_frames() }
	if frames.len == 0 {
		return
	}

	// Draw frame ticks
	margin := f32(40)
	tick_area_width := w - margin * 2
	tick_spacing := tick_area_width / f32(frames.len)

	// Draw tick marks
	for i := 0; i < frames.len; i++ {
		x := margin + f32(i) * tick_spacing
		tick_height := if i % 10 == 0 { f32(15) } else { f32(8) }
		ctx.draw_line(x, timeline_y + 20, x, timeline_y + 20 + tick_height, grid_color)

		// Frame number labels every 10 frames
		if i % 10 == 0 && tick_spacing > 3 {
			ctx.draw_text(int(x) - 10, int(timeline_y) + 40, '${i}', color: text_color, size: 12)
		}
	}

	// Draw selected frame cursor
	if app.selected_frame >= 0 && app.selected_frame < frames.len {
		cursor_x := margin + f32(app.selected_frame) * tick_spacing
		// Triangle cursor
		draw_triangle_filled(ctx, cursor_x, timeline_y + 15, cursor_x - 8, timeline_y + 5,
			cursor_x + 8, timeline_y + 5, timeline_cursor)

		// Vertical line
		ctx.draw_line(cursor_x, timeline_y + 15, cursor_x, timeline_y + timeline_h - 5,
			timeline_cursor)

		// Frame info
		frame := frames[app.selected_frame]
		info := 'Frame ${app.selected_frame}: +${profiler.format_bytes(frame.new_bytes)} / -${profiler.format_bytes(frame.freed_bytes)}'
		ctx.draw_text(int(w / 2) - 100, int(timeline_y) + 55, info, color: timeline_cursor, size: 16)
	}

	// Separator line
	ctx.draw_line(0, timeline_y + timeline_h, w, timeline_y + timeline_h, grid_color)
}

// draw_controls renders the filter controls
fn draw_controls(mut app App) {
	ctx := app.gg
	w := f32(ctx.width)

	// Controls area
	ctrl_y := f32(app.header_height + app.histogram_height + app.timeline_height)
	ctrl_h := f32(app.controls_height)

	// Background
	ctx.draw_rect_filled(0, ctrl_y, w, ctrl_h, bg_color)

	// Filter buttons
	mut x := f32(20)
	y := ctrl_y + 10

	// Filter label
	ctx.draw_text(int(x), int(y), 'Filter:', color: text_color, size: 16)
	x += 60

	// Filter mode buttons
	filters := ['All', 'Leaks', 'Large', 'Frame']
	modes := [FilterMode.all, FilterMode.new_not_freed, FilterMode.large, FilterMode.current_frame]

	for i, label in filters {
		btn_w := f32(60)
		btn_h := f32(24)

		// Highlight active filter
		btn_color := if app.filter_mode == modes[i] { selected_color } else { header_bg }
		ctx.draw_rect_filled(x, y - 2, btn_w, btn_h, btn_color)
		ctx.draw_rect_empty(x, y - 2, btn_w, btn_h, grid_color)

		ctx.draw_text(int(x) + 8, int(y), label, color: text_color, size: 14)
		x += btn_w + 10
	}

	// Separator line
	ctx.draw_line(0, ctrl_y + ctrl_h, w, ctrl_y + ctrl_h, grid_color)
}

// draw_details renders the selection details panel
fn draw_details(mut app App) {
	ctx := app.gg
	w := f32(ctx.width)
	h := f32(ctx.height)

	// Details area
	details_y := f32(app.header_height + app.histogram_height + app.timeline_height +
		app.controls_height)
	details_h := h - details_y

	// Background
	ctx.draw_rect_filled(0, details_y, w, details_h, header_bg)

	// Show selected allocation info or instructions
	if app.selected_alloc >= 0 {
		allocs := if app.demo_mode { app.cached_allocs } else { profiler.get_allocs() }
		if app.selected_alloc < allocs.len {
			alloc := allocs[app.selected_alloc]

			// Format allocation info
			ptr_str := 'Address: 0x${voidptr(alloc.ptr):p}'
			size_str := 'Size: ${profiler.format_bytes(u64(alloc.size))} (${alloc.size} bytes)'
			frame_str := 'Allocated: frame ${alloc.frame}'
			status_str := if alloc.freed {
				'Status: FREED (frame ${alloc.free_frame})'
			} else {
				'Status: LIVE (potential leak)'
			}

			status_color := if alloc.freed { freed_color } else { leak_color }

			mut y := details_y + 15
			ctx.draw_text(20, int(y), ptr_str, color: text_color, size: 16)

			ctx.draw_text(250, int(y), size_str, color: text_color, size: 16)

			ctx.draw_text(500, int(y), frame_str, color: text_color, size: 16)

			ctx.draw_text(700, int(y), status_str, color: status_color, size: 16)

			// Source location if available
			if alloc.file.len > 0 {
				y += 25
				loc_str := 'Source: ${alloc.file}:${alloc.line}'
				ctx.draw_text(20, int(y), loc_str, color: new_alloc_color, size: 16)
			}
		}
	} else {
		// Instructions
		ctx.draw_text(20, int(details_y) + 20, 'Click on a histogram bar to select a frame, or use keyboard arrows to navigate',
			color: text_color
			size:  16
		)
	}
}

// draw_frame is the main frame callback
pub fn draw_frame(mut app App) {
	if app.gg == unsafe { nil } {
		return
	}
	app.gg.begin()

	// Draw a test rectangle to verify rendering works
	app.gg.draw_rect_filled(100, 100, 200, 100, gg.Color{255, 0, 0, 255})

	// Update demo data if in demo mode
	if app.demo_mode {
		update_demo_data(mut app)
	} else {
		app.update_cache()
	}

	// Draw all sections
	draw_header(mut app)
	draw_histogram(mut app)
	draw_timeline(mut app)
	draw_controls(mut app)
	draw_details(mut app)

	app.gg.end()
}

// Helper: draw a filled triangle
fn draw_triangle_filled(ctx &gg.Context, x1 f32, y1 f32, x2 f32, y2 f32, x3 f32, y3 f32, c gg.Color) {
	ctx.draw_convex_poly([x1, y1, x2, y2, x3, y3], c)
}
