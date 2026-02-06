// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import gg
import os
import v2.profiler

// handle_event processes user input events
pub fn handle_event(e &gg.Event, mut app App) {
	match e.typ {
		.mouse_down {
			handle_mouse_click(e, mut app)
		}
		.mouse_move {
			handle_mouse_move(e, mut app)
		}
		.key_down {
			handle_key_down(e, mut app)
		}
		else {}
	}
}

// handle_mouse_click processes mouse click events
fn handle_mouse_click(e &gg.Event, mut app App) {
	x := e.mouse_x
	y := e.mouse_y

	// Check which region was clicked
	hist_y := f32(app.header_height)
	hist_h := f32(app.histogram_height)
	timeline_y := hist_y + hist_h
	timeline_h := f32(app.timeline_height)
	ctrl_y := timeline_y + timeline_h
	ctrl_h := f32(app.controls_height)

	// Click in histogram area
	if y >= hist_y && y < hist_y + hist_h {
		handle_histogram_click(x, y, mut app)
		return
	}

	// Click in timeline area
	if y >= timeline_y && y < timeline_y + timeline_h {
		handle_timeline_click(x, y, mut app)
		return
	}

	// Click in controls area
	if y >= ctrl_y && y < ctrl_y + ctrl_h {
		handle_controls_click(x, y, mut app)
		return
	}
}

// handle_histogram_click selects a frame from the histogram
fn handle_histogram_click(x f32, y f32, mut app App) {
	frames := app.cached_frames
	if frames.len == 0 {
		return
	}

	w := f32(app.gg.width)
	margin := f32(40)
	bar_area_width := w - margin * 2

	// Calculate frame index
	relative_x := x - margin
	if relative_x < 0 || relative_x > bar_area_width {
		return
	}

	bar_width := bar_area_width / f32(frames.len)
	frame_idx := int(relative_x / bar_width)

	if frame_idx >= 0 && frame_idx < frames.len {
		app.selected_frame = frame_idx
		// Select first allocation in this frame
		allocs := app.cached_allocs
		for i, alloc in allocs {
			if alloc.frame == u64(frame_idx) {
				app.selected_alloc = i
				break
			}
		}
	}
}

// handle_timeline_click scrubs the timeline
fn handle_timeline_click(x f32, y f32, mut app App) {
	// Same logic as histogram click for frame selection
	handle_histogram_click(x, y, mut app)
}

// handle_controls_click handles filter button clicks
fn handle_controls_click(x f32, y f32, mut app App) {
	// Filter buttons start at x=80
	btn_start := f32(80)
	btn_w := f32(60)
	btn_gap := f32(10)

	relative_x := x - btn_start
	if relative_x < 0 {
		return
	}

	btn_idx := int(relative_x / (btn_w + btn_gap))
	modes := [FilterMode.all, FilterMode.new_not_freed, FilterMode.large, FilterMode.current_frame]

	if btn_idx >= 0 && btn_idx < modes.len {
		app.filter_mode = modes[btn_idx]
	}
}

// handle_mouse_move updates hover state
fn handle_mouse_move(e &gg.Event, mut app App) {
	x := e.mouse_x
	y := e.mouse_y

	hist_y := f32(app.header_height)
	hist_h := f32(app.histogram_height)

	// Check if hovering over histogram
	if y >= hist_y && y < hist_y + hist_h {
		frames := app.cached_frames
		if frames.len > 0 {
			w := f32(app.gg.width)
			margin := f32(40)
			bar_area_width := w - margin * 2
			relative_x := x - margin

			if relative_x >= 0 && relative_x <= bar_area_width {
				bar_width := bar_area_width / f32(frames.len)
				app.hover_frame = int(relative_x / bar_width)
				if app.hover_frame >= frames.len {
					app.hover_frame = -1
				}
			} else {
				app.hover_frame = -1
			}
		}
	} else {
		app.hover_frame = -1
	}
}

// handle_key_down processes keyboard input
fn handle_key_down(e &gg.Event, mut app App) {
	match e.key_code {
		.left {
			// Previous frame
			if app.selected_frame > 0 {
				app.selected_frame--
			}
		}
		.right {
			// Next frame
			frames := app.cached_frames
			if app.selected_frame < frames.len - 1 {
				app.selected_frame++
			}
		}
		.up {
			// Previous allocation
			if app.selected_alloc > 0 {
				app.selected_alloc--
			}
		}
		.down {
			// Next allocation
			allocs := app.cached_allocs
			if app.selected_alloc < allocs.len - 1 {
				app.selected_alloc++
			}
		}
		.enter {
			// Open source file in editor
			open_in_editor(app)
		}
		.f {
			// Cycle filter modes
			app.filter_mode = match app.filter_mode {
				.all { .new_not_freed }
				.new_not_freed { .large }
				.large { .current_frame }
				.current_frame { .all }
			}
		}
		.r {
			// Reset profiler data
			if !app.demo_mode {
				profiler.reset()
			}
		}
		.escape {
			// Clear selection
			app.selected_frame = -1
			app.selected_alloc = -1
		}
		.space {
			// Toggle demo mode
			app.demo_mode = !app.demo_mode
			if app.demo_mode {
				// Initialize demo data
				init_demo_data(mut app)
			}
		}
		else {}
	}
}

// open_in_editor opens the selected allocation's source file in VS Code
fn open_in_editor(app &App) {
	if app.selected_alloc < 0 {
		return
	}

	allocs := app.cached_allocs
	if app.selected_alloc >= allocs.len {
		return
	}

	alloc := allocs[app.selected_alloc]
	if alloc.file.len == 0 {
		return
	}

	// Try to open in VS Code with line number
	cmd := 'code -g "${alloc.file}:${alloc.line}"'
	os.execute(cmd)
}
