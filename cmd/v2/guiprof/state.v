// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import gg
import v2.profiler

// Filter modes for allocation view
pub enum FilterMode {
	all           // Show all allocations
	new_not_freed // Show allocations that haven't been freed (potential leaks)
	large         // Show allocations larger than threshold
	current_frame // Show allocations from selected frame only
}

// App holds the GUI application state
pub struct App {
pub mut:
	gg &gg.Context = unsafe { nil }
	// View state
	selected_frame  int        = -1 // -1 means no selection
	selected_alloc  int        = -1 // Index of selected allocation
	filter_mode     FilterMode = .all
	large_threshold int        = 1024 // Threshold for "large" filter (bytes)
	search_text     string
	// Scroll/pan state
	timeline_scroll  int // Horizontal scroll for timeline
	histogram_scroll int // Scroll for histogram if needed
	// Cached data (updated each frame)
	cached_frames []profiler.FrameData
	cached_allocs []profiler.AllocRecord
	cached_stats  profiler.Statistics
	// Layout dimensions
	header_height    int = 60
	histogram_height int = 200
	timeline_height  int = 80
	controls_height  int = 40
	details_height   int = 60
	// Hover state
	hover_frame int = -1
	hover_alloc int = -1
	// Demo mode
	demo_mode  bool = true // Use simulated data for demo
	demo_frame int // Current demo frame counter
}

// Colors for the profiler visualization
pub const bg_color = gg.Color{30, 30, 30, 255} // Dark gray background
pub const new_alloc_color = gg.Color{0, 200, 255, 200} // Cyan for new allocations
pub const freed_color = gg.Color{180, 100, 255, 200} // Purple for freed allocations
pub const leak_color = gg.Color{255, 100, 100, 255} // Red for leaks/live
pub const timeline_cursor = gg.Color{255, 255, 0, 255} // Yellow for timeline cursor
pub const text_color = gg.Color{220, 220, 220, 255} // Light gray text
pub const header_bg = gg.Color{40, 40, 45, 255} // Slightly lighter header
pub const grid_color = gg.Color{60, 60, 60, 255} // Grid lines
pub const selected_color = gg.Color{100, 150, 255, 255} // Selection highlight

// Window dimensions
pub const window_width = 1200
pub const window_height = 700

// init_app creates a new App instance (unused, kept for reference)
fn init_app_heap() &App {
	return &App{}
}

// update_cache refreshes cached profiler data
pub fn (mut app App) update_cache() {
	if app.demo_mode {
		return
	}
	app.cached_frames = profiler.get_frames()
	app.cached_allocs = profiler.get_allocs()
	app.cached_stats = profiler.get_statistics()
}

// get_filtered_allocs returns allocations matching current filter
pub fn (app &App) get_filtered_allocs() []profiler.AllocRecord {
	mut result := []profiler.AllocRecord{}

	allocs := if app.demo_mode { app.cached_allocs } else { profiler.get_allocs() }

	for alloc in allocs {
		match app.filter_mode {
			.all {
				result << alloc
			}
			.new_not_freed {
				if !alloc.freed {
					result << alloc
				}
			}
			.large {
				if alloc.size >= app.large_threshold {
					result << alloc
				}
			}
			.current_frame {
				if app.selected_frame >= 0 && alloc.frame == u64(app.selected_frame) {
					result << alloc
				}
			}
		}
	}

	// Apply search filter if text is present
	if app.search_text.len > 0 {
		mut filtered := []profiler.AllocRecord{}
		for alloc in result {
			if alloc.file.contains(app.search_text) {
				filtered << alloc
			}
		}
		return filtered
	}

	return result
}

// get_frame_at_x returns the frame index at the given x coordinate in timeline
pub fn (app &App) get_frame_at_x(x f32, timeline_x f32, timeline_width f32) int {
	frames := if app.demo_mode { app.cached_frames } else { profiler.get_frames() }
	if frames.len == 0 {
		return -1
	}

	// Calculate which frame this x position corresponds to
	relative_x := x - timeline_x
	if relative_x < 0 || relative_x > timeline_width {
		return -1
	}

	frame_width := timeline_width / f32(frames.len)
	frame_idx := int(relative_x / frame_width)

	if frame_idx >= 0 && frame_idx < frames.len {
		return frame_idx
	}
	return -1
}
