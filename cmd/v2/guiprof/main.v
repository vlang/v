// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import gg
import v2.profiler

fn main() {
	// Initialize profiler system
	profiler.profiler_init()

	// Create application state (on stack, like the working example)
	mut app := App{}

	// Initialize with demo data
	init_demo_data(mut app)

	// Create gg context
	app.gg = gg.new_context(
		width:         window_width
		height:        window_height
		create_window: true
		window_title:  'V2 Allocation Profiler'
		bg_color:      bg_color
		frame_fn:      frame
		event_fn:      event_handler
		user_data:     &app
	)

	// Run the application
	app.gg.run()
}

// frame is the frame callback for gg
fn frame(mut app App) {
	draw_frame(mut app)
}

// event_handler handles gg events
fn event_handler(e &gg.Event, mut app App) {
	handle_event(e, mut app)
}

// init_demo_data populates the profiler with simulated data for demonstration
fn init_demo_data(mut app App) {
	app.demo_mode = true
	app.demo_frame = 0

	// Create simulated frame and allocation data
	app.cached_frames = []profiler.FrameData{cap: 100}
	app.cached_allocs = []profiler.AllocRecord{cap: 500}

	// Simulate 100 frames of allocation activity
	mut total_live := u64(0)
	mut peak := u64(0)
	mut alloc_idx := 0
	mut total_allocs := u64(0)
	mut total_frees := u64(0)

	for frame_num := 0; frame_num < 100; frame_num++ {
		mut frame_data := profiler.FrameData{
			frame_num: u64(frame_num)
		}

		// Simulate some allocations each frame
		// More allocations early, fewer later (typical startup pattern)
		num_allocs := if frame_num < 20 { 10 - frame_num / 4 } else { 2 + (frame_num % 3) }

		for _ in 0 .. num_allocs {
			// Random-ish allocation sizes
			size := 64 + ((frame_num * 17 + alloc_idx * 31) % 4096)

			app.cached_allocs << profiler.AllocRecord{
				ptr:       unsafe { voidptr(u64(0x1000) + u64(alloc_idx) * 0x100) }
				size:      size
				frame:     u64(frame_num)
				file:      ['main.v', 'parser.v', 'scanner.v', 'types.v', 'gen.v'][alloc_idx % 5]
				line:      100 + (alloc_idx % 500)
				timestamp: i64(frame_num) * 16_666_667 + i64(alloc_idx) * 1000
				freed:     false
			}

			frame_data.new_allocs << alloc_idx
			frame_data.new_bytes += u64(size)
			total_live += u64(size)
			total_allocs++
			alloc_idx++
		}

		// Simulate some frees (but not all - create some "leaks")
		// Free some allocations from previous frames
		if frame_num > 5 {
			num_frees := num_allocs - 1 // Always free slightly fewer than we allocate
			mut freed_count := 0
			for i := 0; i < app.cached_allocs.len && freed_count < num_frees; i++ {
				if !app.cached_allocs[i].freed && app.cached_allocs[i].frame < u64(frame_num - 3) {
					// 80% chance to free
					if (i * 7 + frame_num * 13) % 10 < 8 {
						app.cached_allocs[i].freed = true
						app.cached_allocs[i].free_frame = u64(frame_num)
						frame_data.freed_idxs << i
						frame_data.freed_bytes += u64(app.cached_allocs[i].size)
						total_live -= u64(app.cached_allocs[i].size)
						total_frees++
						freed_count++
					}
				}
			}
		}

		frame_data.live_bytes = total_live
		if total_live > peak {
			peak = total_live
		}

		app.cached_frames << frame_data
	}

	// Count leaks
	mut leak_count := 0
	for alloc in app.cached_allocs {
		if !alloc.freed {
			leak_count++
		}
	}

	// Update stats
	app.cached_stats = profiler.Statistics{
		live_bytes:   total_live
		peak_bytes:   peak
		total_allocs: total_allocs
		total_frees:  total_frees
		leak_count:   leak_count
		frame_count:  100
	}
}

// update_demo_data simulates ongoing activity in demo mode
fn update_demo_data(mut app App) {
	// In demo mode, we just use the static simulated data
	// For a more dynamic demo, we could add new frames here
	app.demo_frame++
}
