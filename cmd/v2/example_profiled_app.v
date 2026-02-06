// Example application that writes profiler data for live monitoring
// Run with: ./v -enable-globals run cmd/v2/guiprof/example_app.v
// Then run guiprof in another terminal to see live data
module main

import time
import v2.profiler

fn main() {
	println('Starting example app with profiler...')
	println('Run guiprof in another terminal to see live data')
	println('Press Ctrl+C to stop')
	println('')

	// Initialize profiler
	profiler.profiler_init()
	profiler.profiler_enable()

	// Use the profiler allocator to track allocations
	old_alloc := profiler.use_profiler_allocator()
	defer {
		profiler.restore_allocator(old_alloc)
	}

	// Simulate an application loop
	mut frame := 0
	mut allocations := []voidptr{cap: 100}

	for {
		frame++

		// Simulate allocations (using profiler.alloc directly)
		num_allocs := 3 + (frame % 5)
		for _ in 0 .. num_allocs {
			size := 64 + (frame * 17) % 1024
			ptr := profiler.alloc(size)
			if ptr != unsafe { nil } {
				allocations << ptr
			}
		}

		// Simulate frees (free some old allocations)
		if allocations.len > 50 {
			num_frees := allocations.len / 4
			for _ in 0 .. num_frees {
				if allocations.len > 0 {
					ptr := allocations.pop()
					profiler.free(ptr)
				}
			}
		}

		// End frame and write snapshot for live monitoring
		profiler.frame_end_with_snapshot()

		// Print progress
		stats := profiler.get_statistics()
		if frame % 10 == 0 {
			println('Frame ${frame}: ${profiler.format_bytes(stats.live_bytes)} live, ${stats.total_allocs} allocs, ${stats.total_frees} frees')
		}

		// Simulate ~30 fps
		time.sleep(33 * time.millisecond)

		// Run for a while then exit
		if frame > 500 {
			println('Done! Ran 500 frames.')
			break
		}
	}
}
