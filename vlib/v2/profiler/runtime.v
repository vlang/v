// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module profiler

// profiler_init initializes the profiler system
// Must be called before enabling profiling
pub fn profiler_init() {
	init_profiler_state()
}

// profiler_enable enables allocation tracking
pub fn profiler_enable() {
	if profiler_state.mu == unsafe { nil } {
		init_profiler_state()
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}
	profiler_state.enabled = true
	// Start first frame
	if profiler_state.frames.len == 0 {
		profiler_state.frames << FrameData{
			frame_num:   0
			new_bytes:   0
			freed_bytes: 0
			live_bytes:  profiler_state.live_bytes
		}
	}
}

// profiler_disable disables allocation tracking
pub fn profiler_disable() {
	if profiler_state.mu == unsafe { nil } {
		return
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}
	profiler_state.enabled = false
}

// profiler_is_enabled returns whether profiling is active
pub fn profiler_is_enabled() bool {
	if profiler_state.mu == unsafe { nil } {
		return false
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}
	return profiler_state.enabled
}

// frame_end signals the end of a frame and collects frame data
// Call this at the end of each game/application frame
pub fn frame_end() {
	if profiler_state.mu == unsafe { nil } || !profiler_state.enabled {
		return
	}

	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	// Finalize current frame's live bytes
	if profiler_state.frames.len > 0 {
		profiler_state.frames[profiler_state.frames.len - 1].live_bytes = profiler_state.live_bytes
	}

	// Increment frame counter
	profiler_state.current_frame++

	// Start new frame
	profiler_state.frames << FrameData{
		frame_num:   profiler_state.current_frame
		new_bytes:   0
		freed_bytes: 0
		live_bytes:  profiler_state.live_bytes
	}
}

// get_frame_count returns the current frame number
pub fn get_frame_count() u64 {
	if profiler_state.mu == unsafe { nil } {
		return 0
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}
	return profiler_state.current_frame
}

// get_frames returns a copy of all frame data
pub fn get_frames() []FrameData {
	if profiler_state.mu == unsafe { nil } {
		return []
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}
	return profiler_state.frames.clone()
}

// get_allocs returns a copy of all allocation records
pub fn get_allocs() []AllocRecord {
	if profiler_state.mu == unsafe { nil } {
		return []
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}
	return profiler_state.allocs.clone()
}

// get_leaks returns allocations that were never freed
pub fn get_leaks() []AllocRecord {
	if profiler_state.mu == unsafe { nil } {
		return []
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	mut leaks := []AllocRecord{}
	for alloc in profiler_state.allocs {
		if !alloc.freed {
			leaks << alloc
		}
	}
	return leaks
}

// get_allocs_for_frame returns allocations made in a specific frame
pub fn get_allocs_for_frame(frame_num u64) []AllocRecord {
	if profiler_state.mu == unsafe { nil } {
		return []
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	mut frame_allocs := []AllocRecord{}
	for alloc in profiler_state.allocs {
		if alloc.frame == frame_num {
			frame_allocs << alloc
		}
	}
	return frame_allocs
}

// get_live_allocs returns currently live (not freed) allocations
pub fn get_live_allocs() []AllocRecord {
	if profiler_state.mu == unsafe { nil } {
		return []
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	mut live := []AllocRecord{}
	for alloc in profiler_state.allocs {
		if !alloc.freed {
			live << alloc
		}
	}
	return live
}

// reset clears all profiler data
pub fn reset() {
	if profiler_state.mu == unsafe { nil } {
		return
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	profiler_state.allocs.clear()
	profiler_state.alloc_map.clear()
	profiler_state.frames.clear()
	profiler_state.current_frame = 0
	profiler_state.live_bytes = 0
	profiler_state.peak_bytes = 0
	profiler_state.total_allocs = 0
	profiler_state.total_frees = 0

	// Start fresh frame
	if profiler_state.enabled {
		profiler_state.frames << FrameData{
			frame_num: 0
		}
	}
}

// use_profiler_allocator enables the profiler allocator in the current context
// Returns the old allocator for restoration
pub fn use_profiler_allocator() Allocator {
	return push_allocator(profiler_allocator())
}

// restore_allocator restores the previous allocator
pub fn restore_allocator(old Allocator) {
	pop_allocator(old)
}
