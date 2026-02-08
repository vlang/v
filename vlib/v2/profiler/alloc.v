// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module profiler

import time
import sync

// AllocRecord tracks a single allocation
pub struct AllocRecord {
pub:
	ptr       voidptr
	size      int
	frame     u64
	file      string // Source file
	line      int    // Source line
	timestamp i64    // Monotonic time in nanoseconds
pub mut:
	freed      bool
	free_frame u64
}

// FrameData aggregates allocation info for a single frame
pub struct FrameData {
pub:
	frame_num u64
pub mut:
	new_bytes   u64
	freed_bytes u64
	live_bytes  u64
	new_allocs  []int // Indices into allocs array
	freed_idxs  []int // Indices of allocations freed this frame
}

// ProfilerState holds all profiling data
@[heap]
pub struct ProfilerState {
pub mut:
	enabled       bool
	current_frame u64
	allocs        []AllocRecord
	alloc_map     map[voidptr]int // ptr -> index in allocs
	frames        []FrameData
	peak_bytes    u64
	live_bytes    u64
	total_allocs  u64
	total_frees   u64
	mu            &sync.Mutex = unsafe { nil }
}

// Global profiler state singleton
__global profiler_state = &ProfilerState{}

// init_profiler_state initializes the global profiler state
// Must be called before using the profiler
pub fn init_profiler_state() {
	profiler_state.mu = sync.new_mutex()
	profiler_state.enabled = false
	profiler_state.current_frame = 0
	profiler_state.allocs = []AllocRecord{cap: 10000}
	profiler_state.frames = []FrameData{cap: 1000}
	profiler_state.peak_bytes = 0
	profiler_state.live_bytes = 0
	profiler_state.total_allocs = 0
	profiler_state.total_frees = 0
}

// get_state returns the global profiler state
pub fn get_state() &ProfilerState {
	return profiler_state
}

// Statistics returns summary stats
pub struct Statistics {
pub:
	live_bytes   u64
	peak_bytes   u64
	total_allocs u64
	total_frees  u64
	leak_count   int
	frame_count  u64
}

// get_statistics returns current profiler statistics
pub fn get_statistics() Statistics {
	if profiler_state.mu == unsafe { nil } {
		return Statistics{}
	}
	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	mut leak_count := 0
	for alloc in profiler_state.allocs {
		if !alloc.freed {
			leak_count++
		}
	}

	return Statistics{
		live_bytes:   profiler_state.live_bytes
		peak_bytes:   profiler_state.peak_bytes
		total_allocs: profiler_state.total_allocs
		total_frees:  profiler_state.total_frees
		leak_count:   leak_count
		frame_count:  profiler_state.current_frame
	}
}

// format_bytes converts bytes to a human-readable string
pub fn format_bytes(bytes u64) string {
	if bytes < 1024 {
		return '${bytes} B'
	} else if bytes < 1024 * 1024 {
		return '${f64(bytes) / 1024.0:.1} KB'
	} else if bytes < 1024 * 1024 * 1024 {
		return '${f64(bytes) / (1024.0 * 1024.0):.1} MB'
	} else {
		return '${f64(bytes) / (1024.0 * 1024.0 * 1024.0):.2} GB'
	}
}

// now_mono returns the current monotonic time in nanoseconds
fn now_mono() i64 {
	return time.sys_mono_now()
}
