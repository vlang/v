// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module profiler

import time

// profiler_alloc is the allocation function that records allocations
fn profiler_alloc(size int, ctx voidptr) voidptr {
	ptr := unsafe { C.malloc(size) }
	if ptr == unsafe { nil } {
		return ptr
	}

	if profiler_state.mu == unsafe { nil } || !profiler_state.enabled {
		return ptr
	}

	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	idx := profiler_state.allocs.len
	profiler_state.allocs << AllocRecord{
		ptr:       ptr
		size:      size
		frame:     profiler_state.current_frame
		file:      '' // Will be set by caller if needed
		line:      0
		timestamp: time.sys_mono_now()
		freed:     false
	}
	profiler_state.alloc_map[ptr] = idx
	profiler_state.live_bytes += u64(size)
	profiler_state.total_allocs++

	if profiler_state.live_bytes > profiler_state.peak_bytes {
		profiler_state.peak_bytes = profiler_state.live_bytes
	}

	// Add to current frame's new allocs
	if profiler_state.frames.len > 0 {
		frame_idx := profiler_state.frames.len - 1
		profiler_state.frames[frame_idx].new_allocs << idx
		profiler_state.frames[frame_idx].new_bytes += u64(size)
	}

	return ptr
}

// profiler_alloc_with_location records allocation with source location
pub fn profiler_alloc_with_location(size int, file string, line int) voidptr {
	ptr := unsafe { C.malloc(size) }
	if ptr == unsafe { nil } {
		return ptr
	}

	if profiler_state.mu == unsafe { nil } || !profiler_state.enabled {
		return ptr
	}

	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	idx := profiler_state.allocs.len
	profiler_state.allocs << AllocRecord{
		ptr:       ptr
		size:      size
		frame:     profiler_state.current_frame
		file:      file
		line:      line
		timestamp: time.sys_mono_now()
		freed:     false
	}
	profiler_state.alloc_map[ptr] = idx
	profiler_state.live_bytes += u64(size)
	profiler_state.total_allocs++

	if profiler_state.live_bytes > profiler_state.peak_bytes {
		profiler_state.peak_bytes = profiler_state.live_bytes
	}

	// Add to current frame's new allocs
	if profiler_state.frames.len > 0 {
		frame_idx := profiler_state.frames.len - 1
		profiler_state.frames[frame_idx].new_allocs << idx
		profiler_state.frames[frame_idx].new_bytes += u64(size)
	}

	return ptr
}

// profiler_free records a free operation
fn profiler_free(ptr voidptr, ctx voidptr) {
	if ptr == unsafe { nil } {
		return
	}

	if profiler_state.mu != unsafe { nil } && profiler_state.enabled {
		profiler_state.mu.lock()

		if idx := profiler_state.alloc_map[ptr] {
			profiler_state.allocs[idx].freed = true
			profiler_state.allocs[idx].free_frame = profiler_state.current_frame
			alloc_size := u64(profiler_state.allocs[idx].size)
			if profiler_state.live_bytes >= alloc_size {
				profiler_state.live_bytes -= alloc_size
			}
			profiler_state.total_frees++

			// Add to current frame's freed list
			if profiler_state.frames.len > 0 {
				frame_idx := profiler_state.frames.len - 1
				profiler_state.frames[frame_idx].freed_idxs << idx
				profiler_state.frames[frame_idx].freed_bytes += alloc_size
			}

			// Remove from map
			profiler_state.alloc_map.delete(ptr)
		}

		profiler_state.mu.unlock()
	}

	unsafe { C.free(ptr) }
}

// profiler_realloc records a realloc operation
fn profiler_realloc(ptr voidptr, new_size int, ctx voidptr) voidptr {
	if ptr == unsafe { nil } {
		return profiler_alloc(new_size, ctx)
	}
	if new_size == 0 {
		profiler_free(ptr, ctx)
		return unsafe { nil }
	}

	// Record the free of old allocation
	old_size := 0
	if profiler_state.mu != unsafe { nil } && profiler_state.enabled {
		profiler_state.mu.lock()
		if idx := profiler_state.alloc_map[ptr] {
			unsafe {
				*(&old_size) = profiler_state.allocs[idx].size
			}
		}
		profiler_state.mu.unlock()
	}

	new_ptr := unsafe { C.realloc(ptr, new_size) }
	if new_ptr == unsafe { nil } {
		return new_ptr
	}

	if profiler_state.mu != unsafe { nil } && profiler_state.enabled {
		profiler_state.mu.lock()
		defer {
			profiler_state.mu.unlock()
		}

		// Remove old entry
		if idx := profiler_state.alloc_map[ptr] {
			profiler_state.allocs[idx].freed = true
			profiler_state.allocs[idx].free_frame = profiler_state.current_frame
			if profiler_state.live_bytes >= u64(old_size) {
				profiler_state.live_bytes -= u64(old_size)
			}
			profiler_state.alloc_map.delete(ptr)
		}

		// Add new entry
		new_idx := profiler_state.allocs.len
		profiler_state.allocs << AllocRecord{
			ptr:       new_ptr
			size:      new_size
			frame:     profiler_state.current_frame
			file:      ''
			line:      0
			timestamp: time.sys_mono_now()
			freed:     false
		}
		profiler_state.alloc_map[new_ptr] = new_idx
		profiler_state.live_bytes += u64(new_size)

		if profiler_state.live_bytes > profiler_state.peak_bytes {
			profiler_state.peak_bytes = profiler_state.live_bytes
		}
	}

	return new_ptr
}

// profiler_allocator returns an Allocator that tracks all allocations
pub fn profiler_allocator() Allocator {
	return Allocator{
		alloc_fn:   profiler_alloc
		free_fn:    profiler_free
		realloc_fn: profiler_realloc
		ctx:        unsafe { nil }
	}
}
