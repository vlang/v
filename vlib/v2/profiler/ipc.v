// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module profiler

import os
import time

// Default path for profiler data exchange
pub const profiler_data_path = '/tmp/v2_profiler.dat'

// Snapshot represents a point-in-time view of profiler state for IPC
pub struct Snapshot {
pub mut:
	timestamp    i64
	frame_count  u64
	live_bytes   u64
	peak_bytes   u64
	total_allocs u64
	total_frees  u64
	frames       []FrameData
	allocs       []AllocRecord
}

// write_snapshot writes the current profiler state to the shared file
// Call this at the end of each frame in the profiled application
pub fn write_snapshot() {
	write_snapshot_to(profiler_data_path)
}

// write_snapshot_to writes profiler state to a specific path
pub fn write_snapshot_to(path string) {
	if profiler_state.mu == unsafe { nil } {
		return
	}

	profiler_state.mu.lock()
	defer {
		profiler_state.mu.unlock()
	}

	// Build snapshot data as simple text format (easy to parse)
	mut sb := []u8{cap: 4096}

	// Header with stats
	sb << 'V2PROF1\n'.bytes() // Version marker
	sb << 'T:${time.sys_mono_now()}\n'.bytes()
	sb << 'F:${profiler_state.current_frame}\n'.bytes()
	sb << 'L:${profiler_state.live_bytes}\n'.bytes()
	sb << 'P:${profiler_state.peak_bytes}\n'.bytes()
	sb << 'A:${profiler_state.total_allocs}\n'.bytes()
	sb << 'R:${profiler_state.total_frees}\n'.bytes()

	// Frame data (last 200 frames max)
	start_frame := if profiler_state.frames.len > 200 {
		profiler_state.frames.len - 200
	} else {
		0
	}
	sb << 'FRAMES:${profiler_state.frames.len - start_frame}\n'.bytes()
	for i := start_frame; i < profiler_state.frames.len; i++ {
		frame := profiler_state.frames[i]
		sb << '${frame.frame_num},${frame.new_bytes},${frame.freed_bytes},${frame.live_bytes}\n'.bytes()
	}

	// Allocation records (last 1000 max, for performance)
	start_alloc := if profiler_state.allocs.len > 1000 {
		profiler_state.allocs.len - 1000
	} else {
		0
	}
	sb << 'ALLOCS:${profiler_state.allocs.len - start_alloc}\n'.bytes()
	for i := start_alloc; i < profiler_state.allocs.len; i++ {
		alloc := profiler_state.allocs[i]
		freed_val := if alloc.freed { '1' } else { '0' }
		// ptr,size,frame,freed,free_frame,file,line
		sb << '${u64(alloc.ptr)},${alloc.size},${alloc.frame},${freed_val},${alloc.free_frame},${alloc.file},${alloc.line}\n'.bytes()
	}

	sb << 'END\n'.bytes()

	// Write atomically by writing to temp file then renaming
	tmp_path := path + '.tmp'
	os.write_file(tmp_path, sb.bytestr()) or { return }
	os.rename(tmp_path, path) or { return }
}

// read_snapshot reads profiler state from the shared file
// Returns none if file doesn't exist or is invalid
pub fn read_snapshot() ?Snapshot {
	return read_snapshot_from(profiler_data_path)
}

// read_snapshot_from reads profiler state from a specific path
pub fn read_snapshot_from(path string) ?Snapshot {
	content := os.read_file(path) or { return none }
	lines := content.split_into_lines()

	if lines.len < 7 || lines[0] != 'V2PROF1' {
		return none
	}

	mut snap := Snapshot{}

	// Parse header
	snap.timestamp = parse_value(lines[1], 'T:')
	snap.frame_count = u64(parse_value(lines[2], 'F:'))
	snap.live_bytes = u64(parse_value(lines[3], 'L:'))
	snap.peak_bytes = u64(parse_value(lines[4], 'P:'))
	snap.total_allocs = u64(parse_value(lines[5], 'A:'))
	snap.total_frees = u64(parse_value(lines[6], 'R:'))

	mut line_idx := 7

	// Parse frames
	if line_idx < lines.len && lines[line_idx].starts_with('FRAMES:') {
		num_frames := lines[line_idx].all_after('FRAMES:').int()
		line_idx++
		for _ in 0 .. num_frames {
			if line_idx >= lines.len {
				break
			}
			parts := lines[line_idx].split(',')
			if parts.len >= 4 {
				snap.frames << FrameData{
					frame_num:   u64(parts[0].i64())
					new_bytes:   u64(parts[1].i64())
					freed_bytes: u64(parts[2].i64())
					live_bytes:  u64(parts[3].i64())
				}
			}
			line_idx++
		}
	}

	// Parse allocs
	if line_idx < lines.len && lines[line_idx].starts_with('ALLOCS:') {
		num_allocs := lines[line_idx].all_after('ALLOCS:').int()
		line_idx++
		for _ in 0 .. num_allocs {
			if line_idx >= lines.len {
				break
			}
			parts := lines[line_idx].split(',')
			if parts.len >= 7 {
				snap.allocs << AllocRecord{
					ptr:        unsafe { voidptr(u64(parts[0].i64())) }
					size:       parts[1].int()
					frame:      u64(parts[2].i64())
					freed:      parts[3] == '1'
					free_frame: u64(parts[4].i64())
					file:       parts[5]
					line:       parts[6].int()
				}
			}
			line_idx++
		}
	}

	return snap
}

fn parse_value(line string, prefix string) i64 {
	if line.starts_with(prefix) {
		return line.all_after(prefix).i64()
	}
	return 0
}

// frame_end_with_snapshot ends the frame and writes a snapshot
// Use this instead of frame_end() when you want live monitoring
pub fn frame_end_with_snapshot() {
	frame_end()
	write_snapshot()
}
