// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Performance optimizations for HTTP/2 implementation

// Optimized buffer pool for reducing allocations
struct BufferPool {
mut:
	buffers [][]u8
	size    int
}

// new_buffer_pool creates a new buffer pool
pub fn new_buffer_pool(size int, count int) BufferPool {
	mut buffers := [][]u8{cap: count}
	for _ in 0 .. count {
		buffers << []u8{len: size, cap: size}
	}
	return BufferPool{
		buffers: buffers
		size:    size
	}
}

// get gets a buffer from the pool
pub fn (mut p BufferPool) get() []u8 {
	if p.buffers.len > 0 {
		buf := p.buffers[p.buffers.len - 1]
		p.buffers = p.buffers[..p.buffers.len - 1]
		return buf
	}
	return []u8{len: p.size, cap: p.size}
}

// put returns a buffer to the pool
pub fn (mut p BufferPool) put(buf []u8) {
	if buf.cap == p.size {
		// Clear buffer before returning to pool
		mut cleared := unsafe { buf[..0] }
		p.buffers << cleared
	}
}

// Optimized frame encoding with pre-allocated buffer
pub fn encode_frame_optimized(frame Frame, mut buf []u8) int {
	// Ensure buffer is large enough
	required_size := 9 + frame.payload.len
	if buf.len < required_size {
		return 0
	}

	// Write frame header (9 bytes)
	length := u32(frame.payload.len)
	buf[0] = u8(length >> 16)
	buf[1] = u8(length >> 8)
	buf[2] = u8(length)
	buf[3] = u8(frame.header.frame_type)
	buf[4] = frame.header.flags
	buf[5] = u8((frame.header.stream_id >> 24) & 0x7f)
	buf[6] = u8(frame.header.stream_id >> 16)
	buf[7] = u8(frame.header.stream_id >> 8)
	buf[8] = u8(frame.header.stream_id)

	// Copy payload using bulk copy
	if frame.payload.len > 0 {
		unsafe {
			vmemcpy(&buf[9], frame.payload.data, frame.payload.len)
		}
	}

	return required_size
}

// Optimized HPACK encoding with buffer reuse
pub fn (mut e Encoder) encode_optimized(headers []HeaderField, mut buf []u8) int {
	mut offset := 0

	for header in headers {
		// Try to find in static table
		mut found := false
		for i, entry in static_table {
			if entry.name == header.name {
				if entry.value == header.value {
					// Indexed header field
					if offset + 1 > buf.len {
						return offset
					}
					buf[offset] = u8(0x80 | i)
					offset++
					found = true
					break
				} else if entry.value == '' {
					// Literal with incremental indexing - indexed name
					if offset + 2 + header.value.len > buf.len {
						return offset
					}
					buf[offset] = u8(0x40 | i)
					offset++

					// Encode value
					buf[offset] = u8(header.value.len)
					offset++
					for b in header.value.bytes() {
						buf[offset] = b
						offset++
					}
					found = true
					break
				}
			}
		}

		if !found {
			// Literal with incremental indexing - new name
			name_len := header.name.len
			value_len := header.value.len
			required := 3 + name_len + value_len

			if offset + required > buf.len {
				return offset
			}

			buf[offset] = 0x40
			offset++

			buf[offset] = u8(name_len)
			offset++
			for b in header.name.bytes() {
				buf[offset] = b
				offset++
			}

			buf[offset] = u8(value_len)
			offset++
			for b in header.value.bytes() {
				buf[offset] = b
				offset++
			}
		}
	}

	return offset
}

// Fast integer encoding for variable-length integers
pub fn encode_int_fast(value u64, prefix_bits u8, mut buf []u8, offset int) int {
	max_prefix := (u64(1) << prefix_bits) - 1

	if value < max_prefix {
		buf[offset] = u8(value)
		return 1
	}

	buf[offset] = u8(max_prefix)
	mut remaining := value - max_prefix
	mut pos := offset + 1

	for remaining >= 128 {
		buf[pos] = u8((remaining % 128) + 128)
		remaining /= 128
		pos++
	}

	buf[pos] = u8(remaining)
	return pos - offset + 1
}

// Fast string comparison for header matching
@[inline]
pub fn fast_string_equal(a string, b string) bool {
	if a.len != b.len {
		return false
	}

	// Use unsafe pointer comparison for speed
	unsafe {
		a_ptr := a.str
		b_ptr := b.str
		for i in 0 .. a.len {
			if a_ptr[i] != b_ptr[i] {
				return false
			}
		}
	}
	return true
}

// Optimized header field lookup in static table
pub fn lookup_static_table_fast(name string, value string) int {
	// Use binary search for common headers
	match name {
		':authority' {
			return 1
		}
		':method' {
			if value == 'GET' {
				return 2
			}
			if value == 'POST' {
				return 3
			}
		}
		':path' {
			if value == '/' {
				return 4
			}
			if value == '/index.html' {
				return 5
			}
		}
		':scheme' {
			if value == 'http' {
				return 6
			}
			if value == 'https' {
				return 7
			}
		}
		':status' {
			match value {
				'200' { return 8 }
				'204' { return 9 }
				'206' { return 10 }
				'304' { return 11 }
				'400' { return 12 }
				'404' { return 13 }
				'500' { return 14 }
				else {}
			}
		}
		'content-type' {
			return 31
		}
		'content-length' {
			return 28
		}
		else {}
	}

	// Fallback to linear search
	for i, entry in static_table {
		if entry.name == name && entry.value == value {
			return i
		}
	}

	return 0
}

// Memory-efficient frame buffer
pub struct FrameBuffer {
mut:
	data   []u8
	offset int
}

pub fn new_frame_buffer(size int) FrameBuffer {
	return FrameBuffer{
		data:   []u8{len: size, cap: size}
		offset: 0
	}
}

pub fn (mut fb FrameBuffer) reset() {
	fb.offset = 0
}

pub fn (mut fb FrameBuffer) write(data []u8) bool {
	if fb.offset + data.len > fb.data.len {
		return false
	}

	for i, b in data {
		fb.data[fb.offset + i] = b
	}
	fb.offset += data.len
	return true
}

pub fn (fb FrameBuffer) bytes() []u8 {
	return fb.data[..fb.offset]
}

// Connection pool for reusing connections
pub struct ConnectionPool {
mut:
	connections map[string]&PooledConnection
	max_idle    int
}

@[heap]
struct PooledConnection {
mut:
	addr      string
	last_used i64
	in_use    bool
}

pub fn new_connection_pool(max_idle int) ConnectionPool {
	return ConnectionPool{
		max_idle: max_idle
	}
}

pub fn (mut cp ConnectionPool) get(addr string) ?&PooledConnection {
	if addr in cp.connections {
		mut conn := cp.connections[addr] or { return none }
		if !conn.in_use {
			conn.in_use = true
			return conn
		}
	}
	return none
}

pub fn (mut cp ConnectionPool) put(addr string, conn &PooledConnection) {
	if cp.connections.len < cp.max_idle {
		cp.connections[addr] = conn
	}
}

// Statistics for performance monitoring
pub struct PerformanceStats {
pub mut:
	total_requests       u64
	successful_requests  u64
	failed_requests      u64
	total_bytes_sent     u64
	total_bytes_received u64
	total_time_ms        u64
	min_time_ms          u64 = 999999
	max_time_ms          u64
}

pub fn (mut s PerformanceStats) record_request(success bool, bytes_sent int, bytes_received int, time_ms u64) {
	s.total_requests++
	if success {
		s.successful_requests++
	} else {
		s.failed_requests++
	}
	s.total_bytes_sent += u64(bytes_sent)
	s.total_bytes_received += u64(bytes_received)
	s.total_time_ms += time_ms

	if time_ms < s.min_time_ms {
		s.min_time_ms = time_ms
	}
	if time_ms > s.max_time_ms {
		s.max_time_ms = time_ms
	}
}

pub fn (s PerformanceStats) avg_time_ms() f64 {
	if s.total_requests == 0 {
		return 0.0
	}
	return f64(s.total_time_ms) / f64(s.total_requests)
}

pub fn (s PerformanceStats) success_rate() f64 {
	if s.total_requests == 0 {
		return 0.0
	}
	return f64(s.successful_requests) / f64(s.total_requests) * 100.0
}

pub fn (s PerformanceStats) print() {
	println('Performance Statistics:')
	println('  Total requests: ${s.total_requests}')
	println('  Successful: ${s.successful_requests}')
	println('  Failed: ${s.failed_requests}')
	println('  Success rate: ${s.success_rate():.2f}%')
	println('  Total bytes sent: ${s.total_bytes_sent}')
	println('  Total bytes received: ${s.total_bytes_received}')
	println('  Average time: ${s.avg_time_ms():.2f}ms')
	println('  Min time: ${s.min_time_ms}ms')
	println('  Max time: ${s.max_time_ms}ms')
}
