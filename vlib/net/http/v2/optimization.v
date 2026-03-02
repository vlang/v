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

// new_buffer_pool creates a new buffer pool for reducing memory allocations
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

// get gets a buffer from the pool or creates a new one if empty
pub fn (mut p BufferPool) get() []u8 {
	if p.buffers.len > 0 {
		buf := p.buffers[p.buffers.len - 1]
		p.buffers = p.buffers[..p.buffers.len - 1]
		return buf
	}
	return []u8{len: p.size, cap: p.size}
}

// put returns a buffer to the pool for reuse after clearing it
pub fn (mut p BufferPool) put(buf []u8) {
	if buf.cap == p.size {
		// Reuse the buffer's backing memory by trimming length to zero
		mut b := unsafe { buf }
		b.trim(0)
		p.buffers << b
	}
}

// encode_frame_optimized encodes a frame into a pre-allocated buffer for improved performance.
// Returns the number of bytes written, or 0 if the buffer is too small.
fn encode_frame_optimized(frame Frame, mut buf []u8) int {
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
		copy(mut buf[9..], frame.payload)
	}

	return required_size
}

// encode_optimized performs HPACK encoding with buffer reuse for better performance.
// It searches the static and dynamic tables for exact or name-only matches and
// uses RFC 7541-compliant multi-byte integer encoding for indices >= 127.
// Updates the dynamic table when emitting literal representations.
// Returns the number of bytes written to the buffer.
pub fn (mut e Encoder) encode_optimized(headers []HeaderField, mut buf []u8) int {
	mut offset := 0

	for header in headers {
		mut found_exact_idx := 0
		mut found_name_idx := 0

		// Search static table for exact or name-only match
		for i, entry in static_table {
			if entry.name == header.name {
				if entry.value == header.value {
					found_exact_idx = i
					break
				} else if found_name_idx == 0 {
					found_name_idx = i
				}
			}
		}

		// Search dynamic table if no static exact match found
		if found_exact_idx == 0 {
			for i := 0; i < e.dynamic_table.entries.len; i++ {
				entry := e.dynamic_table.entries[i]
				if entry.name == header.name {
					dyn_idx := static_table.len + i
					if entry.value == header.value {
						found_exact_idx = dyn_idx
						break
					} else if found_name_idx == 0 {
						found_name_idx = dyn_idx
					}
				}
			}
		}

		if found_exact_idx > 0 {
			// Indexed header field (RFC 7541 Section 6.1): prefix 7 bits, flag 0x80
			encoded_len := encode_integer(u64(found_exact_idx), 7, mut buf, offset)
			buf[offset] |= 0x80
			offset += encoded_len
		} else if found_name_idx > 0 {
			// Literal with incremental indexing, indexed name (RFC 7541 Section 6.2.1)
			// prefix 6 bits, flag 0x40
			encoded_len := encode_integer(u64(found_name_idx), 6, mut buf, offset)
			buf[offset] |= 0x40
			offset += encoded_len

			// Encode value as a plain string (length + bytes)
			value_len := header.value.len
			if offset + 1 + value_len > buf.len {
				return offset
			}
			buf[offset] = u8(value_len)
			offset++
			for b in header.value.bytes() {
				buf[offset] = b
				offset++
			}
			e.dynamic_table.add(header)
		} else {
			// Literal with incremental indexing, new name (RFC 7541 Section 6.2.1)
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
			e.dynamic_table.add(header)
		}
	}

	return offset
}

// encode_integer encodes a variable-length integer using fast encoding.
// Returns the number of bytes written to the buffer.
pub fn encode_integer(value u64, prefix_bits u8, mut buf []u8, offset int) int {
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

// lookup_static_table_fast performs a fast lookup of header fields in the static table.
// Returns the index of the header field, or 0 if not found.
fn lookup_static_table_fast(name string, value string) int {
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

// new_frame_buffer creates a new frame buffer with the specified size.
pub fn new_frame_buffer(size int) FrameBuffer {
	return FrameBuffer{
		data:   []u8{len: size, cap: size}
		offset: 0
	}
}

// reset resets the frame buffer offset to zero, making it ready for new data.
@[inline]
pub fn (mut fb FrameBuffer) reset() {
	fb.offset = 0
}

// write writes data to the frame buffer using bulk copy. Returns false if there is not enough space.
@[inline]
pub fn (mut fb FrameBuffer) write(data []u8) bool {
	if fb.offset + data.len > fb.data.len {
		return false
	}

	if data.len > 0 {
		copy(mut fb.data[fb.offset..], data)
	}
	fb.offset += data.len
	return true
}

// bytes returns the written bytes from the frame buffer.
@[inline]
pub fn (fb FrameBuffer) bytes() []u8 {
	return fb.data[..fb.offset]
}

// Connection pool for reusing connections.
// TODO: integrate with HTTP/2 client for connection reuse
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

// new_connection_pool creates a new connection pool with the specified maximum idle connections.
pub fn new_connection_pool(max_idle int) ConnectionPool {
	return ConnectionPool{
		max_idle: max_idle
	}
}

// get retrieves a connection from the pool for the given address if available.
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

// put returns a connection to the pool for reuse.
pub fn (mut cp ConnectionPool) put(addr string, conn &PooledConnection) {
	if cp.connections.len < cp.max_idle {
		cp.connections[addr] = conn
	}
}

// Statistics for performance monitoring
pub struct Stats {
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

// record_request records statistics for a single request.
pub fn (mut s Stats) record_request(success bool, bytes_sent int, bytes_received int, time_ms u64) {
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

// avg_time_ms calculates and returns the average request time in milliseconds.
pub fn (s Stats) avg_time_ms() f64 {
	if s.total_requests == 0 {
		return 0.0
	}
	return f64(s.total_time_ms) / f64(s.total_requests)
}

// success_rate calculates and returns the request success rate as a percentage.
pub fn (s Stats) success_rate() f64 {
	if s.total_requests == 0 {
		return 0.0
	}
	return f64(s.successful_requests) / f64(s.total_requests) * 100.0
}

// print displays the performance statistics to stdout.
pub fn (s Stats) print() {
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
