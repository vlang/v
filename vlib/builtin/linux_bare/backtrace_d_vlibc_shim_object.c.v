module builtin

#flag @DIR/libc_backtrace_abi.c

struct BareTraceState {
mut:
	buffer   uint
	capacity i32
	count    i32
}

@[export: 'v_trace_frame'; markused]
fn bare_trace_frame(state uint, pc uint) i32 {
	mut trace := unsafe { &BareTraceState(state) }
	if trace.count >= 0 {
		if pc == 0 { return 1 }
		unsafe { (&uint(trace.buffer))[trace.count] = pc }
	}
	trace.count++
	return i32(trace.count >= trace.capacity)
}

struct BareTraceLine {
mut:
	file        uint
	symbol      uint
	offset      [18]u8
	address     [18]u8
	offset_len  int
	address_len int
}

fn bare_trace_hex(value uint, mut digits [18]u8) int {
	alphabet := c'0123456789abcdef'
	mut n := value
	mut used := 0
	for {
		digits[17 - used] = unsafe { alphabet[n & 15] }
		used++
		n >>= 4
		if n == 0 { break }
	}
	digits[17 - used] = `x`
	digits[16 - used] = `0`
	return used + 2
}

fn bare_trace_line(pc uint, mut line BareTraceLine) {
	mut info := [4]uint{}
	bare_dynamic_address(pc, uint(&info[0]))
	line.file = info[0]
	line.symbol = info[2]
	base := if info[3] != 0 { info[3] } else { info[1] }
	line.offset_len = bare_trace_hex(pc - base, mut line.offset)
	line.address_len = bare_trace_hex(pc, mut line.address)
}

// With no destination, measure the line. With a descriptor, write directly
// from stack and loader storage so the signal diagnostic path needs no heap.
fn bare_trace_render(line &BareTraceLine, fd i32, destination uint) uint {
	mut parts := [8]uint{}
	mut sizes := [8]uint{}
	parts = [line.file, uint(c'('), line.symbol, uint(c'+'), uint(&line.offset[18 - line.offset_len]),
		uint(c') ['), uint(&line.address[18 - line.address_len]), uint(c']')]!
	for i in 0 .. 8 {
		sizes[i] = if i == 4 {
			uint(line.offset_len)
		} else if i == 6 {
			uint(line.address_len)
		} else if parts[i] == 0 {
			uint(0)
		} else {
			bare_string_chr_nul(parts[i], 0) - parts[i]
		}
	}
	mut total := uint(0)
	for i in 0 .. 8 {
		if sizes[i] == 0 { continue }
		if fd >= 0 {
			mut sent := uint(0)
			for sent < sizes[i] {
				n := i64(sys_call3(bf_nr_write, u64(fd), u64(parts[i] + sent), u64(sizes[i] - sent)))
				if n == -i64(C.EINTR) { continue }
				if n <= 0 { return total }
				sent += uint(n)
			}
		} else if destination != 0 {
			unsafe { vmemcpy(voidptr(destination + total), voidptr(parts[i]), int(sizes[i])) }
		}
		total += sizes[i]
	}
	return total
}

@[export: 'v_trace_symbols_fd'; markused]
fn bare_trace_symbols_fd(buffer uint, count i32, fd i32) {
	if fd < 0 { return }
	for i in 0 .. count {
		mut line := BareTraceLine{}
		bare_trace_line(unsafe { (&uint(buffer))[i] }, mut line)
		bare_trace_render(&line, fd, 0)
		sys_call3(bf_nr_write, u64(fd), u64(c'\n'), 1)
	}
}

@[export: 'v_trace_symbols'; markused]
fn bare_trace_symbols(buffer uint, count i32) uint {
	if count < 0 { return 0 }
	mut total := uint(count) * sizeof(uint)
	lines := unsafe { &BareTraceLine(__calloc(uint(count), sizeof(BareTraceLine))) }
	if lines == unsafe { nil } { return 0 }
	defer { unsafe { __free(lines) } }
	for i in 0 .. count {
		mut line := unsafe { &lines[i] }
		bare_trace_line(unsafe { (&uint(buffer))[i] }, mut line)
		length := bare_trace_render(line, -1, 0)
		if length >= ~uint(0) - total {
			bf_native_errno_cell = i32(C.ENOMEM)
			return 0
		}
		total += length + 1
	}
	output := uint(unsafe { __malloc(total) })
	if output == 0 { return 0 }
	mut next := output + uint(count) * sizeof(uint)
	for i in 0 .. count {
		unsafe { (&uint(output))[i] = next }
		next += bare_trace_render(unsafe { &lines[i] }, -1, next)
		unsafe { *(&u8(next)) = 0 }
		next++
	}
	return output
}
