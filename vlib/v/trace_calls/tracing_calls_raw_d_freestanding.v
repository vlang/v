module trace_calls

import strconv

$if macos && amd64 && !native {
	// A trace may run inside an already locked allocator. Both byte buffers and
	// their borrowed array/string descriptors must remain on this call's stack.
	@[manualfree; no_profile; no_trace]
	fn write_raw_trace_row(tid u32, ns i64, stack_size i64, name string) {
		mut prefix := [80]u8{}
		mut used := 0
		for byte in '> trace ' {
			prefix[used] = byte
			used++
		}
		mut digits := [21]u8{}
		// vbytes borrows writable storage. manualfree keeps this stack view from
		// being treated as an owned array by autofree.
		mut number := unsafe { (&digits[0]).vbytes(digits.len) }
		count := strconv.write_dec_u(u64(tid), mut number)
		used = append_raw_trace_number(mut prefix, used, &digits[0], count)
		ns_count := strconv.write_dec(ns, mut number)
		used = append_raw_trace_number(mut prefix, used, &digits[0], ns_count)
		stack_count := strconv.write_dec(stack_size, mut number)
		used = append_raw_trace_number(mut prefix, used, &digits[0], stack_count)
		// The complete prefix is at most 61 bytes. The borrowed string is used
		// synchronously; the function name is streamed without copying/truncation.
		eprint(unsafe { (&prefix[0]).vstring_literal_with_len(used) })
		eprint(name)
		eprint('\n')
	}

	@[no_profile; no_trace]
	fn append_raw_trace_number(mut prefix [80]u8, offset int, digits &u8, count int) int {
		mut next := offset
		mut padding := 8 - count
		for padding > 0 {
			prefix[next] = ` `
			next++
			padding--
		}
		for i in 0 .. count {
			// The callers supply at most 20 digits from their 21-byte buffer.
			prefix[next] = unsafe { digits[i] }
			next++
		}
		prefix[next] = ` `
		return next + 1
	}
}
