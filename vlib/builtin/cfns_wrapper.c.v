module builtin

fn C.__builtin_ctzll(x u64) i32

fn C.__builtin_clzll(x u64) i32

// string_index_byte searches exactly size readable bytes, including embedded NULs.
@[inline]
fn string_index_byte(data &u8, size int, needle u8) int {
	if size <= 0 {
		return -1
	}
	$if freestanding || vinix || native || v2_native_windows_pe_minimal ? {
		for i in 0 .. size {
			if unsafe { data[i] } == needle {
				return i
			}
		}
		return -1
	} $else {
		$if prod && !tinyc && !msvc && !native && little_endian {
			if size >= 8 && size <= 32 {
				repeated := u64(needle) * u64(0x0101010101010101)
				mut offset := 0
				for {
					mut word := u64(0)
					// Each unaligned load remains entirely inside the readable span.
					unsafe { C.memcpy(&word, data + offset, 8) }
					diff := word ^ repeated
					mask := (diff - u64(0x0101010101010101)) & ~diff & u64(0x8080808080808080)
					if mask != 0 {
						// Borrow can mark later bytes, but the first marked byte is exact.
						return offset + int(C.__builtin_ctzll(mask) >> 3)
					}
					if offset + 8 == size {
						return -1
					}
					offset = if offset + 16 > size { size - 8 } else { offset + 8 }
				}
			}
		}
		found := unsafe { C.memchr(data, i32(needle), uint(size)) }
		if isnil(found) {
			return -1
		}
		return int(uint(found) - uint(data))
	}
}

// string_index_pair searches adjacent bytes without reading outside the span.
@[inline]
fn string_index_pair(data &u8, size int, first u8, second u8) int {
	$if prod && !tinyc && !msvc && !native && little_endian {
		if size >= 8 && size <= 32 {
			low_bits := u64(0x7f7f7f7f7f7f7f7f)
			repeated_a := u64(first) * u64(0x0101010101010101)
			repeated_b := u64(second) * u64(0x0101010101010101)
			mut offset := 0
			for {
				mut word := u64(0)
				// Seven adjacent pairs fit in this bounded, unaligned window.
				unsafe { C.memcpy(&word, data + offset, 8) }
				da := word ^ repeated_a
				// Exact masks prevent a borrow from creating a false adjacent match.
				za := ~(((da & low_bits) + low_bits) | da | low_bits)
				mut zb := za
				if first != second {
					db := word ^ repeated_b
					zb = ~(((db & low_bits) + low_bits) | db | low_bits)
				}
				mask := za & (zb >> 8)
				if mask != 0 {
					return offset + int(C.__builtin_ctzll(mask) >> 3)
				}
				if offset + 8 == size { return -1 }
				offset = if offset + 15 > size { size - 8 } else { offset + 7 }
			}
		}
	}
	last_start := size - 2
	for i := 0; i <= last_start; i++ {
		if unsafe { data[i] } != first {
			next := string_index_byte(unsafe { data + i + 1 }, last_start - i, first)
			if next < 0 { return -1 }
			i += next + 1
		}
		if unsafe { data[i + 1] } == second { return i }
	}
	return -1
}

// string_contains_byte checks readable bytes without computing a match position.
@[inline]
fn string_contains_byte(data &u8, size int, needle u8) bool {
	if size <= 0 {
		return false
	}
	$if freestanding || vinix || native || v2_native_windows_pe_minimal ? {
		return string_index_byte(data, size, needle) >= 0
	} $else {
		$if prod && !tinyc && !msvc && !native {
			if size <= 32 {
				if size >= 8 {
					ones := u64(0x0101010101010101)
					repeated := u64(needle) * ones
					mut head := u64(0)
					mut tail := u64(0)
					// Every unaligned load stays inside the readable span.
					unsafe {
						C.memcpy(&head, data, 8)
						C.memcpy(&tail, data + size - 8, 8)
					}
					head ^= repeated
					tail ^= repeated
					mut mask := ((head - ones) & ~head) | ((tail - ones) & ~tail)
					if size > 16 {
						unsafe {
							C.memcpy(&head, data + 8, 8)
							C.memcpy(&tail, data + size - 16, 8)
						}
						head ^= repeated
						tail ^= repeated
						mask |= ((head - ones) & ~head) | ((tail - ones) & ~tail)
					}
					return mask & u64(0x8080808080808080) != 0
				}
				if size >= 4 {
					ones := u32(0x01010101)
					repeated := u32(needle) * ones
					mut head := u32(0)
					mut tail := u32(0)
					unsafe {
						C.memcpy(&head, data, 4)
						C.memcpy(&tail, data + size - 4, 4)
					}
					head ^= repeated
					tail ^= repeated
					return (((head - ones) & ~head) | ((tail - ones) & ~tail)) & u32(0x80808080) != 0
				}
				return unsafe { data[0] == needle || data[size >> 1] == needle || data[size - 1] == needle }
			}
		}
		return !isnil(unsafe { C.memchr(data, i32(needle), uint(size)) })
	}
}

// string_last_index_byte searches complete words without reading outside the span.
@[inline]
fn string_last_index_byte(data &u8, size int, needle u8) int {
	mut end := size
	$if prod && !tinyc && !msvc && !native {
		repeated := u64(needle) * u64(0x0101010101010101)
		for end >= 8 {
			start := end - 8
			mut word := u64(0)
			// A fixed-size copy permits unaligned views and never crosses their bounds.
			unsafe { C.memcpy(&word, data + start, 8) }
			diff := word ^ repeated
			$if little_endian {
				// Unlike subtraction, this mask marks only zero bytes: a borrow from
				// an earlier match cannot create a false match at the high end.
				low_bits := u64(0x7f7f7f7f7f7f7f7f)
				mask := ~(((diff & low_bits) + low_bits) | diff | low_bits)
				if mask != 0 {
					return start + 7 - int(C.__builtin_clzll(mask) >> 3)
				}
			} $else {
				if (diff - u64(0x0101010101010101)) & ~diff & u64(0x8080808080808080) != 0 {
					for i := end - 1; i >= start; i-- {
						if unsafe { data[i] } == needle { return i }
					}
				}
			}
			end = start
		}
	}
	for i := end - 1; i >= 0; i-- {
		if unsafe { data[i] } == needle { return i }
	}
	return -1
}

// string_bytes_equal compares readable byte spans without allocating or reading past size.
@[inline]
fn string_bytes_equal(a &u8, b &u8, size int) bool {
	if a == b { return true }
	$if prod && !tinyc && !msvc && !native {
		if size >= 0 && size <= 16 {
			if size >= 8 {
				mut ah := u64(0)
				mut at := u64(0)
				mut bh := u64(0)
				mut bt := u64(0)
				// memcpy permits unaligned views; these fixed-size copies become loads.
				unsafe {
					C.memcpy(&ah, a, 8)
					C.memcpy(&at, a + size - 8, 8)
					C.memcpy(&bh, b, 8)
					C.memcpy(&bt, b + size - 8, 8)
				}
				return ((ah ^ bh) | (at ^ bt)) == 0
			}
			if size >= 4 {
				mut ah := u32(0)
				mut at := u32(0)
				mut bh := u32(0)
				mut bt := u32(0)
				unsafe {
					C.memcpy(&ah, a, 4)
					C.memcpy(&at, a + size - 4, 4)
					C.memcpy(&bh, b, 4)
					C.memcpy(&bt, b + size - 4, 4)
				}
				return ((ah ^ bh) | (at ^ bt)) == 0
			}
			if size == 0 {
				return true
			}
			return unsafe { ((a[0] ^ b[0]) | (a[size >> 1] ^ b[size >> 1]) | (a[size - 1] ^ b[size - 1])) == 0 }
		}
	}
	return unsafe { C.memcmp(a, b, uint(size)) == 0 }
}

// string_content_equal preserves vmemcmp's guarded and traced string comparison.
@[inline]
fn string_content_equal(a &u8, b &u8, size int) bool {
	$if trace_vmemcmp ? {
		return unsafe { vmemcmp(a, b, size) == 0 }
	}
	if size == 0 || uint(a) <= 0xFFFF || uint(b) <= 0xFFFF {
		return true
	}
	return string_bytes_equal(a, b, size)
}

// vstrlen returns the V length of the C string `s` (0 terminator is not counted).
// The C string is expected to be a &u8 pointer.
@[inline; unsafe]
pub fn vstrlen(s &u8) int {
	return int(unsafe { C.strlen(&char(s)) })
}

// vstrlen_char returns the V length of the C string `s` (0 terminator is not counted).
// The C string is expected to be a &char pointer.
@[inline; unsafe]
pub fn vstrlen_char(s &char) int {
	return int(unsafe { C.strlen(s) })
}

// vmemcpy copies n bytes from memory area src to memory area dest.
// The memory areas *MUST NOT OVERLAP*.  Use vmemmove, if the memory
// areas do overlap. vmemcpy returns a pointer to `dest`.
@[inline; unsafe]
pub fn vmemcpy(dest voidptr, const_src voidptr, n int) voidptr {
	$if trace_vmemcpy ? {
		C.fprintf(C.stderr, c'vmemcpy dest: %p src: %p n: %6ld\n', dest, const_src, n)
	}
	$if trace_vmemcpy_nulls ? {
		if dest == unsafe { 0 } || const_src == unsafe { 0 } {
			C.fprintf(C.stderr, c'vmemcpy null pointers passed, dest: %p src: %p n: %6ld\n', dest,
				const_src, n)
			print_backtrace()
		}
	}
	if n == 0 || uint(dest) <= 0xFFFF || uint(const_src) <= 0xFFFF {
		return dest
	}
	$if prod && !tinyc && !msvc && !native {
		if n > 0 && n <= 64 {
			// Fixed-size copies become unaligned loads/stores. The source and
			// destination do not overlap, so overlapping head/tail copies are safe.
			unsafe {
				dst := &u8(dest)
				src := &u8(const_src)
				if n >= 32 {
					C.memcpy(dst, src, 32)
					C.memcpy(dst + n - 32, src + n - 32, 32)
				} else if n >= 16 {
					C.memcpy(dst, src, 16)
					C.memcpy(dst + n - 16, src + n - 16, 16)
				} else if n >= 8 {
					C.memcpy(dst, src, 8)
					C.memcpy(dst + n - 8, src + n - 8, 8)
				} else if n >= 4 {
					C.memcpy(dst, src, 4)
					C.memcpy(dst + n - 4, src + n - 4, 4)
				} else {
					dst[0] = src[0]
					dst[n >> 1] = src[n >> 1]
					dst[n - 1] = src[n - 1]
				}
			}
			return dest
		}
	}
	unsafe {
		return C.memcpy(dest, const_src, n)
	}
}

// vmemmove copies n bytes from memory area `src` to memory area `dest`.
// The memory areas *MAY* overlap: copying takes place as though the bytes
// in `src` are first copied into a temporary array that does not overlap
// `src` or `dest`, and the bytes are then copied from the temporary array
// to `dest`. vmemmove returns a pointer to `dest`.
@[inline; unsafe]
pub fn vmemmove(dest voidptr, const_src voidptr, n int) voidptr {
	$if trace_vmemmove ? {
		C.fprintf(C.stderr, c'vmemmove dest: %p src: %p n: %6ld\n', dest, const_src, n)
	}
	if n == 0 || uint(dest) <= 0xFFFF || uint(const_src) <= 0xFFFF {
		return dest
	}
	$if prod && !tinyc && !msvc && !native {
		if n > 0 && n <= 64 {
			// Load both ends before storing either: the source and destination may overlap.
			unsafe {
				dst := &u8(dest)
				src := &u8(const_src)
				if n >= 32 {
					mut head := [32]u8{}
					mut tail := [32]u8{}
					C.memcpy(&head[0], src, 32)
					C.memcpy(&tail[0], src + n - 32, 32)
					C.memcpy(dst, &head[0], 32)
					C.memcpy(dst + n - 32, &tail[0], 32)
				} else if n >= 16 {
					mut head := [16]u8{}
					mut tail := [16]u8{}
					C.memcpy(&head[0], src, 16)
					C.memcpy(&tail[0], src + n - 16, 16)
					C.memcpy(dst, &head[0], 16)
					C.memcpy(dst + n - 16, &tail[0], 16)
				} else if n >= 8 {
					mut head := u64(0)
					mut tail := u64(0)
					C.memcpy(&head, src, 8)
					C.memcpy(&tail, src + n - 8, 8)
					C.memcpy(dst, &head, 8)
					C.memcpy(dst + n - 8, &tail, 8)
				} else if n >= 4 {
					mut head := u32(0)
					mut tail := u32(0)
					C.memcpy(&head, src, 4)
					C.memcpy(&tail, src + n - 4, 4)
					C.memcpy(dst, &head, 4)
					C.memcpy(dst + n - 4, &tail, 4)
				} else {
					head := src[0]
					middle := src[n >> 1]
					tail := src[n - 1]
					dst[0] = head
					dst[n >> 1] = middle
					dst[n - 1] = tail
				}
			}
			return dest
		}
	}
	unsafe {
		return C.memmove(dest, const_src, n)
	}
}

// vmemcmp compares the first n bytes (each interpreted as unsigned char)
// of the memory areas s1 and s2. It returns an integer less than, equal to,
// or greater than zero, if the first n bytes of s1 is found, respectively,
// to be less than, to match, or be greater than the first n bytes of s2.
// For a nonzero return value, the sign is determined by the sign of the
// difference between the first pair of bytes (interpreted as unsigned char)
// that differ in s1 and s2.
// If n is zero, the return value is zero.
// Do NOT use vmemcmp to compare security critical data, such as cryptographic
// secrets, because the required CPU time depends on the number of equal bytes.
// You should use a function that performs comparisons in constant time for
// this.
@[inline; unsafe]
pub fn vmemcmp(const_s1 voidptr, const_s2 voidptr, n int) int {
	$if trace_vmemcmp ? {
		C.fprintf(C.stderr, c'vmemcmp s1: %p s2: %p n: %6ld\n', const_s1, const_s2, n)
	}
	if n == 0 || uint(const_s1) <= 0xFFFF || uint(const_s2) <= 0xFFFF {
		return 0
	}
	unsafe {
		return C.memcmp(const_s1, const_s2, n)
	}
}

// vmemset fills the first `n` bytes of the memory area pointed to by `s`,
// with the constant byte `c`. It returns a pointer to the memory area `s`.
@[inline; unsafe]
pub fn vmemset(s voidptr, c int, n int) voidptr {
	$if trace_vmemset ? {
		C.fprintf(C.stderr, c'vmemset s: %p c: %d n: %6ld\n', s, c, n)
	}
	$if trace_vmemset_nulls ? {
		if s == unsafe { 0 } {
			C.fprintf(C.stderr, c'vmemset null pointers passed s: %p, c: %6d, n: %6ld\n', s, c, n)
			print_backtrace()
		}
	}
	if n == 0 || uint(s) <= 0xFFFF {
		return s
	}
	unsafe {
		return C.memset(s, c, n)
	}
}

type FnSortCB = fn (const_a voidptr, const_b voidptr) int

@[inline; unsafe]
fn vsort_ptr_at(base voidptr, index uint, size uint) voidptr {
	return unsafe { voidptr(&u8(base) + index * size) }
}

@[unsafe]
fn vstable_sort_merge(source voidptr, dest voidptr, left uint, mid uint, right uint, size uint, sort_cb FnSortCB) {
	mut left_index := left
	mut right_index := mid
	mut dest_index := left
	for left_index < mid && right_index < right {
		left_ptr := vsort_ptr_at(source, left_index, size)
		right_ptr := vsort_ptr_at(source, right_index, size)
		if sort_cb(left_ptr, right_ptr) <= 0 {
			vmemcpy(vsort_ptr_at(dest, dest_index, size), left_ptr, int(size))
			left_index++
		} else {
			vmemcpy(vsort_ptr_at(dest, dest_index, size), right_ptr, int(size))
			right_index++
		}
		dest_index++
	}
	if left_index < mid {
		vmemcpy(vsort_ptr_at(dest, dest_index, size), vsort_ptr_at(source, left_index, size),
			int((mid - left_index) * size))
	}
	if right_index < right {
		vmemcpy(vsort_ptr_at(dest, dest_index, size), vsort_ptr_at(source, right_index, size),
			int((right - right_index) * size))
	}
}

@[inline; unsafe]
fn vqsort(base voidptr, nmemb uint, size uint, sort_cb FnSortCB) {
	$if trace_vqsort ? {
		C.fprintf(C.stderr, c'vqsort base: %p, nmemb: %6uld, size: %6uld, sort_cb: %p\n', base,
			nmemb, size, sort_cb)
	}
	if nmemb < 2 || size == 0 {
		return
	}
	$if trace_vqsort_nulls ? {
		if base == unsafe { 0 } || voidptr(sort_cb) == unsafe { nil } {
			C.fprintf(C.stderr,
				c'vqsort null pointers passed base: %p, nmemb: %6uld, size: %6uld, sort_cb: %p\n',
				base, nmemb, size, sort_cb)
			print_backtrace()
		}
	}
	total_size := int(nmemb * size)
	buffer := malloc(total_size)
	defer {
		free(buffer)
	}
	mut source := base
	mut dest := voidptr(buffer)
	mut width := uint(1)
	for width < nmemb {
		mut left := uint(0)
		for left < nmemb {
			mid := if left + width < nmemb { left + width } else { nmemb }
			right := if left + width + width < nmemb { left + width + width } else { nmemb }
			vstable_sort_merge(source, dest, left, mid, right, size, sort_cb)
			left += width + width
		}
		mut tmp := source
		source = dest
		dest = tmp
		width += width
	}
	if source != base {
		vmemcpy(base, source, total_size)
	}
}
