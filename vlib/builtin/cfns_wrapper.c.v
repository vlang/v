module builtin

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
pub fn vmemcpy(dest voidptr, const_src voidptr, n isize) voidptr {
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
	if n == 0 || u64(dest) <= 0xFFFF || u64(const_src) <= 0xFFFF {
		return dest
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
pub fn vmemmove(dest voidptr, const_src voidptr, n isize) voidptr {
	$if trace_vmemmove ? {
		C.fprintf(C.stderr, c'vmemmove dest: %p src: %p n: %6ld\n', dest, const_src, n)
	}
	if n == 0 || u64(dest) <= 0xFFFF || u64(const_src) <= 0xFFFF {
		return dest
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
pub fn vmemcmp(const_s1 voidptr, const_s2 voidptr, n isize) int {
	$if trace_vmemcmp ? {
		C.fprintf(C.stderr, c'vmemcmp s1: %p s2: %p n: %6ld\n', const_s1, const_s2, n)
	}
	if n == 0 || u64(const_s1) <= 0xFFFF || u64(const_s2) <= 0xFFFF {
		return 0
	}
	unsafe {
		return C.memcmp(const_s1, const_s2, n)
	}
}

// vmemset fills the first `n` bytes of the memory area pointed to by `s`,
// with the constant byte `c`. It returns a pointer to the memory area `s`.
@[inline; unsafe]
pub fn vmemset(s voidptr, c int, n isize) voidptr {
	$if trace_vmemset ? {
		C.fprintf(C.stderr, c'vmemset s: %p c: %d n: %6ld\n', s, c, n)
	}
	$if trace_vmemset_nulls ? {
		if s == unsafe { 0 } {
			C.fprintf(C.stderr, c'vmemset null pointers passed s: %p, c: %6d, n: %6ld\n', s, c, n)
			print_backtrace()
		}
	}
	if n == 0 || u64(s) <= 0xFFFF {
		return s
	}
	unsafe {
		return C.memset(s, c, n)
	}
}

type FnSortCB = fn (const_a voidptr, const_b voidptr) int

@[inline; unsafe]
fn vsort_ptr_at(base voidptr, index usize, size usize) voidptr {
	return unsafe { voidptr(&u8(base) + index * size) }
}

@[unsafe]
fn vstable_sort_merge(source voidptr, dest voidptr, left usize, mid usize, right usize, size usize, sort_cb FnSortCB) {
	mut left_index := left
	mut right_index := mid
	mut dest_index := left
	for left_index < mid && right_index < right {
		left_ptr := vsort_ptr_at(source, left_index, size)
		right_ptr := vsort_ptr_at(source, right_index, size)
		if sort_cb(left_ptr, right_ptr) <= 0 {
			vmemcpy(vsort_ptr_at(dest, dest_index, size), left_ptr, isize(size))
			left_index++
		} else {
			vmemcpy(vsort_ptr_at(dest, dest_index, size), right_ptr, isize(size))
			right_index++
		}
		dest_index++
	}
	if left_index < mid {
		vmemcpy(vsort_ptr_at(dest, dest_index, size), vsort_ptr_at(source, left_index, size),
			isize((mid - left_index) * size))
	}
	if right_index < right {
		vmemcpy(vsort_ptr_at(dest, dest_index, size), vsort_ptr_at(source, right_index, size),
			isize((right - right_index) * size))
	}
}

@[inline; unsafe]
fn vqsort(base voidptr, nmemb usize, size usize, sort_cb FnSortCB) {
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
	total_size := isize(nmemb * size)
	buffer := malloc(total_size)
	defer {
		free(buffer)
	}
	mut source := base
	mut dest := voidptr(buffer)
	mut width := usize(1)
	for width < nmemb {
		mut left := usize(0)
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
