module builtin

const array_alignment_shift = sizeof(ArrayFlags) * 8 - 8
const array_alignment_mask = u32(63) << array_alignment_shift

@[inline]
fn (a &array) data_alignment() usize {
	exponent := (u32(a.flags) & array_alignment_mask) >> array_alignment_shift
	return if exponent == 0 { usize(16) } else { usize(1) << exponent }
}

fn data_alignment_exponent(alignment usize) u8 {
	if alignment <= 16 {
		return 0
	}
	mut exponent := u8(0)
	mut remaining := alignment
	for remaining > 1 {
		remaining >>= 1
		exponent++
	}
	return exponent
}

fn (mut a array) set_data_alignment(alignment usize) {
	exponent := u32(data_alignment_exponent(alignment))
	bits := u32(a.flags) & ~array_alignment_mask
	a.flags = unsafe { ArrayFlags(bits | (exponent << array_alignment_shift)) }
}

@[inline]
fn (a &array) storage_flags(noscan bool) ArrayFlags {
	mut flags := u32(a.flags) & array_alignment_mask
	flags |= u32(ArrayFlags.managed)
	if noscan {
		flags |= u32(ArrayFlags.noscan_data)
	}
	return unsafe { ArrayFlags(flags) }
}

fn alloc_array_data_aligned(total_size u64, alignment usize, zero bool) voidptr {
	size := u64(array_data_header_size()) + u64(alignment - 1)
	allocation_size := size + __at_least_one(total_size)
	raw := if zero {
		vcalloc(allocation_size)
	} else {
		unsafe { malloc_uninit(allocation_size) }
	}
	header_size := usize(array_data_header_size())
	padding := (alignment - (usize(raw) + header_size) % alignment) % alignment
	unsafe {
		data := &u8(raw) + header_size + padding
		header := &ArrayDataHeader(data - header_size)
		header.allocation = raw
		header.has_slices = false
		header.retained_fixed_views = false
		return data
	}
}

fn __new_array_aligned(mylen int, capacity int, element_size int, alignment usize) array {
	if alignment <= 16 {
		return __new_array(mylen, capacity, element_size)
	}
	panic_on_negative_len(mylen)
	panic_on_negative_cap(capacity)
	mut result := array{
		element_size: element_size
		len:          mylen
		cap:          if capacity < mylen { mylen } else { capacity }
		flags:        .managed
	}
	result.set_data_alignment(alignment)
	if result.cap > 0 {
		size := u64(result.cap) * u64(element_size)
		result.data = alloc_array_data_aligned(size, alignment, mylen > 0)
	}
	return result
}

fn new_array_from_c_array_aligned(length int, capacity int, element_size int, source voidptr, alignment usize) array {
	panic_on_negative_len(length)
	panic_on_negative_cap(capacity)
	mut result := __new_array_aligned(0, if capacity < length { length } else { capacity },
		element_size, alignment)
	result.len = length
	if length > 0 {
		unsafe { vmemcpy(result.data, source, u64(length) * u64(element_size)) }
	}
	return result
}

fn new_array_from_c_array_no_alloc_aligned(length int, capacity int, element_size int, source voidptr, alignment usize) array {
	mut result := new_array_from_c_array_no_alloc(length, capacity, element_size, source)
	result.set_data_alignment(alignment)
	return result
}
