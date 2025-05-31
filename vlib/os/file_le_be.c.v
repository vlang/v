module os

// write_le writes an unsigned number value to the file.
// It assumes that the value should be stored in a *little endian order*.
// If the machine is a big endian one, it will first convert the big endian value to a little endian one.
// It is safe to use as a cross platform way to write data, for which you have to use a predefined order (defined in a file format spec).
pub fn (mut f File) write_le[T](data T) ! {
	mut serialized := data
	$if big_endian {
		serialized = swap_bytes(serialized)
	}
	C.errno = 0 // needed for tcc
	check_fwrite(C.fwrite(voidptr(&serialized), sizeof(T), 1, f.cfile))!
}

// write_be writes an unsigned number value to the file.
// It assumes that the value should be stored in a *big endian order*.
// If the machine is a little endian one, it will first convert the little endian value to a big endian one.
// It is safe to use as a cross platform way to write data, for which you have to use a predefined order (defined in a file format spec).
pub fn (mut f File) write_be[T](data T) ! {
	mut serialized := data
	$if little_endian {
		serialized = swap_bytes(serialized)
	}
	C.errno = 0 // needed for tcc
	check_fwrite(C.fwrite(voidptr(&serialized), sizeof(T), 1, f.cfile))!
}

// read_le reads an unsigned number value from the file.
// It assumes that the value was stored in a *little endian order*.
// If the machine is a big endian one, it will convert the value to a big endian one.
// It is intended to use as a cross platform way to read data, for which the order is known (from the file format spec).
pub fn (mut f File) read_le[T]() !T {
	mut serialized := T(0)
	C.errno = 0 // needed for tcc
	check_fread(C.fread(voidptr(&serialized), sizeof(T), 1, f.cfile))!
	$if big_endian {
		return swap_bytes(serialized)
	}
	return serialized
}

// read_be reads an unsigned number value from the file.
// It assumes that the value was stored in a *big endian order*.
// If the machine is a little endian one, it will convert the value to a little endian one.
// It is intended to use as a cross platform way to read data, for which the order is known (from the file format spec).
pub fn (mut f File) read_be[T]() !T {
	mut serialized := T(0)
	C.errno = 0 // needed for tcc
	check_fread(C.fread(voidptr(&serialized), sizeof(T), 1, f.cfile))!
	$if little_endian {
		return swap_bytes(serialized)
	}
	return serialized
}

// write_u8 writes a single byte value to the file `f`.
// Note: if possible, use some of the other APIs, that write larger chunks of data, before using write_u8/1.
pub fn (mut f File) write_u8(b u8) ! {
	C.errno = 0 // needed for tcc
	check_fwrite(C.fwrite(voidptr(&b), 1, 1, f.cfile))!
}

// read_u8 reads a single byte value from the file `f`.
// Note: if possible, use some of the other APIs, that read larger chunks of data, before using read_u8/1.
pub fn (mut f File) read_u8() !u8 {
	mut res := u8(0)
	C.errno = 0 // needed for tcc
	check_fread(C.fread(voidptr(&res), 1, 1, f.cfile))!
	return res
}

// private helpers

@[inline]
fn swap_bytes_u16(x u16) u16 {
	// vfmt off
	return ((x >> 8) & 0x00FF) |
	       ((x << 8) & 0xFF00)
	// vfmt on
}

@[inline]
fn swap_bytes_u32(x u32) u32 {
	// vfmt off
	return ((x >> 24) & 0x0000_00FF) |
	       ((x >> 8)  & 0x0000_FF00) |
	       ((x << 8)  & 0x00FF_0000) |
	       ((x << 24) & 0xFF00_0000)
	// vfmt on
}

@[inline]
fn swap_bytes_u64(x u64) u64 {
	// vfmt off
	return ((x >> 56) & 0x00000000_000000FF) |
	       ((x >> 40) & 0x00000000_0000FF00) |
	       ((x >> 24) & 0x00000000_00FF0000) |
	       ((x >> 8)  & 0x00000000_FF000000) |
	       ((x << 8)  & 0x000000FF_00000000) |
	       ((x << 24) & 0x0000FF00_00000000) |
	       ((x << 40) & 0x00FF0000_00000000) |
	       ((x << 56) & 0xFF000000_00000000)
	// vfmt on
}

fn swap_bytes[T](input T) T {
	$if T is u8 {
		return input
	} $else $if T is i8 {
		return input
	} $else $if T is byte {
		return input
	} $else $if T is u16 {
		return swap_bytes_u16(input)
	} $else $if T is u32 {
		return swap_bytes_u32(input)
	} $else $if T is u64 {
		return swap_bytes_u64(input)
	} $else $if T is i16 {
		return i16(swap_bytes_u16(u16(input)))
	} $else $if T is i32 {
		return i32(swap_bytes_u32(u32(input)))
	} $else $if T is int {
		return i32(swap_bytes_u32(u32(input)))
	} $else $if T is i64 {
		return i64(swap_bytes_u64(u64(input)))
	} $else {
		panic('type is not supported: ' + typeof[T]().str())
	}
}

fn check_cf(x usize, label string) ! {
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if x == 0 {
		return error(label)
	}
}

fn check_fwrite(x usize) ! {
	check_cf(x, 'fwrite')!
}

fn check_fread(x usize) ! {
	check_cf(x, 'fread')!
}
