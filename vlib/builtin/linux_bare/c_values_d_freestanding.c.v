module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	@[export: 'v_native_byteorder_16'; markused]
	fn bare_native_byteorder_16(value u16) u16 { return (value >> 8) | (value << 8) }
	@[export: 'v_native_byteorder_32'; markused]
	fn bare_native_byteorder_32(value u32) u32 {
		return (value >> 24) | ((value >> 8) & 0xff00) | ((value << 8) & 0xff0000) | (value << 24)
	}

	@[export: 'v_native_device_major'; markused]
	fn bare_native_device_major(device u64) u32 {
		return u32((device >> 8) & 0xfff) | u32((device >> 32) & 0xfffff000)
	}
	@[export: 'v_native_device_minor'; markused]
	fn bare_native_device_minor(device u64) u32 {
		return u32(device & 0xff) | u32((device >> 12) & 0xffffff00)
	}
	@[export: 'v_native_device_make'; markused]
	fn bare_native_device_make(major u32, minor u32) u64 {
		return u64(minor & 0xff) | (u64(major & 0xfff) << 8) |
			(u64(minor & 0xffffff00) << 12) | (u64(major & 0xfffff000) << 32)
	}
	@[export: 'v_native_absolute_32'; markused]
	fn bare_native_absolute_32(value i32) i32 {
		return if value < 0 { i32(u32(0) - u32(value)) } else { value }
	}
	@[export: 'v_native_absolute_64'; markused]
	fn bare_native_absolute_64(value i64) i64 {
		return if value < 0 { i64(u64(0) - u64(value)) } else { value }
	}
	@[export: 'v_native_divide'; markused]
	fn bare_native_divide(numerator i64, denominator i64, output uint) {
		unsafe {
			(&i64(output))[0] = numerator / denominator
			(&i64(output))[1] = numerator % denominator
		}
	}
	@[export: 'v_native_memset_explicit'; markused]
	fn bare_native_memset_explicit(output uint, value i32, count uint) uint {
		unsafe { vmemset(voidptr(output), int(u8(value)), int(count)) }
		// A compiler barrier observes the destination, preventing dead-store
		// removal even when callers no longer read a wiped secret.
		$if amd64 {
			asm volatile amd64 { nop ; ; r (output) ; memory }
		} $else {
			asm volatile arm64 { nop ; ; r (output) ; memory }
		}
		return output
	}
	@[export: 'v_native_basename'; markused]
	fn bare_native_basename(address uint) uint {
		if address == 0 || unsafe { *(&u8(address)) } == 0 { return uint(c'.') }
		mut length := bare_string_chr_nul(address, 0) - address
		for length > 1 && unsafe { (&u8(address))[length - 1] } == `/` { length-- }
		unsafe { (&u8(address))[length] = 0 }
		mut start := length
		for start > 0 && unsafe { (&u8(address))[start - 1] } != `/` { start-- }
		return if start == length { address } else { address + start }
	}
}
