module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include <fenv.h>

	@[export: 'v_native_float_sign'; markused]
	fn bare_native_float_sign(address uint, sign_address uint, precision i32) {
		index := match precision {
			24 { uint(3) }
			53 { uint(7) }
			64 { uint(9) }
			else { uint(15) }
		}
		// Only the native sign bit changes; NaN payloads and signed zero survive.
		sign := if sign_address == 0 {
			u8(0)
		} else {
			unsafe { *(&u8(sign_address + index)) } & 128
		}
		unsafe { *(&u8(address + index)) = (*(&u8(address + index)) & 127) | sign }
	}
	// C passes native floating values by address so extended precision never
	// crosses V's f64 ABI. Bit normalization is exact under every rounding mode.
	@[export: 'v_native_frexp'; markused]
	fn bare_native_frexp(address uint, precision i32, exponent_address uint) i32 {
		mut exponent := i32(0)
		mut native_special := false
		if precision == 24 {
			mut bits := unsafe { *(&u32(address)) }
			encoded := i32((bits >> 23) & 255)
			mut fraction := bits & 0x7fffff
			if encoded == 255 {
				native_special = true
			} else if encoded != 0 || fraction != 0 {
				exponent = encoded - 126
				if encoded == 0 {
					exponent = -125
					for fraction & 0x800000 == 0 {
						fraction <<= 1
						exponent--
					}
				}
				bits = (bits & 0x80000000) | 0x3f000000 | (fraction & 0x7fffff)
				unsafe { *(&u32(address)) = bits }
			}
		} else if precision == 53 {
			mut bits := unsafe { *(&u64(address)) }
			encoded := i32((bits >> 52) & 2047)
			mut fraction := bits & 0xfffffffffffff
			if encoded == 2047 {
				native_special = true
			} else if encoded != 0 || fraction != 0 {
				exponent = encoded - 1022
				if encoded == 0 {
					exponent = -1021
					for fraction & 0x10000000000000 == 0 {
						fraction <<= 1
						exponent--
					}
				}
				bits = (bits & 0x8000000000000000) | 0x3fe0000000000000 | (fraction & 0xfffffffffffff)
				unsafe { *(&u64(address)) = bits }
			}
		} else if precision == 64 {
			mut fraction := unsafe { *(&u64(address)) }
			bits := unsafe { *(&u16(address + 8)) }
			encoded := i32(bits & 32767)
			if encoded == 32767 {
				native_special = true
			} else if fraction != 0 {
				exponent = encoded - 16382
				if encoded == 0 {
					exponent = -16381
					for fraction & 0x8000000000000000 == 0 {
						fraction <<= 1
						exponent--
					}
				}
				unsafe {
					*(&u64(address)) = fraction
					*(&u16(address + 8)) = (bits & 0x8000) | 0x3ffe
				}
			}
		} else if precision == 113 {
			mut low := unsafe { *(&u64(address)) }
			bits := unsafe { *(&u64(address + 8)) }
			encoded := i32((bits >> 48) & 32767)
			mut high := bits & 0xffffffffffff
			if encoded == 32767 {
				// Software binary128 addition does not reliably raise hardware exceptions.
				if (high != 0 || low != 0) && high & 0x800000000000 == 0 {
					unsafe { *(&u64(address + 8)) = bits | 0x800000000000 }
					bare_fenv_raise(i32(C.FE_INVALID))
				}
			} else if encoded != 0 || high != 0 || low != 0 {
				exponent = encoded - 16382
				if encoded == 0 {
					exponent = -16381
					for high & 0x1000000000000 == 0 {
						high = (high << 1) | (low >> 63)
						low <<= 1
						exponent--
					}
				}
				unsafe {
					*(&u64(address)) = low
					*(&u64(address + 8)) = (bits & 0x8000000000000000) | 0x3ffe000000000000 | (high & 0xffffffffffff)
				}
			}
		}
		unsafe { *(&i32(exponent_address)) = exponent }
		return i32(native_special)
	}

	fn bare_float_control_read() (u64, u32) {
		$if amd64 {
			mut control := u16(0)
			mut vector := u32(0)
			control_address := &control
			vector_address := &vector
			asm volatile amd64 {
				fnstcw [control_address]
				stmxcsr [vector_address]
				; ; r (control_address)
				    r (vector_address)
				; memory
			}
			return u64(control), vector
		} $else {
			mut control := u64(0)
			asm volatile arm64 {
				mrs control, fpcr
				; =r (control)
				; ; memory
			}
			return control, 0
		}
	}

	fn bare_float_control_write(control u64, vector u32) {
		$if amd64 {
			word := u16(control)
			control_address := &word
			vector_address := &vector
			asm volatile amd64 {
				fldcw [control_address]
				ldmxcsr [vector_address]
				; ; r (control_address)
				    r (vector_address)
				; memory
			}
		} $else {
			asm volatile arm64 {
				msr fpcr, control
				; ; r (control)
				; memory
			}
		}
	}

	@[export: 'v_native_round_get'; markused]
	fn bare_native_round_get() i32 {
		control, _ := bare_float_control_read()
		$if amd64 {
			return i32(control & 0xc00)
		} $else {
			return i32(control & 0xc00000)
		}
	}

	@[export: 'v_native_round_set'; markused]
	fn bare_native_round_set(mode i32) i32 {
		$if amd64 {
			if mode & ~i32(0xc00) != 0 { return 1 }
			control, vector := bare_float_control_read()
			bare_float_control_write((control & ~u64(0xc00)) | u64(mode), (vector & ~u32(0x6000)) | (u32(mode) << 3))
		} $else {
			if mode & ~i32(0xc00000) != 0 { return 1 }
			control, vector := bare_float_control_read()
			bare_float_control_write((control & ~u64(0xc00000)) | u64(mode), vector)
		}
		return 0
	}
}
