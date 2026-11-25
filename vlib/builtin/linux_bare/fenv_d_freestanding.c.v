module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	fn bare_fenv_mask() u32 {
		$if amd64 {
			return 0x3d
		} $else {
			return 31
		}
	}

	// These pointers address the target toolchain's native fenv_t storage:
	// x87's 28-byte environment plus MXCSR on amd64, FPCR/FPSR on arm64.
	@[export: 'v_fenv_get'; markused]
	fn bare_fenv_get(output uint) {
		$if amd64 {
			vector := output + 28
			asm volatile amd64 {
				fnstenv [output]
				fldenv [output]
				stmxcsr [vector]
				; ; r (output)
				    r (vector)
				; memory
			}
		} $else {
			mut control := u64(0)
			mut status := u64(0)
			asm volatile arm64 {
				mrs control, fpcr
				mrs status, fpsr
				; =r (control)
				  =r (status)
				; ; memory
			}
			unsafe {
				(&u32(output))[0] = u32(control)
				(&u32(output))[1] = u32(status)
			}
		}
	}

	fn bare_fenv_load(input uint) {
		$if amd64 {
			vector := input + 28
			// All callers preserve the current x87 register tags and stack TOP.
			// Loading the control/status image never changes live FP registers.
			asm volatile amd64 {
				fldenv [input]
				ldmxcsr [vector]
				; ; r (input)
				    r (vector)
				; memory
			}
		} $else {
			control := u64(unsafe { (&u32(input))[0] })
			status := u64(unsafe { (&u32(input))[1] })
			asm volatile arm64 {
				msr fpcr, control
				msr fpsr, status
				; ; r (control)
				    r (status)
				; memory
			}
		}
	}

	@[export: 'v_fenv_flags'; markused]
	fn bare_fenv_flags(selected i32) u32 {
		$if amd64 {
			mut status := u16(0)
			mut vector := u32(0)
			status_address := &status
			vector_address := &vector
			asm volatile amd64 {
				fnstsw [status_address]
				stmxcsr [vector_address]
				; ; r (status_address)
				    r (vector_address)
				; memory
			}
			return (u32(status) | vector) & u32(selected) & bare_fenv_mask()
		} $else {
			mut status := u64(0)
			asm volatile arm64 { mrs status, fpsr ; =r (status) ; ; memory }
			return u32(status) & u32(selected) & bare_fenv_mask()
		}
	}

	@[export: 'v_fenv_flags_set'; markused]
	fn bare_fenv_flags_set(flags u32, requested i32) i32 {
		selected := u32(requested) & bare_fenv_mask()
		mut environment := [32]u8{}
		address := uint(&environment[0])
		bare_fenv_get(address)
		$if amd64 {
			// Setting an SSE sticky flag cannot trap. Clear obsolete x87 flags
			// while retaining the already-set ones, avoiding a deferred x87 trap.
			unsafe {
				*(&u16(address + 4)) &= ~u16(selected & ~flags)
				*(&u32(address + 28)) = (*(&u32(address + 28)) & ~selected) | (flags & selected)
			}
		} $else {
			unsafe { *(&u32(address + 4)) = (*(&u32(address + 4)) & ~selected) | (flags & selected) }
		}
		bare_fenv_load(address)
		return 0
	}

	@[export: 'v_fenv_set'; markused]
	fn bare_fenv_set(input uint) i32 {
		mut environment := [32]u8{}
		address := uint(&environment[0])
		bare_fenv_get(address)
		$if amd64 {
			unsafe {
				mut control := *(&u16(address)) & ~u16(0xf3f)
				mut flags := u16(0)
				mut vector := *(&u32(address + 28)) & ~u32(0xffff)
				if input == ~uint(0) || input == ~uint(1) {
					control |= if input == ~uint(0) { u16(0x33f) } else { u16(0x302) }
					vector |= if input == ~uint(0) { u32(0x1f80) } else { u32(0x100) }
					vmemset(voidptr(address + 12), 0, 16)
				} else {
					control |= *(&u16(input)) & 0xf3f
					flags = *(&u16(input + 4)) & 0x3f
					vector = *(&u32(input + 28))
					vmemcpy(voidptr(address + 12), voidptr(input + 12), 16)
				}
				*(&u16(address)) = control
				*(&u16(address + 4)) = (*(&u16(address + 4)) & ~u16(0x3f)) | flags
				*(&u32(address + 28)) = vector
			}
		} $else {
			if input == ~uint(0) || input == ~uint(1) {
				unsafe {
					(&u32(address))[0] = if input == ~uint(0) { u32(0) } else { u32(31 << 8) }
					(&u32(address))[1] = 0
				}
			} else {
				unsafe { vmemcpy(voidptr(address), voidptr(input), 8) }
			}
		}
		bare_fenv_load(address)
		return 0
	}

	@[export: 'v_fenv_hold'; markused]
	fn bare_fenv_hold(output uint) i32 {
		bare_fenv_get(output)
		mut environment := [32]u8{}
		address := uint(&environment[0])
		$if amd64 {
			unsafe {
				vmemcpy(voidptr(address), voidptr(output), 32)
				*(&u16(address)) |= 0x3f
				*(&u16(address + 4)) &= ~u16(0x80ff)
				*(&u32(address + 28)) = (*(&u32(address + 28)) & ~u32(0x3f)) | 0x1f80
			}
		} $else {
			unsafe {
				vmemcpy(voidptr(address), voidptr(output), 8)
				*(&u32(address)) &= ~u32(31 << 8)
				*(&u32(address + 4)) &= ~u32(31)
			}
		}
		bare_fenv_load(address)
		return 0
	}

	@[export: 'v_fenv_enabled'; markused]
	fn bare_fenv_enabled(requested i32, change i32) i32 {
		mask := u32(requested) & bare_fenv_mask()
		control, vector := bare_float_control_read()
		$if amd64 {
			old := u32(~control) & bare_fenv_mask()
			if change == 1 {
				bare_float_control_write(control & ~u64(mask), vector & ~(mask << 7))
			} else if change == -1 {
				bare_float_control_write(control | u64(mask), vector | (mask << 7))
			}
			return i32(old)
		} $else {
			old := u32(control >> 8) & bare_fenv_mask()
			if change == 1 {
				bare_float_control_write(control | (u64(mask) << 8), vector)
				actual, _ := bare_float_control_read()
				if u32(actual >> 8) & mask != mask { return -1 }
			} else if change == -1 {
				bare_float_control_write(control & ~(u64(mask) << 8), vector)
			}
			return i32(old)
		}
	}

	fn bare_fenv_operation(first u32, second u32, multiply bool) {
		a := &first
		b := &second
		$if amd64 {
			if multiply {
				asm volatile amd64 {
					movss xmm0, [a]
					movss xmm1, [b]
					mulss xmm0, xmm1
					; ; r (a)
					    r (b)
					; xmm0
					  xmm1
					  memory
				}
			} else {
				asm volatile amd64 {
					movss xmm0, [a]
					movss xmm1, [b]
					divss xmm0, xmm1
					; ; r (a)
					    r (b)
					; xmm0
					  xmm1
					  memory
				}
			}
		} $else {
			if multiply {
				asm volatile arm64 {
					ldr s0, [a]
					ldr s1, [b]
					fmul s0, s0, s1
					; ; r (a)
					    r (b)
					; v0
					  v1
					  memory
				}
			} else {
				asm volatile arm64 {
					ldr s0, [a]
					ldr s1, [b]
					fdiv s0, s0, s1
					; ; r (a)
					    r (b)
					; v0
					  v1
					  memory
				}
			}
		}
	}

	@[export: 'v_fenv_raise'; markused]
	fn bare_fenv_raise(requested i32) i32 {
		mask := u32(requested) & bare_fenv_mask()
		$if amd64 {
			if mask & 1 != 0 { bare_fenv_operation(0, 0, false) }
			if mask & 4 != 0 { bare_fenv_operation(0x3f800000, 0, false) }
			if mask & 8 != 0 { bare_fenv_operation(0x7f7fffff, 0x7f7fffff, true) }
			if mask & 16 != 0 { bare_fenv_operation(0x00800000, 0x00800000, true) }
			if mask & 32 != 0 { bare_fenv_operation(0x3f800000, 0x40400000, false) }
		} $else {
			if mask & 1 != 0 { bare_fenv_operation(0, 0, false) }
			if mask & 2 != 0 { bare_fenv_operation(0x3f800000, 0, false) }
			if mask & 4 != 0 { bare_fenv_operation(0x7f7fffff, 0x7f7fffff, true) }
			if mask & 8 != 0 { bare_fenv_operation(0x00800000, 0x00800000, true) }
			if mask & 16 != 0 { bare_fenv_operation(0x3f800000, 0x40400000, false) }
		}
		return 0
	}

	@[export: 'v_fenv_update'; markused]
	fn bare_fenv_update(input uint) i32 {
		pending := bare_fenv_flags(-1)
		bare_fenv_set(input)
		return bare_fenv_raise(i32(pending))
	}
}
