module atomics

// add_i32 atomically adds delta to the value at dest and returns the new value.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn add_i32(dest &i32, delta i32) i32 {
	mut result := i32(0)
	asm volatile i386 {
		mov edx, dest
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, delta
		lock xadd [edx], eax
		add eax, delta
		mov result, eax
		2:
		; =r (result)
		; r (dest)
		  r (delta)
		; eax
		  edx
		  memory
	}
	return result
}

// swap_i32 atomically stores new value at dest and returns the old value.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn swap_i32(dest &i32, new i32) i32 {
	mut old := i32(0)
	asm volatile i386 {
		mov edx, dest
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, new
		xchg [edx], eax
		mov old, eax
		2:
		; =m (old)
		; r (dest)
		  r (new)
		; eax
		  edx
		  memory
	}
	return old
}

// store_i32 atomically stores value at dest.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn store_i32(dest &i32, value i32) {
	asm volatile i386 {
		mov edx, dest
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, value
		xchg eax, [edx]
		2:
		; ; r (dest)
		  r (value)
		; eax
		  edx
		  memory
	}
}

// load_i32 atomically loads and returns the value at num.
// The operation is performed with sequential consistency.
// Panics if num is not 4-byte aligned.
pub fn load_i32(num &i32) i32 {
	mut out := i32(0)
	asm volatile i386 {
		mov edx, num
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, [edx]
		mov out, eax
		2:
		; =r (out)
		; r (num)
		; eax
		  edx
		  memory
	}
	return out
}

// cas_i32 performs a compare-and-swap operation.
// If the current value at addr equals old, it atomically stores new.
// Returns true if the swap was performed, false otherwise.
// The operation is performed with sequential consistency.
// Panics if addr is not 4-byte aligned.
pub fn cas_i32(addr &i32, old i32, new i32) bool {
	mut swapped := false
	asm volatile i386 {
		mov edx, addr
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, old
		mov ecx, new
		lock cmpxchg [edx], ecx
		sete al
		mov swapped, al
		2:
		; =m (swapped)
		; r (addr)
		  r (old)
		  r (new)
		; eax
		  ecx
		  edx
		  memory
	}
	return swapped
}

// store_i64 atomically stores value at dest using MMX instructions.
// The operation is performed with sequential consistency.
// Requires MMX support. Panics if dest is not 8-byte aligned.
pub fn store_i64(dest &i64, value i64) {
	asm volatile i386 {
		mov esi, dest
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		movq mm0, value
		movq [esi], mm0
		emms
		xor eax, eax
		lock xaddl [esp], eax
		2:
		; ; r (dest)
		  m (value)
		; esi
		  eax
		  mm0
		  memory
	}
}

// load_i64 atomically loads and returns the value at num using MMX instructions.
// The operation is performed with sequential consistency.
// Requires MMX support. Panics if num is not 8-byte aligned.
pub fn load_i64(num &i64) i64 {
	mut out := i64(0)
	asm volatile i386 {
		mov esi, num
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		movq mm0, [esi]
		movq out, mm0
		emms
		2:
		; =m (out)
		; r (num)
		; esi
		  mm0
		  memory
	}
	return out
}

// add_i64 atomically adds delta to the value at dest and returns the new value.
// Uses a compare-and-swap loop. The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn add_i64(dest &i64, delta i64) i64 {
	mut delta_lo := u32(u64(delta) & 0xFFFF_FFFF)
	mut delta_hi := u32(u64(delta) >> 32)
	mut res_lo := u32(0)
	mut res_hi := u32(0)
	asm volatile i386 {
		mov esi, dest
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [esi]
		mov edx, [esi + 4]
		mov ebx, eax
		mov ecx, edx
		add ebx, delta_lo
		adc ecx, delta_hi
		lock cmpxchg8b [esi]
		jnz '3b'
		mov res_lo, ebx
		mov res_hi, ecx
		2:
		; ; r (dest)
		  m (delta_lo)
		  m (delta_hi)
		  m (res_lo)
		  m (res_hi)
		; eax
		  ebx
		  ecx
		  edx
		  esi
		  memory
	}
	return i64(u64(res_lo) | (u64(res_hi) << 32))
}

// swap_i64 atomically stores value at dest and returns the old value.
// Uses a compare-and-swap loop. The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn swap_i64(dest &i64, value i64) i64 {
	mut value_lo := u32(u64(value) & 0xFFFF_FFFF)
	mut value_hi := u32(u64(value) >> 32)
	asm volatile i386 {
		mov esi, dest
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [esi]
		mov edx, [esi + 4]
		mov ebx, value_lo
		mov ecx, value_hi
		lock cmpxchg8b [esi]
		jnz '3b'
		mov value_lo, eax
		mov value_hi, edx
		2:
		; ; r (dest)
		  m (value_lo)
		  m (value_hi)
		; eax
		  ebx
		  ecx
		  edx
		  esi
		  memory
	}
	return i64(u64(value_lo) | (u64(value_hi) << 32))
}

// cas_i64 performs a compare-and-swap operation.
// If the current value at addr equals old, it atomically stores new.
// Returns true if the swap was performed, false otherwise.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn cas_i64(addr &i64, old i64, new i64) bool {
	mut swapped := false
	mut old_lo := u32(u64(old) & 0xFFFF_FFFF)
	mut old_hi := u32(u64(old) >> 32)
	mut new_lo := u32(u64(new) & 0xFFFF_FFFF)
	mut new_hi := u32(u64(new) >> 32)
	asm volatile i386 {
		mov esi, addr
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, old_lo
		mov edx, old_hi
		mov ebx, new_lo
		mov ecx, new_hi
		lock cmpxchg8b [esi]
		sete al
		mov swapped, al
		2:
		; =m (swapped)
		; r (addr)
		  m (old_lo)
		  m (old_hi)
		  m (new_lo)
		  m (new_hi)
		; eax
		  ebx
		  ecx
		  edx
		  esi
		  memory
	}
	return swapped
}

// add_u32 atomically adds delta to the value at dest and returns the new value.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn add_u32(dest &u32, delta u32) u32 {
	mut result := u32(0)
	asm volatile i386 {
		mov edx, dest
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, delta
		lock xadd [edx], eax
		add eax, delta
		mov result, eax
		2:
		; =r (result)
		; r (dest)
		  r (delta)
		; eax
		  edx
		  memory
	}
	return result
}

// swap_u32 atomically stores new value at dest and returns the old value.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn swap_u32(dest &u32, new u32) u32 {
	mut old := u32(0)
	asm volatile i386 {
		mov edx, dest
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, new
		xchg [edx], eax
		mov old, eax
		2:
		; =m (old)
		; r (dest)
		  r (new)
		; eax
		  edx
		  memory
	}
	return old
}

// store_u32 atomically stores value at dest.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn store_u32(dest &u32, value u32) {
	asm volatile i386 {
		mov edx, dest
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, value
		xchg eax, [edx]
		2:
		; ; r (dest)
		  r (value)
		; eax
		  edx
		  memory
	}
}

// load_u32 atomically loads and returns the value at num.
// The operation is performed with sequential consistency.
// Panics if num is not 4-byte aligned.
pub fn load_u32(num &u32) u32 {
	mut out := u32(0)
	asm volatile i386 {
		mov edx, num
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, [edx]
		mov out, eax
		2:
		; =r (out)
		; r (num)
		; eax
		  edx
		  memory
	}
	return out
}

// cas_u32 performs a compare-and-swap operation.
// If the current value at addr equals old, it atomically stores new.
// Returns true if the swap was performed, false otherwise.
// The operation is performed with sequential consistency.
// Panics if addr is not 4-byte aligned.
pub fn cas_u32(addr &u32, old u32, new u32) bool {
	mut swapped := false
	asm volatile i386 {
		mov edx, addr
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, old
		mov ecx, new
		lock cmpxchg [edx], ecx
		sete al
		mov swapped, al
		2:
		; =m (swapped)
		; r (addr)
		  r (old)
		  r (new)
		; eax
		  ecx
		  edx
		  memory
	}
	return swapped
}

// load_u64 atomically loads and returns the value at num using MMX instructions.
// The operation is performed with sequential consistency.
// Requires MMX support. Panics if num is not 8-byte aligned.
pub fn load_u64(num &u64) u64 {
	mut out := u64(0)
	asm volatile i386 {
		mov esi, num
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		movq mm0, [esi]
		movq out, mm0
		emms
		2:
		; =m (out)
		; r (num)
		; esi
		  mm0
		  memory
	}
	return out
}

// store_u64 atomically stores value at dest using MMX instructions.
// The operation is performed with sequential consistency.
// Requires MMX support. Panics if dest is not 8-byte aligned.
pub fn store_u64(dest &u64, value u64) {
	asm volatile i386 {
		mov esi, dest
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		movq mm0, value
		movq [esi], mm0
		emms
		xor eax, eax
		lock xaddl [esp], eax
		2:
		; ; r (dest)
		  m (value)
		; eax
		  mm0
		  memory
	}
}

// add_u64 atomically adds delta to the value at dest and returns the new value.
// Uses a compare-and-swap loop. The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn add_u64(dest &u64, delta u64) u64 {
	mut delta_lo := u32(delta & 0xFFFF_FFFF)
	mut delta_hi := u32(delta >> 32)
	mut res_lo := u32(0)
	mut res_hi := u32(0)
	asm volatile i386 {
		mov esi, dest
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [esi]
		mov edx, [esi + 4]
		mov ebx, eax
		mov ecx, edx
		add ebx, delta_lo
		adc ecx, delta_hi
		lock cmpxchg8b [esi]
		jnz '3b'
		mov res_lo, ebx
		mov res_hi, ecx
		2:
		; ; r (dest)
		  m (delta_lo)
		  m (delta_hi)
		  m (res_lo)
		  m (res_hi)
		; eax
		  ebx
		  ecx
		  edx
		  esi
		  memory
	}
	return u64(res_lo) | (u64(res_hi) << 32)
}

// swap_u64 atomically stores value at dest and returns the old value.
// Uses a compare-and-swap loop. The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn swap_u64(dest &u64, value u64) u64 {
	mut old := u64(0)
	mut value_lo := u32(value & 0xFFFF_FFFF)
	mut value_hi := u32(value >> 32)
	asm volatile i386 {
		mov esi, dest
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [esi]
		mov edx, [esi + 4]
		mov ebx, value_lo
		mov ecx, value_hi
		lock cmpxchg8b [esi]
		jnz '3b'
		mov value_lo, eax
		mov value_hi, edx
		2:
		; ; r (dest)
		  m (value_lo)
		  m (value_hi)
		; eax
		  ebx
		  ecx
		  edx
		  esi
		  memory
	}
	old = u64(value_lo) | (u64(value_hi) << 32)
	return old
}

// cas_u64 performs a compare-and-swap operation.
// If the current value at addr equals old, it atomically stores new.
// Returns true if the swap was performed, false otherwise.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn cas_u64(addr &u64, old u64, new u64) bool {
	mut swapped := false
	mut old_lo := u32(old & 0xFFFF_FFFF)
	mut old_hi := u32(old >> 32)
	mut new_lo := u32(new & 0xFFFF_FFFF)
	mut new_hi := u32(new >> 32)
	asm volatile i386 {
		mov esi, addr
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, old_lo
		mov edx, old_hi
		mov ebx, new_lo
		mov ecx, new_hi
		lock cmpxchg8b [esi]
		sete al
		mov swapped, al
		2:
		; =m (swapped)
		; r (addr)
		  m (old_lo)
		  m (old_hi)
		  m (new_lo)
		  m (new_hi)
		; eax
		  ebx
		  ecx
		  edx
		  esi
		  memory
	}
	return swapped
}

// and_i64 atomically performs a bitwise AND of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn and_i64(addr &i64, mask i64) i64 {
	mask_lo := u32(u64(mask) & 0xFFFF_FFFF)
	mask_hi := u32(u64(mask) >> 32)
	mut old_lo := u32(0)
	mut old_hi := u32(0)

	asm volatile i386 {
		mov esi, addr
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, [esi]
		mov edx, [esi + 4]
		3:
		mov ebx, eax
		and ebx, mask_lo
		mov ecx, edx
		and ecx, mask_hi
		lock cmpxchg8b [esi]
		jnz '3b'
		mov old_lo, eax
		mov old_hi, edx
		2:
		; =m (old_lo)
		  =m (old_hi)
		; m (mask_lo)
		  m (mask_hi)
		  r (addr)
		; eax
		  edx
		  ecx
		  ebx
		  esi
	}
	return i64(u64(old_lo) | (u64(old_hi) << 32))
}

// and_u64 atomically performs a bitwise AND of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn and_u64(addr &u64, mask u64) u64 {
	mask_lo := u32(u64(mask) & 0xFFFF_FFFF)
	mask_hi := u32(u64(mask) >> 32)
	mut old_lo := u32(0)
	mut old_hi := u32(0)

	asm volatile i386 {
		mov esi, addr
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, [esi]
		mov edx, [esi + 4]
		3:
		mov ebx, eax
		and ebx, mask_lo
		mov ecx, edx
		and ecx, mask_hi
		lock cmpxchg8b [esi]
		jnz '3b'
		mov old_lo, eax
		mov old_hi, edx
		2:
		; =m (old_lo)
		  =m (old_hi)
		; m (mask_lo)
		  m (mask_hi)
		  r (addr)
		; eax
		  edx
		  ecx
		  ebx
		  esi
	}
	return u64(old_lo) | (u64(old_hi) << 32)
}

// or_i64 atomically performs a bitwise OR of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn or_i64(addr &i64, mask i64) i64 {
	mask_lo := u32(u64(mask) & 0xFFFF_FFFF)
	mask_hi := u32(u64(mask) >> 32)
	mut old_lo := u32(0)
	mut old_hi := u32(0)

	asm volatile i386 {
		mov esi, addr
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, [esi]
		mov edx, [esi + 4]
		3:
		mov ebx, eax
		or ebx, mask_lo
		mov ecx, edx
		or ecx, mask_hi
		lock cmpxchg8b [esi]
		jnz '3b'
		mov old_lo, eax
		mov old_hi, edx
		2:
		; =m (old_lo)
		  =m (old_hi)
		; m (mask_lo)
		  m (mask_hi)
		  r (addr)
		; eax
		  edx
		  ecx
		  ebx
		  esi
	}
	return i64(u64(old_lo) | (u64(old_hi) << 32))
}

// or_u64 atomically performs a bitwise OR of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn or_u64(addr &u64, mask u64) u64 {
	mask_lo := u32(u64(mask) & 0xFFFF_FFFF)
	mask_hi := u32(u64(mask) >> 32)
	mut old_lo := u32(0)
	mut old_hi := u32(0)

	asm volatile i386 {
		mov esi, addr
		test esi, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, [esi]
		mov edx, [esi + 4]
		3:
		mov ebx, eax
		or ebx, mask_lo
		mov ecx, edx
		or ecx, mask_hi
		lock cmpxchg8b [esi]
		jnz '3b'
		mov old_lo, eax
		mov old_hi, edx
		2:
		; =m (old_lo)
		  =m (old_hi)
		; m (mask_lo)
		  m (mask_hi)
		  r (addr)
		; eax
		  edx
		  ecx
		  ebx
		  esi
	}
	return u64(old_lo) | (u64(old_hi) << 32)
}

// and_u32 atomically performs a bitwise AND of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 4-byte aligned.
pub fn and_u32(addr &u32, mask u32) u32 {
	mut old := u32(0)

	asm volatile i386 {
		mov edx, addr
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [edx]
		mov ecx, eax
		and ecx, mask
		lock cmpxchgl [edx], ecx
		jnz '3b'
		mov old, eax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; edx
		  eax
		  ecx
		  memory
	}
	return old
}

// and_i32 atomically performs a bitwise AND of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 4-byte aligned.
pub fn and_i32(addr &i32, mask i32) i32 {
	mut old := i32(0)

	asm volatile i386 {
		mov edx, addr
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [edx]
		mov ecx, eax
		and ecx, mask
		lock cmpxchgl [edx], ecx
		jnz '3b'
		mov old, eax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; edx
		  eax
		  ecx
		  memory
	}
	return old
}

// or_i32 atomically performs a bitwise OR of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 4-byte aligned.
pub fn or_i32(addr &i32, mask i32) i32 {
	mut old := i32(0)

	asm volatile i386 {
		mov edx, addr
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [edx]
		mov ecx, eax
		or ecx, mask
		lock cmpxchgl [edx], ecx
		jnz '3b'
		mov old, eax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; edx
		  eax
		  ecx
		  memory
	}
	return old
}

// or_u32 atomically performs a bitwise OR of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 4-byte aligned.
pub fn or_u32(addr &u32, mask u32) u32 {
	mut old := u32(0)

	asm volatile i386 {
		mov edx, addr
		test edx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [edx]
		mov ecx, eax
		or ecx, mask
		lock cmpxchgl [edx], ecx
		jnz '3b'
		mov old, eax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; edx
		  eax
		  ecx
		  memory
	}
	return old
}
