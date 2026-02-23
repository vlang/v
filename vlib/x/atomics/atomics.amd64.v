module atomics

// add_i32 atomically adds delta to the value at dest and returns the new value.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn add_i32(dest &i32, delta i32) i32 {
	mut result := i32(0)
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, delta
		lock xadd [rdx], eax
		add eax, delta
		mov result, eax
		2:
		; =r (result)
		; r (dest)
		  r (delta)
		; eax
		  rdx
		  memory
	}
	return result
}

// swap_i32 atomically stores new value at dest and returns the old value.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn swap_i32(dest &i32, new i32) i32 {
	mut old := i32(0)
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, new
		xchg [rdx], eax
		mov old, eax
		2:
		; =r (old)
		; r (dest)
		  r (new)
		; eax
		  rdx
		  memory
	}
	return old
}

// store_i32 atomically stores value at dest.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn store_i32(dest &i32, value i32) {
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, value
		xchg eax, [rdx]
		2:
		; ; r (dest)
		  r (value)
		; eax
		  rdx
		  memory
	}
}

// load_i32 atomically loads and returns the value at num.
// The operation is performed with sequential consistency.
// Panics if num is not 4-byte aligned.
pub fn load_i32(num &i32) i32 {
	mut out := i32(0)
	asm volatile amd64 {
		mov rdx, num
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, [rdx]
		mov out, eax
		2:
		; =r (out)
		; r (num)
		; eax
		  rdx
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
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, old
		mov ecx, new
		lock cmpxchg [rdx], ecx
		sete al
		mov swapped, al
		2:
		; =r (swapped)
		; r (addr)
		  r (old)
		  r (new)
		; eax
		  ecx
		  rdx
		  memory
	}
	return swapped
}

// store_i64 atomically stores value at dest.
// The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn store_i64(dest &i64, value i64) {
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, value
		xchg rax, [rdx]
		2:
		; ; r (dest)
		  r (value)
		; rax
		  rdx
		  memory
	}
}

// load_i64 atomically loads and returns the value at num.
// The operation is performed with sequential consistency.
// Panics if num is not 8-byte aligned.
pub fn load_i64(num &i64) i64 {
	mut out := i64(0)
	asm volatile amd64 {
		mov rdx, num
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, [rdx]
		mov out, rax
		2:
		; =r (out)
		; r (num)
		; rax
		  rdx
		  memory
	}
	return out
}

// add_i64 atomically adds delta to the value at dest and returns the new value.
// The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn add_i64(dest &i64, delta i64) i64 {
	mut result := i64(0)
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, delta
		lock xadd [rdx], rax
		add rax, delta
		mov result, rax
		2:
		; =r (result)
		; r (delta)
		  r (dest)
		; rax
		  rdx
		  memory
	}
	return result
}

// swap_i64 atomically stores value at dest and returns the old value.
// The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn swap_i64(dest &i64, value i64) i64 {
	mut old := i64(0)
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, value
		xchg rax, [rdx]
		mov old, rax
		2:
		; =r (old)
		; r (dest)
		  r (value)
		; rax
		  rdx
		  memory
	}
	return old
}

// cas_i64 performs a compare-and-swap operation.
// If the current value at addr equals old, it atomically stores new.
// Returns true if the swap was performed, false otherwise.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn cas_i64(addr &i64, old i64, new i64) bool {
	mut swapped := false
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, old
		mov rcx, new
		lock cmpxchgq [rdx], rcx
		sete al
		mov swapped, al
		2:
		; =r (swapped)
		; r (addr)
		  r (old)
		  r (new)
		; rax
		  rcx
		  rdx
		  memory
	}
	return swapped
}

// add_u32 atomically adds delta to the value at dest and returns the new value.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn add_u32(dest &u32, delta u32) u32 {
	mut result := u32(0)
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, delta
		lock xadd [rdx], eax
		add eax, delta
		mov result, eax
		2:
		; =r (result)
		; r (dest)
		  r (delta)
		; rax
		  rdx
		  memory
	}
	return result
}

// swap_u32 atomically stores new value at dest and returns the old value.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn swap_u32(dest &u32, new u32) u32 {
	mut old := u32(0)
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, new
		xchg [rdx], eax
		mov old, eax
		2:
		; =r (old)
		; r (dest)
		  r (new)
		; eax
		  rdx
		  memory
	}
	return old
}

// store_u32 atomically stores value at dest.
// The operation is performed with sequential consistency.
// Panics if dest is not 4-byte aligned.
pub fn store_u32(dest &u32, value u32) {
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, value
		xchg eax, [rdx]
		2:
		; ; r (dest)
		  r (value)
		; eax
		  rdx
		  memory
	}
}

// load_u32 atomically loads and returns the value at num.
// The operation is performed with sequential consistency.
// Panics if num is not 4-byte aligned.
pub fn load_u32(num &u32) u32 {
	mut out := u32(0)
	asm volatile amd64 {
		mov rdx, num
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, [rdx]
		mov out, eax
		2:
		; =r (out)
		; r (num)
		; rax
		  rdx
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
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov eax, old
		mov ecx, new
		lock cmpxchg [rdx], ecx
		sete al
		mov swapped, al
		2:
		; =r (swapped)
		; r (addr)
		  r (old)
		  r (new)
		; eax
		  ecx
		  rdx
		  memory
	}
	return swapped
}

// load_u64 atomically loads and returns the value at num.
// The operation is performed with sequential consistency.
// Panics if num is not 8-byte aligned.
pub fn load_u64(num &u64) u64 {
	mut out := u64(0)
	asm volatile amd64 {
		mov rdx, num
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, [rdx]
		mov out, rax
		2:
		; =r (out)
		; r (num)
		; rax
		  rdx
		  memory
	}
	return out
}

// store_u64 atomically stores value at dest.
// The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn store_u64(dest &u64, value u64) {
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, value
		xchg rax, [rdx]
		2:
		; ; r (dest)
		  r (value)
		; rax
		  rdx
		  memory
	}
}

// add_u64 atomically adds delta to the value at dest and returns the new value.
// The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn add_u64(dest &u64, delta u64) u64 {
	mut result := u64(0)
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, delta
		lock xadd [rdx], rax
		add rax, delta
		mov result, rax
		2:
		; =r (result)
		; r (dest)
		  r (delta)
		; rax
		  rdx
		  memory
	}
	return result
}

// swap_u64 atomically stores value at dest and returns the old value.
// The operation is performed with sequential consistency.
// Panics if dest is not 8-byte aligned.
pub fn swap_u64(dest &u64, value u64) u64 {
	mut old := u64(0)
	asm volatile amd64 {
		mov rdx, dest
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, value
		xchg [rdx], rax
		mov old, rax
		2:
		; =r (old)
		; r (dest)
		  r (value)
		; rax
		  rdx
		  memory
	}
	return old
}

// cas_u64 performs a compare-and-swap operation.
// If the current value at addr equals old, it atomically stores new.
// Returns true if the swap was performed, false otherwise.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn cas_u64(addr &u64, old u64, new u64) bool {
	mut swapped := false
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		mov rax, old
		mov rcx, new
		lock cmpxchgq [rdx], rcx
		sete al
		mov swapped, al
		2:
		; =r (swapped)
		; r (addr)
		  r (old)
		  r (new)
		; rax
		  rcx
		  rdx
		  memory
	}
	return swapped
}

// and_i64 atomically performs a bitwise AND of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn and_i64(addr &i64, mask i64) i64 {
	mut old := i64(0)
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov rax, [rdx]
		mov rcx, rax
		and rcx, mask
		lock cmpxchg [rdx], rcx
		jnz '3b'
		mov old, rax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; rdx
		  rax
		  rcx
		  memory
	}
	return old
}

// and_u64 atomically performs a bitwise AND of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn and_u64(addr &u64, mask u64) u64 {
	mut old := u64(0)
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov rax, [rdx]
		mov rcx, rax
		and rcx, mask
		lock cmpxchg [rdx], rcx
		jnz '3b'
		mov old, rax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; rdx
		  rax
		  rcx
		  memory
	}
	return old
}

// or_i64 atomically performs a bitwise OR of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn or_i64(addr &i64, mask i64) i64 {
	mut old := i64(0)
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov rax, [rdx]
		mov rcx, rax
		or rcx, mask
		lock cmpxchg [rdx], rcx
		jnz '3b'
		mov old, rax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; rdx
		  rax
		  rcx
		  memory
	}
	return old
}

// or_u64 atomically performs a bitwise OR of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 8-byte aligned.
pub fn or_u64(addr &u64, mask u64) u64 {
	mut old := u64(0)
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 7
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov rax, [rdx]
		mov rcx, rax
		or rcx, mask
		lock cmpxchg [rdx], rcx
		jnz '3b'
		mov old, rax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; rdx
		  rax
		  rcx
		  memory
	}
	return old
}

// and_i32 atomically performs a bitwise AND of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 4-byte aligned.
pub fn and_i32(addr &i32, mask i32) i32 {
	mut old := i32(0)
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [rdx]
		mov ecx, eax
		and ecx, mask
		lock cmpxchg [rdx], ecx
		jnz '3b'
		mov old, eax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; rdx
		  eax
		  ecx
		  memory
	}
	return old
}

// and_u32 atomically performs a bitwise AND of the value at addr with mask and returns the old value.
// The operation is performed with sequential consistency.
// Panics if addr is not 4-byte aligned.
pub fn and_u32(addr &u32, mask u32) u32 {
	mut old := u32(0)
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [rdx]
		mov ecx, eax
		and ecx, mask
		lock cmpxchg [rdx], ecx
		jnz '3b'
		mov old, eax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; rdx
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
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [rdx]
		mov ecx, eax
		or ecx, mask
		lock cmpxchg [rdx], ecx
		jnz '3b'
		mov old, eax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; rdx
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
	asm volatile amd64 {
		mov rdx, addr
		test rdx, 3
		jz '1f'
		call panicUnaligned
		jmp '2f'
		1:
		3:
		mov eax, [rdx]
		mov ecx, eax
		or ecx, mask
		lock cmpxchg [rdx], ecx
		jnz '3b'
		mov old, eax
		2:
		; =r (old)
		; r (addr)
		  r (mask)
		; rdx
		  eax
		  ecx
		  memory
	}
	return old
}
