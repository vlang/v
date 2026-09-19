// vtest build: amd64 && !msvc && !tinyc

#flag @VMODROOT/vlib/v/slow_tests/assembly/util/v_sha256_block.o

@[c_extern]
fn C.v_sha256_block(&u32, &u8)

fn test_external_sha256_block() {
	$if amd64 {
		mut state := [
			u32(0x6a09e667),
			u32(0xbb67ae85),
			u32(0x3c6ef372),
			u32(0xa54ff53a),
			u32(0x510e527f),
			u32(0x9b05688c),
			u32(0x1f83d9ab),
			u32(0x5be0cd19),
		]
		mut block := [64]u8{}
		block[0] = `a`
		block[1] = `b`
		block[2] = `c`
		block[3] = 0x80
		block[63] = 24

		unsafe {
			C.v_sha256_block(&state[0], &block[0])
		}

		expected := [
			u32(0xba7816bf),
			u32(0x8f01cfea),
			u32(0x414140de),
			u32(0x5dae2223),
			u32(0xb00361a3),
			u32(0x96177a9c),
			u32(0xb410ff61),
			u32(0xf20015ad),
		]
		assert state == expected
	}
}

// On Win64, `rdi` is callee-saved (it is an argument register only on System V), so
// the fixture must return it untouched even though it uses it internally.
fn win64_rdi_after_sha256_block(state &u32, block &u8) u64 {
	mut rdi_after := u64(0)
	// The raw call only links on Win64 (Mach-O prefixes C symbols with `_`), and the
	// argument registers below are the Win64 ones.
	$if windows && amd64 {
		mut state_reg := voidptr(state)
		mut block_reg := voidptr(block)
		asm amd64 raw {
			"movq %%rsp, %%rbx\n\t"
			"subq $32, %%rsp\n\t"
			"andq $-16, %%rsp\n\t"
			"movabsq $0x5a5a5a5a5a5a5a5a, %%rdi\n\t"
			"call v_sha256_block\n\t"
			"movq %%rbx, %%rsp\n\t"
			"movq %%rdi, %[out]"
			; [out] "=r" (rdi_after)
			  [state_reg] "+c" (state_reg)
			  [block_reg] "+d" (block_reg)
			;
			; rax
			  rbx
			  rdi
			  rsi
			  r8
			  r9
			  r10
			  r11
			  memory
			  cc
		}
	}
	return rdi_after
}

fn test_external_sha256_block_preserves_rdi_on_win64() {
	$if windows && amd64 {
		mut state := [8]u32{}
		mut block := [64]u8{}
		unsafe {
			assert win64_rdi_after_sha256_block(&state[0], &block[0]) == u64(0x5a5a5a5a5a5a5a5a)
		}
	}
}
