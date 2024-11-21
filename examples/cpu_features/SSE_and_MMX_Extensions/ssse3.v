// SSE Instruction Set
// SSSE3: Added with Xeon 5100 and early Core 2
// PSIGNW, PSIGND, PSIGNB, PSHUFB, PMULHRSW, PMADDUBSW, PHSUBW, PHSUBSW, PHSUBD, PHADDW, PHADDSW,
// PHADDD, PALIGNR, PABSW, PABSD, PABSB
// The PSIGNW instruction negates or leaves elements unchanged based on another vector's signs.

@[if amd64 && !tinyc && !msvc]
fn psignw_example(a &i16, b &i16, result &i16) {
	unsafe {
		asm volatile amd64 {
			movdqa xmm0, [a] // Load 8 signed 16-bit integers from array a into xmm0
			movdqa xmm1, [b] // Load 8 signed 16-bit integers from array b into xmm1
			psignw xmm0, xmm1 // Adjust the sign of elements in xmm0 based on xmm1
			movdqa [result], xmm0 // Store the result back to memory
			; ; r (a)
			  r (b)
			  r (result)
			; xmm0
			  xmm1
		}
	}
}

fn main() {
	a0 := [i16(1), -2, 3, -4, 5, -6, 7, -8]
	b0 := [i16(1), -1, 1, -1, 1, -1, 1, -1]
	result0 := []i16{len: 8}
	psignw_example(&a0[0], &b0[0], &result0[0])
	dump(result0)
	assert result0 == [i16(1), 2, 3, 4, 5, 6, 7, 8]
}
