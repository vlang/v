// SSE Instruction Set
// SSE3: Added with later Pentium 4
// ADDSUBPD, ADDSUBPS, HADDPD, HADDPS, HSUBPD, HSUBPS, MOVDDUP, MOVSHDUP, MOVSLDUP
// The HADDPS instruction performs horizontal addition of two vectors of floats using SSE3
// instructions.

@[if amd64 && !tinyc && !msvc]
fn horizontal_add_sse3(a &f32, b &f32, result &f32) {
	unsafe {
		asm volatile amd64 {
			movaps xmm0, [a] // Load 4 floats from array a into SSE3 register xmm0
			movaps xmm1, [b] // Load 4 floats from array b into SSE3 register xmm1
			haddps xmm0, xmm1 // Perform horizontal add of xmm0 and xmm1
			movaps [result], xmm0 // Store the result back to memory
			; ; r (a)
			  r (b)
			  r (result)
			; xmm0
			  xmm1
		}
	}
}

fn main() {
	a := [f32(1.0), 2.0, 3.0, 4.0]
	b := [f32(5.0), 6.0, 7.0, 8.0]
	result := []f32{len: 4}
	horizontal_add_sse3(&a[0], &b[0], &result[0])
	println(result)
	// The result should be [3.0, 7.0, 11.0, 15.0] due to horizontal addition.
	// 1.0 + 2.0 = 3.0
	// 3.0 + 4.0 = 7.0
	// 5.0 + 6.0 = 11.0
	// 7.0 + 8.0 = 15.0
	assert result == [f32(3.0), 7.0, 11.0, 15.0]
}
