// SSE Instruction Set
// SSE: Added with Pentium III
// Floating-point Instructions:
// ADDPS, ADDSS, CMPPS, CMPSS, COMISS, CVTPI2PS, CVTPS2PI, CVTSI2SS, CVTSS2SI, CVTTPS2PI, CVTTSS2SI,
// DIVPS, DIVSS, LDMXCSR, MAXPS, MAXSS, MINPS, MINSS, MOVAPS, MOVHLPS, MOVHPS, MOVLHPS, MOVLPS,
// MOVMSKPS, MOVNTPS, MOVSS, MOVUPS, MULPS, MULSS, RCPPS, RCPSS, RSQRTPS, RSQRTSS, SHUFPS, SQRTPS,
// SQRTSS, STMXCSR, SUBPS, SUBSS, UCOMISS, UNPCKHPS, UNPCKLPS
//
// Integer Instructions:
// ANDNPS, ANDPS, ORPS, PAVGB, PAVGW, PEXTRW, PINSRW, PMAXSW, PMAXUB, PMINSW, PMINUB, PMOVMSKB, PMULHUW, PSADBW, PSHUFW, XORPS
// The ADDPS instruction adds two vectors of floats using SSE instructions.

@[if amd64 && !tinyc && !msvc]
fn add_vectors_sse(a &f32, b &f32, result &f32) {
	unsafe {
		asm volatile amd64 {
			movups xmm0, [a] // Load 4 floats from array a into SSE register xmm0
			movups xmm1, [b] // Load 4 floats from array b into SSE register xmm1
			addps xmm0, xmm1 // Add the two vectors using SSE instruction
			movups [result], xmm0 // Store the result back to memory
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
	b := [f32(4.0), 3.0, 2.0, 1.0]
	result := []f32{len: 4}
	add_vectors_sse(&a[0], &b[0], &result[0])
	println(result)
	assert result == [f32(5.0), 5.0, 5.0, 5.0]
}
