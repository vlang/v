// SSE Instruction Set
// SSE2: Added with Pentium 4
// Floating-point Instructions:
// ADDPD, ADDSD, ANDNPD, ANDPD, CMPPD, CMPSD*, COMISD, CVTDQ2PD, CVTDQ2PS, CVTPD2DQ, CVTPD2PI,
// CVTPD2PS, CVTPI2PD, CVTPS2DQ, CVTPS2PD, CVTSD2SI, CVTSD2SS, CVTSI2SD, CVTSS2SD, CVTTPD2DQ,
// CVTTPD2PI, CVTTPS2DQ, CVTTSD2SI, DIVPD, DIVSD, MAXPD, MAXSD, MINPD, MINSD, MOVAPD, MOVHPD,
// MOVLPD, MOVMSKPD, MOVSD*, MOVUPD, MULPD, MULSD, ORPD, SHUFPD, SQRTPD, SQRTSD, SUBPD, SUBSD,
// UCOMISD, UNPCKHPD, UNPCKLPD, XORPD
// * CMPSD and MOVSD have the same name as the string instruction mnemonics CMPSD (CMPS) and
// MOVSD (MOVS); however, the former refer to scalar double-precision floating-points whereas
// the latter refer to doubleword strings.
// Integer Instructions:
// MOVDQ2Q, MOVDQA, MOVDQU, MOVQ2DQ, PADDQ, PSUBQ, PMULUDQ, PSHUFHW, PSHUFLW, PSHUFD, PSLLDQ, PSRLDQ, PUNPCKHQDQ, PUNPCKLQDQ
// The MULPD instruction multiplies two vectors of doubles using SSE2 instructions.

@[if amd64 && !tinyc && !msvc]
fn multiply_vectors_sse2(a &f64, b &f64, result &f64) {
	unsafe {
		asm volatile amd64 {
			movupd xmm0, [a] // Load 2 doubles from array a into SSE2 register xmm0
			movupd xmm1, [b] // Load 2 doubles from array b into SSE2 register xmm1
			mulpd xmm0, xmm1 // Multiply the two vectors using SSE2 instruction
			movupd [result], xmm0 // Store the result back to memory
			; ; r (a)
			  r (b)
			  r (result)
			; xmm0
			  xmm1
		}
	}
}

fn main() {
	a := [f64(1.5), 2.5]
	b := [f64(3.5), 4.5]
	result := []f64{len: 2}
	multiply_vectors_sse2(&a[0], &b[0], &result[0])
	println(result)
	// 5.25 = 1.5 * 3.5
	// 11.25 = 2.5 * 4.5
	assert result == [f64(5.25), 11.25]
}
