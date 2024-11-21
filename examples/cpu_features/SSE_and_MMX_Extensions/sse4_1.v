// SSE Instruction Set
// SSE4.1: Added with later Core 2
// MPSADBW, PHMINPOSUW, PMULLD, PMULDQ, DPPS, DPPD, BLENDPS, BLENDPD, BLENDVPS, BLENDVPD,
// PBLENDVB, PBLENDW, PMINSB, PMAXSB, PMINUW, PMAXUW, PMINUD, PMAXUD, PMINSD, PMAXSD, ROUNDPS,
// ROUNDSS, ROUNDPD, ROUNDSD, INSERTPS, PINSRB, PINSRD, PINSRQ, EXTRACTPS, PEXTRB, PEXTRW,
// PEXTRD, PEXTRQ, PMOVSXBW, PMOVZXBW, PMOVSXBD, PMOVZXBD, PMOVSXBQ, PMOVZXBQ, PMOVSXWD,
// PMOVZXWD, PMOVSXWQ, PMOVZXWQ, PMOVSXDQ, PMOVZXDQ, PTEST, PCMPEQQ, PACKUSDW, MOVNTDQA

@[if amd64 && !tinyc && !msvc]
fn round_floats_sse4_1(a &f32, result &f32) {
	unsafe {
		asm volatile amd64 {
			movups xmm0, [a] // Load 4 floats from array a into xmm0
			roundps xmm0, xmm0, 0 // Round to nearest integer
			movups [result], xmm0 // Store the result in result array
			; ; r (a)
			  r (result)
			; xmm0
		}
	}
}

fn main() {
	a := [f32(1.2), 2.5, 3.8, 4.4]
	result := []f32{len: 4}
	// Rounding mode 0 corresponds to rounding to the nearest integer
	round_floats_sse4_1(&a[0], &result[0])
	println(result)
	// The expected rounded result should be [1.0, 2.0, 4.0, 4.0]
	assert result == [f32(1.0), 2.0, 4.0, 4.0]
}
