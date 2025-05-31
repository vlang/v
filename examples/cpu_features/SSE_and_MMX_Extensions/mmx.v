// MMX Instruction Set
// Several suffixes are used to indicate what data size the instruction operates on:
// Byte (8 bits)
// Word (16 bits)
// Double word (32 bits)
// Quad word (64 bits)
// The signedness of the operation is also signified by the suffix: US for unsigned and S for signed.
// For example, PSUBUSB subtracts unsigned bytes, while PSUBSD subtracts signed double words.
// MMX defined over 40 new instructions, listed below.
// EMMS, MOVD, MOVQ, PACKSSDW, PACKSSWB, PACKUSWB, PADDB, PADDD, PADDSB, PADDSW, PADDUSB, PADDUSW,
// PADDW, PAND, PANDN, PCMPEQB, PCMPEQD, PCMPEQW, PCMPGTB, PCMPGTD, PCMPGTW, PMADDWD, PMULHW, PMULLW,
// POR, PSLLD, PSLLQ, PSLLW, PSRAD, PSRAW, PSRLD, PSRLQ, PSRLW, PSUBB, PSUBD, PSUBSB, PSUBSW, PSUBUSB,
// PSUBUSW, PSUBW, PUNPCKHBW, PUNPCKHDQ, PUNPCKHWD, PUNPCKLBW, PUNPCKLDQ, PUNPCKLWD, PXOR

@[if amd64 && !tinyc && !msvc]
fn add_vectors_mmx(a &u8, b &u8, result &u8) {
	unsafe {
		asm volatile amd64 {
			movq mm0, [a] // Load 8 bytes from a into MMX register mm0
			movq mm1, [b] // Load 8 bytes from b into MMX register mm1
			paddb mm0, mm1 // Add the two vectors using MMX instruction
			movq [result], mm0 // Store the result back to memory
			; ; r (a)
			  r (b)
			  r (result)
			; mm0
			  mm1
		}
	}
}

fn main() {
	a := [u8(1), 2, 3, 4, 5, 6, 7, 8]
	b := [u8(8), 7, 6, 5, 4, 3, 2, 1]
	result := []u8{len: 8}
	add_vectors_mmx(&a[0], &b[0], &result[0])
	println(result)
	assert result == [u8(9), 9, 9, 9, 9, 9, 9, 9]
}
