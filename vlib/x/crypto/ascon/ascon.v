// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
module ascon

// max_nr_perm is the maximum number of permutations round supported on this module.
// The previous Ascon submission defined three Ascon permutations with 6, 8, and 12 rounds.
// The NIST SP 800-232 standard specifies additional Ascon permutations by providing round
// constants for up to 16 rounds to accommodate potential functionality extensions in the future.
const max_nr_perm = 16

// The constants to derive round constants of the Ascon permutations
// See Table 5. of NIST SP 800-232 docs
//
// 0 0x000000000000003c 8 0x00000000000000b4
// 1 0x000000000000002d 9 0x00000000000000a5
// 2 0x000000000000001e 10 0x0000000000000096
// 3 0x000000000000000f 11 0x0000000000000087
// 4 0x00000000000000f0 12 0x0000000000000078
// 5 0x00000000000000e1 13 0x0000000000000069
// 6 0x00000000000000d2 14 0x000000000000005a
// 7 0x00000000000000c3 15 0x000000000000004b
//
// We use u8 instead, since the first 56 bits of the constants are zero
const rnc = [u8(0x3c), 0x2d, 0x1e, 0x0f, 0xf0, 0xe1, 0xd2, 0xc3, 0xb4, 0xa5, 0x96, 0x87, 0x78,
	0x69, 0x5a, 0x4b]

// ascon_pnr is ascon permutation routine with specified numbers of round nr, where 1 ≤ nr ≤ 16
@[direct_array_access]
fn ascon_pnr(mut s State, nr int) {
	// We dont allow nr == 0
	if nr < 1 || nr > 16 {
		panic('Invalid round number')
	}
	// handle other number
	for i := max_nr_perm - nr; i < max_nr_perm; i++ {
		ascon_perm(mut s, rnc[i])
	}
}

// ascon_perm was the main permutations routine in Ascon-family crypto. Its consist of
// iterations of the round function that is defined as the composition of three steps, ie:
// 1. the constant-addition layer (see Sec. 3.2),
// 2. the substitution layer (see Sec.3.3), and,
// 3. the linear diffusion layer
@[direct_array_access]
fn ascon_perm(mut s State, c u8) {
	// 3.2 Constant-Addition Layer step
	//
	// The constant-addition layer adds a 64-bit round constant 𝑐𝑖
	// to 𝑆₂ in round 𝑖, for 𝑖 ≥ 0, ie, this is equivalent to applying
	// the constant to only the least significant eight bits of 𝑆₂
	s.e2 ^= c

	// 3.3. Substitution Layer
	// The substitution layer updates the state S with 64 parallel applications of the 5-bit
	// substitution box SBOX
	s.e0 ^= s.e4
	s.e4 ^= s.e3
	s.e2 ^= s.e1

	t0 := s.e4 ^ (~s.e0 & s.e1)
	t1 := s.e0 ^ (~s.e1 & s.e2)
	t2 := s.e1 ^ (~s.e2 & s.e3)
	t3 := s.e2 ^ (~s.e3 & s.e4)
	t4 := s.e3 ^ (~s.e4 & s.e0)

	s.e0 = t1
	s.e1 = t2
	s.e2 = t3
	s.e3 = t4
	s.e4 = t0

	s.e1 ^= s.e0
	s.e0 ^= s.e4
	s.e3 ^= s.e2
	s.e2 = ~(s.e2)

	// 3.4. Linear Diffusion Layer
	//
	// The linear diffusion layer provides diffusion within each 64-bit word S,
	// defined as :
	// 		Σ0(𝑆0) = 𝑆0 ⊕ (𝑆0 ⋙ 19) ⊕ (𝑆0 ⋙ 28)
	// 		Σ1(𝑆1) = 𝑆1 ⊕ (𝑆1 ⋙ 61) ⊕ (𝑆1 ⋙ 39)
	// 		Σ2(𝑆2) = 𝑆2 ⊕ (𝑆2 ⋙ 1) ⊕ (𝑆2 ⋙ 6)
	// 		Σ3(𝑆3) = 𝑆3 ⊕ (𝑆3 ⋙ 10) ⊕ (𝑆3 ⋙ 17)
	// 		Σ4(𝑆4) = 𝑆4 ⊕ (𝑆4 ⋙ 7) ⊕ (𝑆4 ⋙ 41)
	/*
	s.e0 ^= rotate_right_64(s.e0, 19) ^ rotate_right_64(s.e0, 28)
	s.e1 ^= rotate_right_64(s.e1, 61) ^ rotate_right_64(s.e1, 39)
	s.e2 ^= rotate_right_64(s.e2, 1) ^ rotate_right_64(s.e2, 6)
	s.e3 ^= rotate_right_64(s.e3, 10) ^ rotate_right_64(s.e3, 17)
	s.e4 ^= rotate_right_64(s.e4, 7) ^ rotate_right_64(s.e4, 41)
	*/
	s.e0 ^= ascon_rotate_right(s.e0, 19) ^ ascon_rotate_right(s.e0, 28)
	s.e1 ^= ascon_rotate_right(s.e1, 61) ^ ascon_rotate_right(s.e1, 39)
	s.e2 ^= ascon_rotate_right(s.e2, 1) ^ ascon_rotate_right(s.e2, 6)
	s.e3 ^= ascon_rotate_right(s.e3, 10) ^ ascon_rotate_right(s.e3, 17)
	s.e4 ^= ascon_rotate_right(s.e4, 7) ^ ascon_rotate_right(s.e4, 41)
}

// State is structure represents Ascon state. Its operates on the 320-bit opaque,
// which is represented as five of 64-bit words.
@[noinit]
struct State {
mut:
	e0 u64
	e1 u64
	e2 u64
	e3 u64
	e4 u64
}

// clone returns a clone of current state.
@[inline]
fn clone_state(s State) State {
	return State{
		e0: s.e0
		e1: s.e1
		e2: s.e2
		e3: s.e3
		e4: s.e4
	}
}

// reset this state with default value
@[inline]
fn reset_state(mut s State) {
	s.e0 = 0
	s.e1 = 0
	s.e2 = 0
	s.e3 = 0
	s.e4 = 0
}
