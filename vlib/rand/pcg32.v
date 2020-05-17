module rand
// Ported from http://www.pcg-random.org/download.html
// and https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c
pub struct Pcg32 {
mut:
	state u64
	inc   u64
}

/**
 * new_pcg32 - a Pcg32 PRNG generator
 * @param initstate - the initial state of the PRNG.
 * @param initseq - the stream/step of the PRNG.
 * @return a new Pcg32 PRNG instance
*/


pub fn new_pcg32(initstate u64, initseq u64) Pcg32 {
	mut rng := Pcg32{
	}
	rng.state = u64(0)
	rng.inc = (initseq<<u64(1)) | u64(1)
	rng.next()
	rng.state += initstate
	rng.next()
	return rng
}

/**
 * Pcg32.next - update the PRNG state and get back the next random number
 * @return the generated pseudo random number
*/


[inline]
pub fn (mut rng Pcg32) next() u32 {
	oldstate := rng.state
	rng.state = oldstate * (6364136223846793005) + rng.inc
	xorshifted := u32(((oldstate>>u64(18)) ^ oldstate)>>u64(27))
	rot := u32(oldstate>>u64(59))
	return ((xorshifted>>rot) | (xorshifted<<((-rot) & u32(31))))
}

/**
 * Pcg32.bounded_next - update the PRNG state. Get the next number <  bound
 * @param bound - the returned random number will be < bound
 * @return the generated pseudo random number
*/


[inline]
pub fn (mut rng Pcg32) bounded_next(bound u32) u32 {
	// To avoid bias, we need to make the range of the RNG a multiple of
	// bound, which we do by dropping output less than a threshold.
	threshold := (-bound % bound)
	// Uniformity guarantees that loop below will terminate. In practice, it
	// should usually terminate quickly; on average (assuming all bounds are
	// equally likely), 82.25% of the time, we can expect it to require just
	// one iteration. In practice, bounds are typically small and only a
	// tiny amount of the range is eliminated.
	for {
		r := rng.next()
		if r >= threshold {
			return (r % bound)
		}
	}
	return u32(0)
}
