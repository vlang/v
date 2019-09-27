module rand

// Ported from http://xoshiro.di.unimi.it/splitmix64.c

struct Splitmix64 {
mut:
	state u64
}
/**
 * new_splitmix64 - a Splitmix64 PRNG generator
 * @param seed the initial seed of the PRNG. 
 * @return a new Splitmix64 PRNG instance
*/
pub fn new_splitmix64(seed u64) Splitmix64 {
	return Splitmix64{ seed }
}
/**
 * Splitmix64.next - update the PRNG state and get back the next random number
 * @return the generated pseudo random number
*/
[inline] pub fn (rng mut Splitmix64) next() u64 {
	rng.state += u64(0x9e3779b97f4a7c15)
	mut z := rng.state
	z = (z ^ u64((z >> u64(30)))) * u64(0xbf58476d1ce4e5b9)
	z = (z ^ u64((z >> u64(27)))) * u64(0x94d049bb133111eb)
	return z ^ u64(z >> u64(31))
}
/**
 * Splitmix64.bounded_next - Get the next random number < bound
 * @param bound - the returned random number will be < bound
 * @return the generated pseudo random number
*/
[inline] pub fn (rng mut Splitmix64) bounded_next(bound u64) u64 {
	threshold := u64( -bound % bound )
	for {
		r := rng.next()
		if r >= threshold {
			return u64( r % bound )
		}			
	}
	return u64(0)
}
