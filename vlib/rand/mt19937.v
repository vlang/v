module rand

struct MTState {
mut:
	state [624]u32
	left int
	initf int
	next int
}

fn C.time(t &C.time_t) C.time_t

// seed() - Set the seed, needs only one int32
fn (mut rng MTState) seed(seed_data []u32) {
	if seed_data.len != 1{
		panic("mt19937 needs only one 32bit seed")
	}
	rng.state[0] = seed_data[0]
	for j := 1; j < 624; j++ {
		rng.state[j] = u32(1812433253 * (rng.state[j-1] ^ (rng.state[j-1] >> 30)) + j)
	}
	rng.left = 1
	rng.initf = 1
}

[inline]
fn mix_bits(a, b u32) u32 {
	return (a & 0x80000000) | (b & 0x7fffffff)
}

[inline]
fn twist(a, b u32) u32 {
	return (mix_bits(a, b)>>1) ^ (0x9908b0df * (b & 1))
}

fn (mut rng MTState) next_state() {
	pos := 0
	if rng.initf == 0 {
		rng.seed(u32(C.time(0)))
	}
	rng.left = 624
	rng.next = 0
	for j := 624-397; j > 0; j-- {
		rng.state[pos] = rng.state[pos+397]^twist(rng.state[pos], rng.state[pos+1])
		pos++
	}
	for j := 397-1; j > 0; j-- {
		rng.state[pos] = rng.state[397-624+pos]^twist(rng.state[pos], rng.state[pos+1])
		pos++
	}
	rng.state[pos] = rng.state[397-624+pos]^twist(rng.state[pos], rng.state[0])
}

// rng.u32() - return a pseudorandom 32bit int in [0, 2**32)
fn (mut rng MTState) u32() u32 {
	rng.left--
	if rng.left == 0 {
		rng.next_state()
	}
	y := rng.state[rng.next]
	rng.next++
	y ^= (y >> 11)
    y ^= (y << 7) & 0x9d2c5680
    y ^= (y << 15) & 0xefc60000
    y ^= (y >> 18)
	return y
}

// rng.int31() - return a 31bit positive pseudorandom integer
fn (mut rng MTState) int31() int {
	return int(rng.u32() >> 1)
}

// rng.f64() - return 64bit real in [0, 1)
fn (mut rng MTState) f64() f64 {
	return f64(rng.u32())*(1.0/4294967296.0)
}

// rng.f32() - return a 32bit real in [0, 1)
fn (mut rng MTState) f32() f32 {
	return f32(rng.u32())*f32(1.0/4294967296.0)
}