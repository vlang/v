// Methods, aliases and generic instantiations with 128-bit values. The generic
// case is here because stringifying a 128-bit value through a generic call used
// to emit a call to a str method that the marking step never kept.
type Wide = u128
type Signed = i128

struct Counter {
mut:
	total u128
}

fn (c Counter) doubled() u128 {
	return c.total * u128(2)
}

fn (mut c Counter) add(n u128) {
	c.total += n
}

fn (w Wide) high_half() u128 {
	return u128(w) >> 64
}

fn (s Signed) magnitude() u128 {
	return if s < 0 { u128(-i128(s)) } else { u128(s) }
}

fn identity[T](v T) T {
	return v
}

fn total_of(n int, seed u128) u128 {
	mut acc := seed
	for i in 0 .. n {
		acc += u128(i)
	}
	return acc
}

fn test_struct_methods_with_128_bit_fields() {
	mut c := Counter{}
	c.add(u128(1) << 100)
	c.add(u128(5))
	total := (u128(1) << 100) + u128(5)
	assert c.total == total
	assert c.doubled() == total * u128(2)
}

fn test_methods_on_128_bit_aliases() {
	w := Wide(u128(1) << 70)
	assert w.high_half() == u128(64)
	s := Signed(i128(-1) << 100)
	assert s.magnitude() == u128(1) << 100
}

fn test_128_bit_through_generics() {
	assert identity(u128(340282366920938463463374607431768211455)) == u128(340282366920938463463374607431768211455)
	assert identity(i128(-1) << 100) == i128(-1) << 100
	// Interpolating the generic call is the shape that used to fail to compile.
	x := (u128(1) << 100) + u128(5)
	assert '${identity(x)}' == '1267650600228229401496703205381'
}

fn test_128_bit_through_a_loop() {
	assert total_of(5, u128(1) << 64) == (u128(1) << 64) + u128(10)
}
