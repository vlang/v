module big

fn parse(s string) Integer {
	return integer_from_string(s) or { panic('cannot parse ${s}') }
}

// r_of returns the montgomery radix used for the modulus `m`.
fn r_of(m Integer) Integer {
	return one_int.left_shift(u32(m.digits.len * digit_bits))
}

// moduli spanning both cases that matter: a top digit with its high bit set,
// where twice the modulus no longer fits in the accumulator, and one without.
const test_moduli = ['1162278682028009299004281299775925417', '1000000000000000000000000000000000001',
	'170141183460469231731687303715884105727', '618970019642690137449562111', '18446744073709551629',
	'13072341731781637478543110202182709643196526891726746125798087730494475533760525940644433783483689624697551241382508919700393467667516668276328127648845601']!

fn test_mod_inv_digit() {
	for d in [u64(1), 3, 5, 7, 9, 12345, 987654321, max_digit] {
		assert (d * mod_inv_digit(d)) & max_digit == 1, 'inverse of ${d}'
	}
}

fn test_n0inv_makes_low_digit_vanish() {
	base := one_int.left_shift(u32(digit_bits))
	for ms in test_moduli {
		m := parse(ms)
		ctx := m.montgomery()
		// n * n0inv == -1 (mod base) is what cancels the low digit during
		// reduction.
		assert (m * integer_from_u64(ctx.n0inv)) % base == base - one_int, ms
	}
}

fn test_rr_is_r_squared() {
	for ms in test_moduli {
		m := parse(ms)
		r := r_of(m)
		assert m.montgomery().rr == (r * r) % m, ms
	}
}

fn test_to_mont_and_back() {
	for ms in test_moduli {
		m := parse(ms)
		ctx := m.montgomery()
		r := r_of(m)
		for xs in ['0', '1', '2', '123456789012345678901234567890'] {
			x := parse(xs) % m
			xm := x.to_mont(ctx)
			assert xm == (x * r) % m, '${ms} ${xs}'
			assert xm.from_mont(ctx) == x, '${ms} ${xs}'
		}
	}
}

fn test_mont_mul_is_multiplication() {
	for ms in test_moduli {
		m := parse(ms)
		ctx := m.montgomery()
		r := r_of(m)
		x := parse('123456789012345678901234567890') % m
		y := parse('987654321098765432109876543210') % m
		got := x.to_mont(ctx).mont_mul(y.to_mont(ctx), ctx)
		assert got == ((x * y) % m * r) % m, ms
	}
}

fn test_mont_mul_edge_operands() {
	for ms in test_moduli {
		m := parse(ms)
		ctx := m.montgomery()
		zero_m := zero_int.to_mont(ctx)
		one_m := one_int.to_mont(ctx)
		max_m := (m - one_int).to_mont(ctx)
		assert zero_m.mont_mul(max_m, ctx) == zero_m, ms
		assert one_m.mont_mul(max_m, ctx) == max_m, ms
		// (m - 1)^2 == 1 (mod m)
		assert max_m.mont_mul(max_m, ctx).from_mont(ctx) == one_int, ms
	}
}

fn test_big_mod_pow_against_known_values() {
	// A modulus whose top digit has its high bit set exercises the carry above
	// the accumulator, which a shorter modulus never reaches.
	m := parse('1162278682028009299004281299775925417')
	b := parse('945596254727652808979496128754971402')
	e := parse('123456789012345678901234567890')
	assert b.big_mod_pow(e, m)! == parse('1077005272560686094056654269825388903')

	m2 := parse('13072341731781637478543110202182709643196526891726746125798087730494475533760525940644433783483689624697551241382508919700393467667516668276328127648845601')
	assert two_int.big_mod_pow(integer_from_int(1000), m2)! == parse('5848170423500259450307411250576052705164922792243947095124701618904319708904943405613438880505126197401303359111611077481205722733257374715695004680230675')
}

fn test_big_mod_pow_matches_mod_pow() {
	for ms in test_moduli {
		m := parse(ms)
		b := parse('123456789012345678901234567890') % m
		for e in [u64(2), 3, 17, 65537, 4294967311] {
			assert b.big_mod_pow(integer_from_u64(e), m)! == b.mod_pow(e, m), '${ms} ${e}'
		}
	}
}

fn test_fermat_little_theorem() {
	// a^(p-1) == 1 (mod p) for prime p, which fails loudly on any reduction bug.
	primes := ['170141183460469231731687303715884105727', '618970019642690137449562111',
		'2305843009213693951']
	for ps in primes {
		p := parse(ps)
		e := p - one_int
		for a in [u64(2), 3, 5, 12345] {
			assert integer_from_u64(a).big_mod_pow(e, p)! == one_int, '${ps} ${a}'
		}
	}
}
