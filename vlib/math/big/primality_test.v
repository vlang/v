import math.big

fn parse(s string) big.Integer {
	return big.integer_from_string(s) or { panic('cannot parse ${s}') }
}

fn test_small_numbers() {
	primes := [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61]
	for p in primes {
		assert big.integer_from_int(p).is_probably_prime(20), '${p} should be prime'
	}
	composites := [0, 1, 4, 6, 8, 9, 10, 12, 15, 21, 25, 27, 33, 35, 39, 49, 51, 55, 57]
	for c in composites {
		assert !big.integer_from_int(c).is_probably_prime(20), '${c} should be composite'
	}
}

fn test_negative_and_zero() {
	assert !big.integer_from_int(0).is_probably_prime(20)
	assert !big.integer_from_int(-2).is_probably_prime(20)
	assert !big.integer_from_int(-7).is_probably_prime(20)
	assert !big.integer_from_int(-561).is_probably_prime(20)
}

fn test_carmichael_numbers() {
	// Absolute pseudoprimes: composite, yet pass the Fermat test for every
	// base coprime to them. A naive Fermat test reports these as prime.
	carmichael := ['561', '1105', '1729', '2465', '2821', '6601', '8911', '10585', '15841', '29341',
		'41041', '62745', '63973', '75361', '101101', '340561', '825265']
	for c in carmichael {
		assert !parse(c).is_probably_prime(20), '${c} is a Carmichael number'
	}
}

fn test_strong_pseudoprimes() {
	// Composites that survive Miller-Rabin against the first few prime bases.
	// These are the values that make a fixed-base-only implementation wrong.
	// spsp base 2
	assert !parse('2047').is_probably_prime(20)
	// spsp bases 2, 3
	assert !parse('1373653').is_probably_prime(20)
	// spsp bases 2, 3, 5
	assert !parse('25326001').is_probably_prime(20)
	// spsp bases 2, 3, 5, 7
	assert !parse('3215031751').is_probably_prime(20)
	// spsp first 5 primes
	assert !parse('2152302898747').is_probably_prime(20)
	// spsp first 6 primes
	assert !parse('3474749660383').is_probably_prime(20)
	// spsp first 7 primes
	assert !parse('341550071728321').is_probably_prime(20)
	// spsp first 9 primes
	assert !parse('3825123056546413051').is_probably_prime(20)
	// spsp first 12 primes
	assert !parse('318665857834031151167461').is_probably_prime(20)
	// spsp first 13 primes
	assert !parse('3317044064679887385961981').is_probably_prime(20)
}

fn test_mersenne_primes() {
	// 2^31 - 1
	assert parse('2147483647').is_probably_prime(20)
	// 2^61 - 1
	assert parse('2305843009213693951').is_probably_prime(20)
	// 2^89 - 1
	assert parse('618970019642690137449562111').is_probably_prime(20)
	// 2^107 - 1
	assert parse('162259276829213363391578010288127').is_probably_prime(20)
	// 2^127 - 1
	assert parse('170141183460469231731687303715884105727').is_probably_prime(20)
}

fn test_mersenne_composites() {
	// 2^11 - 1
	assert !parse('2047').is_probably_prime(20)
	// 2^23 - 1
	assert !parse('8388607').is_probably_prime(20)
	// 2^29 - 1
	assert !parse('536870911').is_probably_prime(20)
	// 2^37 - 1
	assert !parse('137438953471').is_probably_prime(20)
}

fn test_perfect_squares() {
	// A perfect square admits no D with Jacobi(D / n) == -1, so the Lucas
	// step has to terminate on its own rather than search forever.
	for n in [u64(1000003), 1000033, 32416190071] {
		x := big.integer_from_u64(n)
		assert !(x * x).is_probably_prime(20)
	}
}

fn test_large_primes() {
	// Verified against an independent Miller-Rabin implementation.
	assert parse('108088063435698740984721578578639788695211488257777317159208121754987034915181').is_probably_prime(40)
	assert parse('13072341731781637478543110202182709643196526891726746125798087730494475533760525940644433783483689624697551241382508919700393467667516668276328127648845601').is_probably_prime(40)
}

fn test_large_composite() {
	assert !parse('52289366927126549914172440808730838572786107566906984503192350921977902135042103762577735133934758498790204965530035678801573870670066673105312510595382406').is_probably_prime(40)
}

fn test_semiprime() {
	// The RSA-style case: a product of two large primes must be rejected.
	p := parse('108088063435698740984721578578639788695211488257777317159208121754987034915181')
	q := parse('2305843009213693951')
	assert !(p * q).is_probably_prime(40)
}

fn test_default_rounds() {
	// rounds <= 0 selects the default instead of skipping the test.
	assert parse('170141183460469231731687303715884105727').is_probably_prime(0)
	assert !parse('3317044064679887385961981').is_probably_prime(-1)
}

fn test_jacobi() {
	// Reference values for (a / n).
	assert big.jacobi(big.integer_from_int(1), big.integer_from_int(1)) == 1
	assert big.jacobi(big.integer_from_int(0), big.integer_from_int(1)) == 1
	assert big.jacobi(big.integer_from_int(2), big.integer_from_int(3)) == -1
	assert big.jacobi(big.integer_from_int(4), big.integer_from_int(3)) == 1
	assert big.jacobi(big.integer_from_int(3), big.integer_from_int(9)) == 0
	assert big.jacobi(big.integer_from_int(1001), big.integer_from_int(9907)) == -1
	assert big.jacobi(big.integer_from_int(19), big.integer_from_int(45)) == 1
	assert big.jacobi(big.integer_from_int(8), big.integer_from_int(21)) == -1
	// Negative arguments are reduced modulo n first.
	assert big.jacobi(big.integer_from_int(5), big.integer_from_int(21)) == 1
	assert big.jacobi(big.integer_from_int(-1), big.integer_from_int(3)) == -1
	assert big.jacobi(big.integer_from_int(-1), big.integer_from_int(5)) == 1
}
