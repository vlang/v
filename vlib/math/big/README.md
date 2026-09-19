# math.big

The `math.big` module provides arbitrary-precision signed integers and arithmetic.

## Primality testing

`Integer.is_probably_prime(rounds)` combines trial division, Miller-Rabin, and a Lucas
probable-prime test. For values below `3317044064679887385961981`, its fixed Miller-Rabin
bases make the result deterministic. Above that bound, it performs every requested random
round in addition to the fixed bases, so a composite has probability below `1 / 4^rounds`
of being reported as probably prime. Passing zero or a negative round count selects the
default of 40 rounds.

Use `Integer.is_probably_prime_checked(rounds)` when an operating-system entropy failure
must be distinguished from a composite result. The boolean form fails closed and returns
`false` if entropy is unavailable.

The result remains probabilistic for large values. Protocols that validate untrusted
parameters may require additional checks.

```v
import math.big

fn main() {
	n := big.integer_from_string('170141183460469231731687303715884105727')!
	assert n.is_probably_prime(40)
}
```

`big.jacobi(a, n)` returns the Jacobi symbol `-1`, `0`, or `1`. The modulus `n` must be
positive and odd; invalid moduli cause a panic.
