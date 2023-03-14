// `pub` functions will be exported.
//
// ```
// v -b wasm -no-builtin functions.v
// wasmer functions.wasm -i main.powi <a> <b>
// wasmer functions.wasm -i main.gcd <a> <b>
// ```

pub fn gcd(a_ i64, b_ i64) i64 {
	mut a := a_
	mut b := b_
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a %= b
		if a == 0 {
			return b
		}
		b %= a
	}
	return a
}

pub fn powi(a i64, b i64) i64 {
	mut b_ := b
	mut p := a
	mut v := i64(1)

	if b_ < 0 { // exponent < 0
		if a == 0 {
			return -1 // division by 0
		}
		return if a * a != 1 {
			0
		} else {
			if (b_ & 1) > 0 {
				a
			} else {
				1
			}
		}
	}

	for b_ > 0 {
		if b_ & 1 > 0 {
			v *= p
		}
		p *= p
		b_ >>= 1
	}

	return v
}
