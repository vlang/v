// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module math

struct Complex {
	re f64
	im f64
}

pub fn complex(re f64,im f64) Complex {
	return Complex{re,im}
}

// To String method
pub fn (c Complex) str() string { 
	mut out := '$c.re'
	out += if c.im >= 0 {
		'+$c.im'
	}
	else {
		'$c.im'
	}
	out += 'i'
	return out
}

// Complex Addition c1 + c2
pub fn (c1 Complex) + (c2 Complex) Complex {
	return Complex{c1.re+c2.re,c1.im+c2.im}
}

// Complex Substraction c1 - c2
pub fn (c1 Complex) - (c2 Complex) Complex {
	return Complex{c1.re-c2.re,c1.im-c2.im}
}

// Complex Multiplication c1 * c2
// Currently Not Supported
// pub fn (c1 Complex) * (c2 Complex) Complex {
// 	return Complex{
// 		(c1.re * c2.re) + ((c1.im * c2.im) * -1), 
// 		(c1.re * c2.im) + (c1.im * c2.re)
// 	}
// }

// Complex Division c1 / c2
// Currently Not Supported
// pub fn (c1 Complex) / (c2 Complex) Complex {
// 	denom := (c2.re * c2.re) + (c2.im * c2.im)
// 	return Complex { 
// 		((c1.re * c2.re) + ((c1.im * -c2.im) * -1))/denom, 
// 		((c1.re * -c2.im) + (c1.im * c2.re))/denom
// 	}
// }

// Complex Addition c1.add(c2)
pub fn (c1 Complex) add(c2 Complex) Complex {
	return c1 + c2
}

// Complex Subtraction c1.subtract(c2)
pub fn (c1 Complex) subtract(c2 Complex) Complex {
	return c1 - c2
}

// Complex Multiplication c1.multiply(c2)
pub fn (c1 Complex) multiply(c2 Complex) Complex {
	return Complex{
		(c1.re * c2.re) + ((c1.im * c2.im) * -1), 
		(c1.re * c2.im) + (c1.im * c2.re)
	}
}

// Complex Division c1.divide(c2)
pub fn (c1 Complex) divide(c2 Complex) Complex {
	denom := (c2.re * c2.re) + (c2.im * c2.im)
	return Complex { 
		((c1.re * c2.re) + ((c1.im * -c2.im) * -1))/denom, 
		((c1.re * -c2.im) + (c1.im * c2.re))/denom
	}
}

// Complex Conjugate
pub fn (c1 Complex) conjugate() Complex{
	return Complex{c1.re,-c1.im}
}

// Complex Equals
pub fn (c1 Complex) equals(c2 Complex) bool {
	return (c1.re == c2.re) && (c1.im == c2.im)
}