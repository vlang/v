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

// Complex Absolute value
pub fn (c Complex) abs() f64 {
	return C.hypot(c.re,c.im)
}

// Complex Angle
pub fn (c Complex) angle() f64 { 
	return atan2(c.im, c.re)
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

// Complex Additive Inverse
// Based on 
// http://tutorial.math.lamar.edu/Extras/ComplexPrimer/Arithmetic.aspx
pub fn (c1 Complex) addinv() Complex {
	return Complex{-c1.re,-c1.im}
}

// Complex Multiplicative Inverse
// Based on
// http://tutorial.math.lamar.edu/Extras/ComplexPrimer/Arithmetic.aspx
pub fn (c1 Complex) mulinv() Complex {
	return Complex {
		c1.re / (pow(c1.re,2) + pow(c1.im,2)),
		-c1.im / (pow(c1.re,2) + pow(c1.im,2))
	}
}
 
// Complex Mod or Absolute
// Based on
// http://tutorial.math.lamar.edu/Extras/ComplexPrimer/ConjugateModulus.aspx
pub fn (c1 Complex) mod() f64 {
	return sqrt(pow(c1.re,2)+pow(c1.im,2))
}

// Complex Power
// Based on
// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers/multiplying-and-dividing-complex-numbers-in-polar-form/a/complex-number-polar-form-review
pub fn (c1 Complex) pow(n f64) Complex {
	r := pow(c1.mod(),n)
	angle := atan2(c1.im,c1.re)
	return Complex {
		r * cos(n*angle),
		r * sin(n*angle)
	}
}

// Complex nth root 
pub fn (c1 Complex) root(n f64) Complex {
	return c1.pow(1.0/n)
}

// Complex Exponential
// Using Euler's Identity 
// Based on
// https://www.math.wisc.edu/~angenent/Free-Lecture-Notes/freecomplexnumbers.pdf
pub fn (c1 Complex) exp() Complex {
	a := exp(c1.re)
	return Complex {
		a * cos(c1.im),
		a * sin(c1.im)
	}
}

// Complex Natural Logarithm
// Based on 
// http://www.chemistrylearning.com/logarithm-of-complex-number/
pub fn (c1 Complex) ln() Complex {
	return Complex {
		log(c1.mod()),
		atan2(c1.im,c1.re)
	}
}

// Complex Equals
pub fn (c1 Complex) equals(c2 Complex) bool {
	return (c1.re == c2.re) && (c1.im == c2.im)
}
