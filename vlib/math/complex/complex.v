// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module complex

import math

pub struct Complex {
pub mut:
	re f64
	im f64
}

pub fn complex(re f64, im f64) Complex {
	return Complex{re, im}
}

// To String method
pub fn (c Complex) str() string {
	mut out := '${c.re:.6f}'
	out += if c.im >= 0 { '+${c.im:.6f}' } else { '${c.im:.6f}' }
	out += 'i'
	return out
}

// Complex Modulus value
// mod() and abs() return the same
pub fn (c Complex) abs() f64 {
	return math.hypot(c.re, c.im)
}

pub fn (c Complex) mod() f64 {
	return c.abs()
}

// Complex Angle
pub fn (c Complex) angle() f64 {
	return math.atan2(c.im, c.re)
}

// Complex Addition c1 + c2
pub fn (c1 Complex) + (c2 Complex) Complex {
	return Complex{c1.re + c2.re, c1.im + c2.im}
}

// Complex Substraction c1 - c2
pub fn (c1 Complex) - (c2 Complex) Complex {
	return Complex{c1.re - c2.re, c1.im - c2.im}
}

// Complex Multiplication c1 * c2
pub fn (c1 Complex) * (c2 Complex) Complex {
	return Complex{(c1.re * c2.re) + ((c1.im * c2.im) * -1), (c1.re * c2.im) + (c1.im * c2.re)}
}

// Complex Division c1 / c2
pub fn (c1 Complex) / (c2 Complex) Complex {
	denom := (c2.re * c2.re) + (c2.im * c2.im)
	return Complex{((c1.re * c2.re) + ((c1.im * -c2.im) * -1)) / denom, ((c1.re * -c2.im) +
		(c1.im * c2.re)) / denom}
}

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
	return Complex{(c1.re * c2.re) + ((c1.im * c2.im) * -1), (c1.re * c2.im) + (c1.im * c2.re)}
}

// Complex Division c1.divide(c2)
pub fn (c1 Complex) divide(c2 Complex) Complex {
	denom := (c2.re * c2.re) + (c2.im * c2.im)
	return Complex{((c1.re * c2.re) + ((c1.im * -c2.im) * -1)) / denom, ((c1.re * -c2.im) +
		(c1.im * c2.re)) / denom}
}

// Complex Conjugate
pub fn (c Complex) conjugate() Complex {
	return Complex{c.re, -c.im}
}

// Complex Additive Inverse
// Based on
// http://tutorial.math.lamar.edu/Extras/ComplexPrimer/Arithmetic.aspx
pub fn (c Complex) addinv() Complex {
	return Complex{-c.re, -c.im}
}

// Complex Multiplicative Inverse
// Based on
// http://tutorial.math.lamar.edu/Extras/ComplexPrimer/Arithmetic.aspx
pub fn (c Complex) mulinv() Complex {
	return Complex{c.re / (c.re * c.re + c.im * c.im), -c.im / (c.re * c.re + c.im * c.im)}
}

// Complex Power
// Based on
// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers/multiplying-and-dividing-complex-numbers-in-polar-form/a/complex-number-polar-form-review
pub fn (c Complex) pow(n f64) Complex {
	r := math.pow(c.abs(), n)
	angle := c.angle()
	return Complex{r * math.cos(n * angle), r * math.sin(n * angle)}
}

// Complex nth root
pub fn (c Complex) root(n f64) Complex {
	return c.pow(1.0 / n)
}

// Complex Exponential
// Using Euler's Identity
// Based on
// https://www.math.wisc.edu/~angenent/Free-Lecture-Notes/freecomplexnumbers.pdf
pub fn (c Complex) exp() Complex {
	a := math.exp(c.re)
	return Complex{a * math.cos(c.im), a * math.sin(c.im)}
}

// Complex Natural Logarithm
// Based on
// http://www.chemistrylearning.com/logarithm-of-complex-number/
pub fn (c Complex) ln() Complex {
	return Complex{math.log(c.abs()), c.angle()}
}

// Complex Log Base Complex
// Based on
// http://www.milefoot.com/math/complex/summaryops.htm
pub fn (c Complex) log(base Complex) Complex {
	return base.ln().divide(c.ln())
}

// Complex Argument
// Based on
// http://mathworld.wolfram.com/ComplexArgument.html
pub fn (c Complex) arg() f64 {
	return math.atan2(c.im, c.re)
}

// Complex raised to Complex Power
// Based on
// http://mathworld.wolfram.com/ComplexExponentiation.html
pub fn (c Complex) cpow(p Complex) Complex {
	a := c.arg()
	b := math.pow(c.re, 2) + math.pow(c.im, 2)
	d := p.re * a + (1.0 / 2) * p.im * math.log(b)
	t1 := math.pow(b, p.re / 2) * math.exp(-p.im * a)
	return Complex{t1 * math.cos(d), t1 * math.sin(d)}
}

// Complex Sin
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) sin() Complex {
	return Complex{math.sin(c.re) * math.cosh(c.im), math.cos(c.re) * math.sinh(c.im)}
}

// Complex Cosine
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) cos() Complex {
	return Complex{math.cos(c.re) * math.cosh(c.im), -(math.sin(c.re) * math.sinh(c.im))}
}

// Complex Tangent
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) tan() Complex {
	return c.sin().divide(c.cos())
}

// Complex Cotangent
// Based on
// http://www.suitcaseofdreams.net/Trigonometric_Functions.htm
pub fn (c Complex) cot() Complex {
	return c.cos().divide(c.sin())
}

// Complex Secant
// Based on
// http://www.suitcaseofdreams.net/Trigonometric_Functions.htm
pub fn (c Complex) sec() Complex {
	return complex(1, 0).divide(c.cos())
}

// Complex Cosecant
// Based on
// http://www.suitcaseofdreams.net/Trigonometric_Functions.htm
pub fn (c Complex) csc() Complex {
	return complex(1, 0).divide(c.sin())
}

// Complex Arc Sin / Sin Inverse
// Based on
// http://www.milefoot.com/math/complex/summaryops.htm
pub fn (c Complex) asin() Complex {
	return complex(0, -1).multiply(complex(0, 1).multiply(c).add(complex(1, 0).subtract(c.pow(2)).root(2)).ln())
}

// Complex Arc Consine / Consine Inverse
// Based on
// http://www.milefoot.com/math/complex/summaryops.htm
pub fn (c Complex) acos() Complex {
	return complex(0, -1).multiply(c.add(complex(0, 1).multiply(complex(1, 0).subtract(c.pow(2)).root(2))).ln())
}

// Complex Arc Tangent / Tangent Inverse
// Based on
// http://www.milefoot.com/math/complex/summaryops.htm
pub fn (c Complex) atan() Complex {
	i := complex(0, 1)
	return complex(0, 1.0 / 2).multiply(i.add(c).divide(i.subtract(c)).ln())
}

// Complex Arc Cotangent / Cotangent Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse_Functions.htm
pub fn (c Complex) acot() Complex {
	return complex(1, 0).divide(c).atan()
}

// Complex Arc Secant / Secant Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse_Functions.htm
pub fn (c Complex) asec() Complex {
	return complex(1, 0).divide(c).acos()
}

// Complex Arc Cosecant / Cosecant Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse_Functions.htm
pub fn (c Complex) acsc() Complex {
	return complex(1, 0).divide(c).asin()
}

// Complex Hyperbolic Sin
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) sinh() Complex {
	return Complex{math.cos(c.im) * math.sinh(c.re), math.sin(c.im) * math.cosh(c.re)}
}

// Complex Hyperbolic Cosine
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) cosh() Complex {
	return Complex{math.cos(c.im) * math.cosh(c.re), math.sin(c.im) * math.sinh(c.re)}
}

// Complex Hyperbolic Tangent
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) tanh() Complex {
	return c.sinh().divide(c.cosh())
}

// Complex Hyperbolic Cotangent
// Based on
// http://www.suitcaseofdreams.net/Hyperbolic_Functions.htm
pub fn (c Complex) coth() Complex {
	return c.cosh().divide(c.sinh())
}

// Complex Hyperbolic Secant
// Based on
// http://www.suitcaseofdreams.net/Hyperbolic_Functions.htm
pub fn (c Complex) sech() Complex {
	return complex(1, 0).divide(c.cosh())
}

// Complex Hyperbolic Cosecant
// Based on
// http://www.suitcaseofdreams.net/Hyperbolic_Functions.htm
pub fn (c Complex) csch() Complex {
	return complex(1, 0).divide(c.sinh())
}

// Complex Hyperbolic Arc Sin / Sin Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
pub fn (c Complex) asinh() Complex {
	return c.add(c.pow(2).add(complex(1, 0)).root(2)).ln()
}

// Complex Hyperbolic Arc Consine / Consine Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
pub fn (c Complex) acosh() Complex {
	if c.re > 1 {
		return c.add(c.pow(2).subtract(complex(1, 0)).root(2)).ln()
	} else {
		one := complex(1, 0)
		return c.add(c.add(one).root(2).multiply(c.subtract(one).root(2))).ln()
	}
}

// Complex Hyperbolic Arc Tangent / Tangent Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
pub fn (c Complex) atanh() Complex {
	one := complex(1, 0)
	if c.re < 1 {
		return complex(1.0 / 2, 0).multiply(one.add(c).divide(one.subtract(c)).ln())
	} else {
		return complex(1.0 / 2, 0).multiply(one.add(c).ln().subtract(one.subtract(c).ln()))
	}
}

// Complex Hyperbolic Arc Cotangent / Cotangent Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
pub fn (c Complex) acoth() Complex {
	one := complex(1, 0)
	if c.re < 0 || c.re > 1 {
		return complex(1.0 / 2, 0).multiply(c.add(one).divide(c.subtract(one)).ln())
	} else {
		div := one.divide(c)
		return complex(1.0 / 2, 0).multiply(one.add(div).ln().subtract(one.subtract(div).ln()))
	}
}

// Complex Hyperbolic Arc Secant / Secant Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
// For certain scenarios, Result mismatch in crossverification with Wolfram Alpha - analysis pending
// pub fn (c Complex) asech() Complex {
// 	one := complex(1,0)
// if(c.re < -1.0) {
// 	return one.subtract(
// 		one.subtract(
// 			c.pow(2)
// 		)
// 		.root(2)
// 	)
// 	.divide(c)
// 	.ln()
// }
// else {
// return one.add(
// 	one.subtract(
// 		c.pow(2)
// 	)
// 	.root(2)
// )
// .divide(c)
// .ln()
// }
// }

// Complex Hyperbolic Arc Cosecant / Cosecant Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
pub fn (c Complex) acsch() Complex {
	one := complex(1, 0)
	if c.re < 0 {
		return one.subtract(one.add(c.pow(2)).root(2)).divide(c).ln()
	} else {
		return one.add(one.add(c.pow(2)).root(2)).divide(c).ln()
	}
}

// Complex Equals
pub fn (c1 Complex) equals(c2 Complex) bool {
	return (c1.re == c2.re) && (c1.im == c2.im)
}
