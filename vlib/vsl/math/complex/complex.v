// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module complex

import vsl.math

pub struct Complex {
pub:
	re f64
	im f64
}

pub fn complex(re f64, im f64) Complex {
	return Complex{
		re,im}
}

pub fn complex_polar(r f64, theta f64) Complex {
	return complex(r * math.cos(theta), r * math.sin(theta))
}

// To String method
pub fn (c Complex) str() string {
	mut out := '$c.re'
	out += if c.im >= 0 { '+$c.im' } else { '$c.im' }
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
pub fn (c1 Complex) +(c2 Complex) Complex {
	return Complex{
		c1.re + c2.re,c1.im + c2.im}
}

// Complex Substraction c1 - c2
pub fn (c1 Complex) -(c2 Complex) Complex {
	return Complex{
		c1.re - c2.re,c1.im - c2.im}
}

// Complex Multiplication c1 * c2
pub fn (c1 Complex) *(c2 Complex) Complex {
	return Complex{
		(c1.re * c2.re) + ((c1.im * c2.im) * -1),(c1.re * c2.im) + (c1.im * c2.re)}
}

// Complex Division c1 / c2
pub fn (c1 Complex) /(c2 Complex) Complex {
	denom := (c2.re * c2.re) + (c2.im * c2.im)
	return Complex{
		((c1.re * c2.re) + ((c1.im * -c2.im) * -1)) / denom,((c1.re * -c2.im) + (c1.im * c2.re)) / denom}
}

// Complex Addition c1.add(c2)
pub fn (c1 Complex) add(c2 Complex) Complex {
	return c1 + c2
}

// Complex Subtraction c1.sub(c2)
pub fn (c1 Complex) sub(c2 Complex) Complex {
	return c1 - c2
}

// Complex Multiplication c1.mul(c2)
pub fn (c1 Complex) mul(c2 Complex) Complex {
	return Complex{
		(c1.re * c2.re) + ((c1.im * c2.im) * -1),(c1.re * c2.im) + (c1.im * c2.re)}
}

// Complex Division c1.div(c2)
pub fn (c1 Complex) div(c2 Complex) Complex {
	denom := (c2.re * c2.re) + (c2.im * c2.im)
	return Complex{
		((c1.re * c2.re) + ((c1.im * -c2.im) * -1)) / denom,((c1.re * -c2.im) + (c1.im * c2.re)) / denom}
}

// Complex Conjugate
pub fn (c Complex) conj() Complex {
	return Complex{
		c.re,-c.im}
}

// Complex Additive Inverse
// Based on
// http://tutorial.math.lamar.edu/Extras/ComplexPrimer/Arithmetic.aspx
pub fn (c Complex) addinv() Complex {
	return Complex{
		-c.re,-c.im}
}

// Complex Multiplicative Inverse
// Based on
// http://tutorial.math.lamar.edu/Extras/ComplexPrimer/Arithmetic.aspx
pub fn (c Complex) mulinv() Complex {
	return Complex{
		c.re / (c.re * c.re + c.im * c.im),-c.im / (c.re * c.re + c.im * c.im)}
}

// Complex Power
// Based on
// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers/muling-and-dividing-complex-numbers-in-polar-form/a/complex-number-polar-form-review
pub fn (c Complex) pow(n f64) Complex {
	r := math.pow(c.abs(), n)
	angle := c.angle()
	return Complex{
		r * math.cos(n * angle),r * math.sin(n * angle)}
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
	return Complex{
		a * math.cos(c.im),a * math.sin(c.im)}
}

// Complex Natural Logarithm
// Based on
// http://www.chemistrylearning.com/logarithm-of-complex-number/
pub fn (c Complex) ln() Complex {
	return Complex{
		math.log(c.abs()),c.angle()}
}

// Complex Log Base Complex
// Based on
// http://www.milefoot.com/math/complex/summaryops.htm
pub fn (c Complex) log(base Complex) Complex {
	return base.ln().div(c.ln())
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
	return Complex{
		t1 * math.cos(d),t1 * math.sin(d)}
}

// Complex Sin
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) sin() Complex {
	return Complex{
		math.sin(c.re) * math.cosh(c.im),math.cos(c.re) * math.sinh(c.im)}
}

// Complex Cosine
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) cos() Complex {
	return Complex{
		math.cos(c.re) * math.cosh(c.im),-(math.sin(c.re) * math.sinh(c.im))}
}

// Complex Tangent
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) tan() Complex {
	return c.sin().div(c.cos())
}

// Complex Cotangent
// Based on
// http://www.suitcaseofdreams.net/Trigonometric_Functions.htm
pub fn (c Complex) cot() Complex {
	return c.cos().div(c.sin())
}

// Complex Secant
// Based on
// http://www.suitcaseofdreams.net/Trigonometric_Functions.htm
pub fn (c Complex) sec() Complex {
	return complex(1, 0).div(c.cos())
}

// Complex Cosecant
// Based on
// http://www.suitcaseofdreams.net/Trigonometric_Functions.htm
pub fn (c Complex) csc() Complex {
	return complex(1, 0).div(c.sin())
}

// Complex Arc Sin / Sin Inverse
// Based on
// http://www.milefoot.com/math/complex/summaryops.htm
pub fn (c Complex) asin() Complex {
	return complex(0, -1).mul(complex(0, 1).mul(c).add(complex(1, 0).sub(c.pow(2)).root(2)).ln())
}

// Complex Arc Consine / Consine Inverse
// Based on
// http://www.milefoot.com/math/complex/summaryops.htm
pub fn (c Complex) acos() Complex {
	return complex(0, -1).mul(c.add(complex(0, 1).mul(complex(1, 0).sub(c.pow(2)).root(2))).ln())
}

// Complex Arc Tangent / Tangent Inverse
// Based on
// http://www.milefoot.com/math/complex/summaryops.htm
pub fn (c Complex) atan() Complex {
	i := complex(0, 1)
	return complex(0, 1.0 / 2).mul(i.add(c).div(i.sub(c)).ln())
}

// Complex Arc Cotangent / Cotangent Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse_Functions.htm
pub fn (c Complex) acot() Complex {
	return complex(1, 0).div(c).atan()
}

// Complex Arc Secant / Secant Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse_Functions.htm
pub fn (c Complex) asec() Complex {
	return complex(1, 0).div(c).acos()
}

// Complex Arc Cosecant / Cosecant Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse_Functions.htm
pub fn (c Complex) acsc() Complex {
	return complex(1, 0).div(c).asin()
}

// Complex Hyperbolic Sin
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) sinh() Complex {
	return Complex{
		math.cos(c.im) * math.sinh(c.re),math.sin(c.im) * math.cosh(c.re)}
}

// Complex Hyperbolic Cosine
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) cosh() Complex {
	return Complex{
		math.cos(c.im) * math.cosh(c.re),math.sin(c.im) * math.sinh(c.re)}
}

// Complex Hyperbolic Tangent
// Based on
// http://www.milefoot.com/math/complex/functionsofi.htm
pub fn (c Complex) tanh() Complex {
	return c.sinh().div(c.cosh())
}

// Complex Hyperbolic Cotangent
// Based on
// http://www.suitcaseofdreams.net/Hyperbolic_Functions.htm
pub fn (c Complex) coth() Complex {
	return c.cosh().div(c.sinh())
}

// Complex Hyperbolic Secant
// Based on
// http://www.suitcaseofdreams.net/Hyperbolic_Functions.htm
pub fn (c Complex) sech() Complex {
	return complex(1, 0).div(c.cosh())
}

// Complex Hyperbolic Cosecant
// Based on
// http://www.suitcaseofdreams.net/Hyperbolic_Functions.htm
pub fn (c Complex) csch() Complex {
	return complex(1, 0).div(c.sinh())
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
	if (c.re > 1) {
		return c.add(c.pow(2).sub(complex(1, 0)).root(2)).ln()
	}
	else {
		one := complex(1, 0)
		return c.add(c.add(one).root(2).mul(c.sub(one).root(2))).ln()
	}
}

// Complex Hyperbolic Arc Tangent / Tangent Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
pub fn (c Complex) atanh() Complex {
	one := complex(1, 0)
	if (c.re < 1) {
		return complex(1.0 / 2, 0).mul(one.add(c).div(one.sub(c)).ln())
	}
	else {
		return complex(1.0 / 2, 0).mul(one.add(c).ln().sub(one.sub(c).ln()))
	}
}

// Complex Hyperbolic Arc Cotangent / Cotangent Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
pub fn (c Complex) acoth() Complex {
	one := complex(1, 0)
	if (c.re < 0 || c.re > 1) {
		return complex(1.0 / 2, 0).mul(c.add(one).div(c.sub(one)).ln())
	}
	else {
		div := one.div(c)
		return complex(1.0 / 2, 0).mul(one.add(div).ln().sub(one.sub(div).ln()))
	}
}

// Complex Hyperbolic Arc Secant / Secant Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
// For certain scenarios, Result mismatch in crossverification with Wolfram Alpha - analysis pending
pub fn (c Complex) asech() Complex {
	one := complex(1, 0)
	if (c.re < -1.0) {
		return one.sub(one.sub(c.pow(2)).root(2)).div(c).ln()
	}
	else {
		return one.add(one.sub(c.pow(2)).root(2)).div(c).ln()
	}
}

// Complex Hyperbolic Arc Cosecant / Cosecant Inverse
// Based on
// http://www.suitcaseofdreams.net/Inverse__Hyperbolic_Functions.htm
pub fn (c Complex) acsch() Complex {
	one := complex(1, 0)
	if (c.re < 0) {
		return one.sub(one.add(c.pow(2)).root(2)).div(c).ln()
	}
	else {
		return one.add(one.add(c.pow(2)).root(2)).div(c).ln()
	}
}

// Complex Equals
pub fn (c1 Complex) equals(c2 Complex) bool {
	return (c1.re == c2.re) && (c1.im == c2.im)
}
