import math
import math.complex as cmplx

fn tst_res(str1 string, str2 string) bool {
	if (math.abs(str1.f64() - str2.f64())) < 1e-5 {
		return true
	}
	return false
}

fn test_complex_addition() {
	// Test is based on and verified from practice examples of Khan Academy
	// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers
	mut c1 := cmplx.complex(0, -10)
	mut c2 := cmplx.complex(-40, 8)
	mut result := c1 + c2
	assert result.equals(cmplx.complex(-40, -2))
	c1 = cmplx.complex(-71, 2)
	c2 = cmplx.complex(88, -12)
	result = c1 + c2
	assert result.equals(cmplx.complex(17, -10))
	c1 = cmplx.complex(0, -30)
	c2 = cmplx.complex(52, -30)
	result = c1 + c2
	assert result.equals(cmplx.complex(52, -60))
	c1 = cmplx.complex(12, -9)
	c2 = cmplx.complex(32, -6)
	result = c1 + c2
	assert result.equals(cmplx.complex(44, -15))
}

fn test_complex_subtraction() {
	// Test is based on and verified from practice examples of Khan Academy
	// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers
	mut c1 := cmplx.complex(-8, 0)
	mut c2 := cmplx.complex(6, 30)
	mut result := c1 - c2
	assert result.equals(cmplx.complex(-14, -30))
	c1 = cmplx.complex(-19, 7)
	c2 = cmplx.complex(29, 32)
	result = c1 - c2
	assert result.equals(cmplx.complex(-48, -25))
	c1 = cmplx.complex(12, 0)
	c2 = cmplx.complex(23, 13)
	result = c1 - c2
	assert result.equals(cmplx.complex(-11, -13))
	c1 = cmplx.complex(-14, 3)
	c2 = cmplx.complex(0, 14)
	result = c1 - c2
	assert result.equals(cmplx.complex(-14, -11))
}

fn test_complex_multiplication() {
	// Test is based on and verified from practice examples of Khan Academy
	// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers
	mut c1 := cmplx.complex(1, 2)
	mut c2 := cmplx.complex(1, -4)
	mut result := c1 * c2
	assert result.equals(cmplx.complex(9, -2))
	c1 = cmplx.complex(-4, -4)
	c2 = cmplx.complex(-5, -3)
	result = c1 * c2
	assert result.equals(cmplx.complex(8, 32))
	c1 = cmplx.complex(4, 4)
	c2 = cmplx.complex(-2, -5)
	result = c1 * c2
	assert result.equals(cmplx.complex(12, -28))
	c1 = cmplx.complex(2, -2)
	c2 = cmplx.complex(4, -4)
	result = c1 * c2
	assert result.equals(cmplx.complex(0, -16))
}

fn test_complex_division() {
	// Test is based on and verified from practice examples of Khan Academy
	// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers
	mut c1 := cmplx.complex(-9, -6)
	mut c2 := cmplx.complex(-3, -2)
	mut result := c1 / c2
	assert result.equals(cmplx.complex(3, 0))
	c1 = cmplx.complex(-23, 11)
	c2 = cmplx.complex(5, 1)
	result = c1 / c2
	assert result.equals(cmplx.complex(-4, 3))
	c1 = cmplx.complex(8, -2)
	c2 = cmplx.complex(-4, 1)
	result = c1 / c2
	assert result.equals(cmplx.complex(-2, 0))
	c1 = cmplx.complex(11, 24)
	c2 = cmplx.complex(-4, -1)
	result = c1 / c2
	assert result.equals(cmplx.complex(-4, -5))
}

fn test_complex_conjugate() {
	// Test is based on and verified from practice examples of Khan Academy
	// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers
	mut c1 := cmplx.complex(0, 8)
	mut result := c1.conjugate()
	assert result.equals(cmplx.complex(0, -8))
	c1 = cmplx.complex(7, 3)
	result = c1.conjugate()
	assert result.equals(cmplx.complex(7, -3))
	c1 = cmplx.complex(2, 2)
	result = c1.conjugate()
	assert result.equals(cmplx.complex(2, -2))
	c1 = cmplx.complex(7, 0)
	result = c1.conjugate()
	assert result.equals(cmplx.complex(7, 0))
}

fn test_complex_equals() {
	mut c1 := cmplx.complex(0, 8)
	mut c2 := cmplx.complex(0, 8)
	assert c1.equals(c2)
	c1 = cmplx.complex(-3, 19)
	c2 = cmplx.complex(-3, 19)
	assert c1.equals(c2)
}

fn test_complex_abs() {
	mut c1 := cmplx.complex(3, 4)
	assert c1.abs() == 5
	c1 = cmplx.complex(1, 2)
	assert c1.abs() == math.sqrt(5)
	assert c1.abs() == c1.conjugate().abs()
	c1 = cmplx.complex(7, 0)
	assert c1.abs() == 7
}

fn test_complex_angle() {
	// Test is based on and verified from practice examples of Khan Academy
	// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers
	mut c := cmplx.complex(1, 0)
	assert c.angle() * 180 / math.pi == 0
	c = cmplx.complex(1, 1)
	assert c.angle() * 180 / math.pi == 45
	c = cmplx.complex(0, 1)
	assert c.angle() * 180 / math.pi == 90
	c = cmplx.complex(-1, 1)
	assert c.angle() * 180 / math.pi == 135
	c = cmplx.complex(-1, -1)
	assert c.angle() * 180 / math.pi == -135
	cc := c.conjugate()
	assert cc.angle() + c.angle() == 0
}

fn test_complex_addinv() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(-5, -7)
	mut result := c1.addinv()
	assert result.equals(c2)
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(3, -4)
	result = c1.addinv()
	assert result.equals(c2)
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(1, 2)
	result = c1.addinv()
	assert result.equals(c2)
}

fn test_complex_mulinv() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.067568, -0.094595)
	mut result := c1.mulinv()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	println(c2.str())
	println(result.str())
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.12, -0.16)
	result = c1.mulinv()
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.2, 0.4)
	result = c1.mulinv()
	assert result.equals(c2)
}

fn test_complex_mod() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut result := c1.mod()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(result.str(), '8.602325')
	c1 = cmplx.complex(-3, 4)
	result = c1.mod()
	assert result == 5
	c1 = cmplx.complex(-1, -2)
	result = c1.mod()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(result.str(), '2.236068')
}

fn test_complex_pow() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(-24.0, 70.0)
	mut result := c1.pow(2)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(117, 44)
	result = c1.pow(3)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-7, -24)
	result = c1.pow(4)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_root() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(2.607904, 1.342074)
	mut result := c1.root(2)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(1.264953, 1.150614)
	result = c1.root(3)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(1.068059, -0.595482)
	result = c1.root(4)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_exp() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(111.889015, 97.505457)
	mut result := c1.exp()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.032543, -0.037679)
	result = c1.exp()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.153092, -0.334512)
	result = c1.exp()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_ln() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(2.152033, 0.950547)
	mut result := c1.ln()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(1.609438, 2.214297)
	result = c1.ln()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(0.804719, -2.034444)
	result = c1.ln()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_arg() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(2.152033, 0.950547)
	mut result := c1.arg()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(result.str(), '0.950547')
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(1.609438, 2.214297)
	result = c1.arg()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(result.str(), '2.214297')
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(0.804719, -2.034444)
	result = c1.arg()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(result.str(), '-2.034444')
}

fn test_complex_log() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut b1 := cmplx.complex(-6, -2)
	mut c2 := cmplx.complex(0.232873, -1.413175)
	mut result := c1.log(b1)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	b1 = cmplx.complex(3, -1)
	c2 = cmplx.complex(0.152198, -0.409312)
	result = c1.log(b1)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	b1 = cmplx.complex(0, 9)
	c2 = cmplx.complex(-0.298243, 1.197981)
	result = c1.log(b1)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_cpow() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut r1 := cmplx.complex(2, 2)
	mut c2 := cmplx.complex(11.022341, -0.861785)
	mut result := c1.cpow(r1)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	r1 = cmplx.complex(-4, -2)
	c2 = cmplx.complex(0.118303, 0.063148)
	result = c1.cpow(r1)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	r1 = cmplx.complex(8, -9)
	c2 = cmplx.complex(-0.000000, 0.000007)
	result = c1.cpow(r1)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_sin() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(-525.794515, 155.536550)
	mut result := c1.sin()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-3.853738, -27.016813)
	result = c1.sin()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-3.165779, -1.959601)
	result = c1.sin()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_cos() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(155.536809, 525.793641)
	mut result := c1.cos()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-27.034946, 3.851153)
	result = c1.cos()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(2.032723, -3.051898)
	result = c1.cos()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_tan() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(-0.000001, 1.000001)
	mut result := c1.tan()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(0.000187, 0.999356)
	result = c1.tan()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.033813, -1.014794)
	result = c1.tan()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_cot() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(-0.000001, -0.999999)
	mut result := c1.cot()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(0.000188, -1.000644)
	result = c1.cot()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.032798, 0.984329)
	result = c1.cot()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_sec() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.000517, -0.001749)
	mut result := c1.sec()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.036253, -0.005164)
	result = c1.sec()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(0.151176, 0.226974)
	result = c1.sec()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_csc() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(-0.001749, -0.000517)
	mut result := c1.csc()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.005174, 0.036276)
	result = c1.csc()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.228375, 0.141363)
	result = c1.csc()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_asin() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.617064, 2.846289)
	mut result := c1.asin()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.633984, 2.305509)
	result = c1.asin()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.427079, -1.528571)
	result = c1.asin()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_acos() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.953732, -2.846289)
	mut result := c1.acos()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(2.204780, -2.305509)
	result = c1.acos()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(1.997875, 1.528571)
	result = c1.acos()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_atan() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(1.502727, 0.094441)
	mut result := c1.atan()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-1.448307, 0.158997)
	result = c1.atan()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-1.338973, -0.402359)
	result = c1.atan()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_acot() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.068069, -0.094441)
	mut result := c1.acot()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.122489, -0.158997)
	result = c1.acot()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.231824, 0.402359)
	result = c1.acot()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_asec() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(1.503480, 0.094668)
	mut result := c1.asec()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(1.689547, 0.160446)
	result = c1.asec()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(1.757114, -0.396568)
	result = c1.asec()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_acsc() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.067317, -0.094668)
	mut result := c1.acsc()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.118751, -0.160446)
	result = c1.acsc()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.186318, 0.396568)
	result = c1.acsc()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_sinh() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(55.941968, 48.754942)
	mut result := c1.sinh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(6.548120, -7.619232)
	result = c1.sinh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(0.489056, -1.403119)
	result = c1.sinh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_cosh() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(55.947047, 48.750515)
	mut result := c1.cosh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-6.580663, 7.581553)
	result = c1.cosh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.642148, 1.068607)
	result = c1.cosh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_tanh() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.999988, 0.000090)
	mut result := c1.tanh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-1.000710, 0.004908)
	result = c1.tanh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-1.166736, 0.243458)
	result = c1.tanh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_coth() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(1.000012, -0.000090)
	mut result := c1.coth()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.999267, -0.004901)
	result = c1.coth()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.821330, -0.171384)
	result = c1.coth()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_sech() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.010160, -0.008853)
	mut result := c1.sech()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.065294, -0.075225)
	result = c1.sech()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.413149, -0.687527)
	result = c1.sech()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_csch() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.010159, -0.008854)
	mut result := c1.csch()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(0.064877, 0.075490)
	result = c1.csch()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(0.221501, 0.635494)
	result = c1.csch()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_asinh() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(2.844098, 0.947341)
	mut result := c1.asinh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-2.299914, 0.917617)
	result = c1.asinh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-1.469352, -1.063440)
	result = c1.asinh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_acosh() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(2.846289, 0.953732)
	mut result := c1.acosh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(2.305509, 2.204780)
	result = c1.acosh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(1.528571, -1.997875)
	result = c1.acosh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_atanh() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.067066, 1.476056)
	mut result := c1.atanh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.117501, 1.409921)
	result = c1.atanh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.173287, -1.178097)
	result = c1.atanh()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_acoth() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.067066, -0.094740)
	mut result := c1.acoth()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.117501, -0.160875)
	result = c1.acoth()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.173287, 0.392699)
	result = c1.acoth()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

// fn test_complex_asech() {
// 	// Tests were also verified on Wolfram Alpha
// 	mut c1 := cmplx.complex(5,7)
// 	mut c2 := cmplx.complex(0.094668,-1.503480)
// 	mut result := c1.asech()
// 	// Some issue with precision comparison in f64 using == operator hence serializing to string
// 	assert result.str().eq(c2.str())
// 	c1 = cmplx.complex(-3,4)
// 	c2 = cmplx.complex(0.160446,-1.689547)
// 	result = c1.asech()
// 	// Some issue with precision comparison in f64 using == operator hence serializing to string
// 	assert result.str().eq(c2.str())
// 	c1 = cmplx.complex(-1,-2)
// 	c2 = cmplx.complex(0.396568,1.757114)
// 	result = c1.asech()
// 	// Some issue with precision comparison in f64 using == operator hence serializing to string
// 	assert result.str().eq(c2.str())
// }
fn test_complex_acsch() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := cmplx.complex(5, 7)
	mut c2 := cmplx.complex(0.067819, -0.094518)
	mut result := c1.acsch()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-3, 4)
	c2 = cmplx.complex(-0.121246, -0.159507)
	result = c1.acsch()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = cmplx.complex(-1, -2)
	c2 = cmplx.complex(-0.215612, 0.401586)
	result = c1.acsch()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_re_im() {
	c := cmplx.complex(2.1, 9.05)
	assert c.re == 2.1
	assert c.im == 9.05
}
