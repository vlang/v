import math

// Tests are based on and verified from practice examples of Khan Academy 
// https://www.khanacademy.org/math/precalculus/imaginary-and-complex-numbers

fn test_complex_addition() {
	mut c1 := math.complex(0,-10)
	mut c2 := math.complex(-40,8)
	mut result := c1 + c2
	assert result.equals(math.complex(-40,-2))
	c1 = math.complex(-71,2)
	c2 = math.complex(88,-12)
	result = c1 + c2
	assert result.equals(math.complex(17,-10))
	c1 = math.complex(0,-30)
	c2 = math.complex(52,-30)
	result = c1 + c2
	assert result.equals(math.complex(52,-60))
	c1 = math.complex(12,-9)
	c2 = math.complex(32,-6)
	result = c1 + c2
	assert result.equals(math.complex(44,-15))
}

fn test_complex_subtraction() {
	mut c1 := math.complex(-8,0)
	mut c2 := math.complex(6,30)
	mut result := c1 - c2
	assert result.equals(math.complex(-14,-30))
	c1 = math.complex(-19,7)
	c2 = math.complex(29,32)
	result = c1 - c2
	assert result.equals(math.complex(-48,-25))
	c1 = math.complex(12,0)
	c2 = math.complex(23,13)
	result = c1 - c2
	assert result.equals(math.complex(-11,-13))
	c1 = math.complex(-14,3)
	c2 = math.complex(0,14)
	result = c1 - c2
	assert result.equals(math.complex(-14,-11))
}

fn test_complex_multiplication() {
	mut c1 := math.complex(1,2)
	mut c2 := math.complex(1,-4)
	mut result := c1.multiply(c2)
	assert result.equals(math.complex(9,-2))
	c1 = math.complex(-4,-4)
	c2 = math.complex(-5,-3)
	result = c1.multiply(c2)
	assert result.equals(math.complex(8,32))
	c1 = math.complex(4,4)
	c2 = math.complex(-2,-5)
	result = c1.multiply(c2)
	assert result.equals(math.complex(12,-28))
	c1 = math.complex(2,-2)
	c2 = math.complex(4,-4)
	result = c1.multiply(c2)
	assert result.equals(math.complex(0,-16))
}

fn test_complex_division() {
	mut c1 := math.complex(-9,-6)
	mut c2 := math.complex(-3,-2)
	mut result := c1.divide(c2)
	assert result.equals(math.complex(3,0))
	c1 = math.complex(-23,11)
	c2 = math.complex(5,1)
	result = c1.divide(c2)
	assert result.equals(math.complex(-4,3))
	c1 = math.complex(8,-2)
	c2 = math.complex(-4,1)
	result = c1.divide(c2)
	assert result.equals(math.complex(-2,0))
	c1 = math.complex(11,24)
	c2 = math.complex(-4,-1)
	result = c1.divide(c2)
	assert result.equals(math.complex(-4,-5))
}

fn test_complex_conjugate() {
	mut c1 := math.complex(0,8)
	mut result := c1.conjugate()
	assert result.equals(math.complex(0,-8))
	c1 = math.complex(7,3)
	result = c1.conjugate()
	assert result.equals(math.complex(7,-3))
	c1 = math.complex(2,2)
	result = c1.conjugate()
	assert result.equals(math.complex(2,-2))
	c1 = math.complex(7,0)
	result = c1.conjugate()
	assert result.equals(math.complex(7,0))
}

fn test_complex_equals() {
	mut c1 := math.complex(0,8)
	mut c2 := math.complex(0,8)
	assert c1.equals(c2)
	c1 = math.complex(-3,19)
	c2 = math.complex(-3,19)
	assert c1.equals(c2)
}

fn test_complex_abs() {
	mut c1 := math.complex(3,4)
	assert c1.abs() == 5
	c1 = math.complex(1,2)
	assert c1.abs() == math.sqrt(5)
	assert c1.abs() == c1.conjugate().abs()
	c1 = math.complex(7,0)
	assert c1.abs() == 7
}

fn test_complex_angle(){
	mut c := math.complex(1, 0)
	assert c.angle() * 180 / math.Pi == 0
	c = math.complex(1, 1)
	assert c.angle() * 180 / math.Pi == 45
	c = math.complex(0, 1)
	assert c.angle() * 180 / math.Pi == 90
	c = math.complex(-1, 1)
	assert c.angle() * 180 / math.Pi == 135
	c = math.complex(-1, -1)
	assert c.angle() * 180 / math.Pi == -135
	mut cc := c.conjugate()
	assert cc.angle() + c.angle() == 0
}


fn test_complex_addinv() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := math.complex(5,7)
	mut c2 := math.complex(-5,-7)
	mut result := c1.addinv()
	assert result.equals(c2)
	c1 = math.complex(-3,4)
	c2 = math.complex(3,-4)
	result = c1.addinv()
	assert result.equals(c2)
	c1 = math.complex(-1,-2)
	c2 = math.complex(1,2)
	result = c1.addinv()
	assert result.equals(c2)
}

fn test_complex_mulinv() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := math.complex(5,7)
	mut c2 := math.complex(0.067568,-0.094595)
	mut result := c1.mulinv()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-3,4)
	c2 = math.complex(-0.12,-0.16)
	result = c1.mulinv()
	assert result.str().eq(c2.str())
	c1 = math.complex(-1,-2)
	c2 = math.complex(-0.2,0.4)
	result = c1.mulinv()
	assert result.equals(c2)
}

fn test_complex_mod() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := math.complex(5,7)
	mut result := c1.mod()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq('8.602325')
	c1 = math.complex(-3,4)
	result = c1.mod()
	assert result == 5
	c1 = math.complex(-1,-2)
	result = c1.mod()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq('2.236068')
}

fn test_complex_pow() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := math.complex(5,7)
	mut c2 := math.complex(-24.0,70.0)
	mut result := c1.pow(2)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-3,4)
	c2 = math.complex(117,44)
	result = c1.pow(3)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-1,-2)
	c2 = math.complex(-7,-24)
	result = c1.pow(4)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_root() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := math.complex(5,7)
	mut c2 := math.complex(2.607904,1.342074)
	mut result := c1.root(2)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-3,4)
	c2 = math.complex(1.264953,1.150614)
	result = c1.root(3)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-1,-2)
	c2 = math.complex(1.068059,-0.595482)
	result = c1.root(4)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_exp() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := math.complex(5,7)
	mut c2 := math.complex(111.889015,97.505457)
	mut result := c1.exp()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-3,4)
	c2 = math.complex(-0.032543,-0.037679)
	result = c1.exp()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-1,-2)
	c2 = math.complex(-0.153092,-0.334512)
	result = c1.exp()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}

fn test_complex_ln() {
	// Tests were also verified on Wolfram Alpha
	mut c1 := math.complex(5,7)
	mut c2 := math.complex(2.152033,0.950547)
	mut result := c1.ln()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-3,4)
	c2 = math.complex(1.609438,2.214297)
	result = c1.ln()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
	c1 = math.complex(-1,-2)
	c2 = math.complex(0.804719,-2.034444)
	result = c1.ln()
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert result.str().eq(c2.str())
}
