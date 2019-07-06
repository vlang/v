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