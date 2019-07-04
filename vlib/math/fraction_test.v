import math

// Results are verified using https://www.calculatorsoup.com/calculators/math/fractions.php

fn test_fraction_creation() {
	mut f1 := math.fraction(4,8)
	assert f1.f64() == 0.5
	assert f1.str().eq('4/8')
	f1 = math.fraction(10,5)
	assert f1.f64() == 2.0
	assert f1.str().eq('10/5')
	f1 = math.fraction(9,3)
	assert f1.f64() == 3.0
	assert f1.str().eq('9/3')
}

fn test_fraction_add() {
	mut f1 := math.fraction(4,8)
	mut f2 := math.fraction(5,10)
	mut sum := f1 + f2
	assert sum.f64() == 1.0
	assert sum.str().eq('80/80')
	f1 = math.fraction(5,5)
	f2 = math.fraction(8,8)
	sum = f1 + f2
	assert sum.f64() == 2.0
	assert sum.str().eq('80/40')
	f1 = math.fraction(9,3)
	f2 = math.fraction(1,3)
	sum = f1 + f2
	println(sum.f64())
	assert sum.str().eq('10/3')
	f1 = math.fraction(3,7)
	f2 = math.fraction(1,4)
	sum = f1 + f2
	println(sum.f64())
	assert sum.str().eq('19/28')
}

fn test_fraction_subtract() {
	mut f1 := math.fraction(4,8)
	mut f2 := math.fraction(5,10)
	mut diff := f2 - f1
	assert diff.f64() == 0
	assert diff.str().eq('0/80')
	f1 = math.fraction(5,5)
	f2 = math.fraction(8,8)
	diff = f2 - f1
	assert diff.f64() == 0
	assert diff.str().eq('0/40')
	f1 = math.fraction(9,3)
	f2 = math.fraction(1,3)
	diff = f1 - f2
	println(diff.f64())
	assert diff.str().eq('8/3')
	f1 = math.fraction(3,7)
	f2 = math.fraction(1,4)
	diff = f1 - f2
	println(diff.f64())
	assert diff.str().eq('5/28')
}

fn test_fraction_multiply() {
	mut f1 := math.fraction(4,8)
	mut f2 := math.fraction(5,10)
	mut product := f1.multiply(f2)
	assert product.f64() == 0.25
	assert product.str().eq('20/80')
	f1 = math.fraction(5,5)
	f2 = math.fraction(8,8)
	product = f1.multiply(f2)
	assert product.f64() == 1.0
	assert product.str().eq('40/40')
	f1 = math.fraction(9,3)
	f2 = math.fraction(1,3)
	product = f1.multiply(f2)
	assert product.f64() == 1.0
	assert product.str().eq('9/9')
	f1 = math.fraction(3,7)
	f2 = math.fraction(1,4)
	product = f1.multiply(f2)
	println(product.f64())
	assert product.str().eq('3/28')
}

fn test_fraction_divide() {
	mut f1 := math.fraction(4,8)
	mut f2 := math.fraction(5,10)
	mut re := f1.divide(f2)
	assert re.f64() == 1.0
	assert re.str().eq('40/40')
	f1 = math.fraction(5,5)
	f2 = math.fraction(8,8)
	re = f1.divide(f2)
	assert re.f64() == 1.0
	assert re.str().eq('40/40')
	f1 = math.fraction(9,3)
	f2 = math.fraction(1,3)
	re = f1.divide(f2)
	assert re.f64() == 9.0
	assert re.str().eq('27/3')
	f1 = math.fraction(3,7)
	f2 = math.fraction(1,4)
	re = f1.divide(f2)
	println(re.f64())
	assert re.str().eq('12/7')
}

fn test_fraction_reciprocal() {
	mut f1 := math.fraction(4,8)
	assert f1.reciprocal().str().eq('8/4')
	f1 = math.fraction(5,10)
	assert f1.reciprocal().str().eq('10/5')
	f1 = math.fraction(5,5)
	assert f1.reciprocal().str().eq('5/5')
	f1 = math.fraction(8,8)
	assert f1.reciprocal().str().eq('8/8')
	f1 = math.fraction(9,3)
	assert f1.reciprocal().str().eq('3/9')
	f1 = math.fraction(1,3)
	assert f1.reciprocal().str().eq('3/1')
	f1 = math.fraction(3,7)
	assert f1.reciprocal().str().eq('7/3')
	f1 = math.fraction(1,4)
	assert f1.reciprocal().str().eq('4/1')
}

fn test_fraction_equals() {
	mut f1 := math.fraction(4,8)
	mut f2 := math.fraction(5,10)
	assert f1.equals(f2)
	f1 = math.fraction(1,2)
	f2 = math.fraction(3,4)
	assert !f1.equals(f2)
}