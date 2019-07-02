import math

fn main() {
	f1 := math.fraction(2,4)
	f2 := math.fraction(5,10)
	println(f1 + f2)
	println(f2 - f1)
	println(f1.multiply(f2))
	println(f1.divide(f2))
	println(f1.gcf())
	println(f1.reduce())
	println(f2.gcf())
	println(f2.reduce())
	println(f1.reciprocal())
	println(f2.reciprocal())
	println(f1.equals(f1))
	println(f2.equals(f2))
	println(f1.equals(f2))
	println(f1.to_decimal())
	println(f2.to_decimal())
}