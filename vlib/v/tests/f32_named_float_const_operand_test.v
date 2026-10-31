// A named untyped float constant next to an f32 is converted to f32, like the literal that
// it names, as V 0.5.2 does. It used to stay a C double, so `f32(1.2) == c` was false for
// `const c = 1.2`.
import math

const named_float = 1.2
const named_sum = 1.2 + 0.1
const named_negative = -1.2
const named_paren = (1.2)
const named_alias = named_float

fn test_comparisons() {
	p := f32(1.2)
	assert p == named_float
	assert named_float == p
	assert !(p != named_float)
	assert f32(1.3) == named_sum
	assert f32(-1.2) == named_negative
	assert p == named_paren
	assert p == named_alias
	assert !(p > named_float)
	assert p <= named_float
	assert f32(math.pi) == math.pi
}

fn test_array_elements() {
	arr := [f32(1.2)]
	assert arr[0] == named_float
	assert named_float in arr
}

fn test_arithmetic() {
	p := f32(1.2)
	assert p - named_float == 0
	assert p + named_float == f32(2.4)
	assert typeof(p + named_float).name == 'f32'
}
