import math

const (
	max_float = math.max_f32
)

fn test_const_embed() {
	println(max_float)
	println(math.max_f32)
	assert max_float == math.max_f32
}
