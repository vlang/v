import math
import flag

struct S1 {
	p voidptr
}
struct S2 {
	i int
}

fn test_math_sizeof() {
	r := math.f32_from_bits(sizeof(int))
	assert r > 5.6e-45 && r < 5.7e-45
}

fn test_sizeof() {
	assert sizeof(rune) == 4
	assert sizeof([44]byte) == 44
	assert sizeof(`€`) == 4
	// depends on -m32/64
	assert sizeof(S1) in [u32(4), 8]
	s := S2{}
	assert sizeof(s.i) == 4
	assert sizeof(flag.Flag) > 4
}
