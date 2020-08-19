// ---[ MIN ]-------------------------------------------------------------------


// SIGNED

fn test_min2_i64() {
	assert [i64(3),6,3,5].min2()   ==  3
	assert [i64(3),1,3,5].min2()   ==  1
	assert [i64(3),-1,3,-6].min2() == -6
}

fn test_min2_int() {
	assert [3,6,3,5].min2()   ==  3
	assert [3,1,3,5].min2()   ==  1
	assert [3,-1,3,-6].min2() == -6
}

fn test_min2_i16() {
	assert [i16(3),6,3,5].min2()   ==  3
	assert [i16(3),1,3,5].min2()   ==  1
	assert [i16(3),-1,3,-6].min2() == -6
}

fn test_min2_i8() {
	assert [i8(3),6,3,5].min2()   ==  3
	assert [i8(3),1,3,5].min2()   ==  1
	assert [i8(3),-1,3,-6].min2() == -6
}

// UNSIGNED

fn test_min2_u64() {
	assert [u64(3),6,3,5].min2()   ==  3
	assert [u64(3),1,3,5].min2()   ==  1
}

fn test_min2_u32() {
	assert [u32(3),6,3,5].min2()   ==  3
	assert [u32(3),1,3,5].min2()   ==  1
}

fn test_min2_u16() {
	assert [u16(3),6,3,5].min2()   ==  3
	assert [u16(3),1,3,5].min2()   ==  1
}

fn test_min2_byte() {
	assert [byte(3),6,3,5].min2()  ==  3
	assert [byte(3),1,3,5].min2()  ==  1
}

// FLOAT

fn test_min2_f32() {
	assert [f32(3.1), 1.1,  2.1].min2() == f32(1.1)
	assert [f32(3.2), 1.2, -2.2].min2() == f32(-2.2)
}

fn test_min2_f64() {
	assert [f64(3.1), 1.1,  2.1].min2() == f64(1.1)
	assert [f64(3.2), 1.2, -2.2].min2() == f64(-2.2)
}

