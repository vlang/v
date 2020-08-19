// ---[ MIN ]-------------------------------------------------------------------


// SIGNED

fn test_min_i64() {
	assert [i64(3),6,3,5].min()   ==  3
	assert [i64(3),1,3,5].min()   ==  1
	assert [i64(3),-1,3,-6].min() == -6
}

fn test_min_int() {
	assert [3,6,3,5].min()   ==  3
	assert [3,1,3,5].min()   ==  1
	assert [3,-1,3,-6].min() == -6
}

fn test_min_i16() {
	assert [i16(3),6,3,5].min()   ==  3
	assert [i16(3),1,3,5].min()   ==  1
	assert [i16(3),-1,3,-6].min() == -6
}

fn test_min_i8() {
	assert [i8(3),6,3,5].min()   ==  3
	assert [i8(3),1,3,5].min()   ==  1
	assert [i8(3),-1,3,-6].min() == -6
}

// UNSIGNED

fn test_min_u64() {
	assert [u64(3),6,3,5].min()   ==  3
	assert [u64(3),1,3,5].min()   ==  1
}

fn test_min_u32() {
	assert [u32(3),6,3,5].min()   ==  3
	assert [u32(3),1,3,5].min()   ==  1
}

fn test_min_u16() {
	assert [u16(3),6,3,5].min()   ==  3
	assert [u16(3),1,3,5].min()   ==  1
}

fn test_min_byte() {
	assert [byte(3),6,3,5].min()  ==  3
	assert [byte(3),1,3,5].min()  ==  1
}

// FLOAT

fn test_min_f32() {
	assert [f32(3.1), 1.1,  2.1].min() == f32(1.1)
	assert [f32(3.2), 1.2, -2.2].min() == f32(-2.2)
}

fn test_min_f64() {
	assert [f64(3.1), 1.1,  2.1].min() == f64(1.1)
	assert [f64(3.2), 1.2, -2.2].min() == f64(-2.2)
}



// ---[ MAX ]-------------------------------------------------------------------


// SIGNED

fn test_max_i64() {
	assert [i64(3),6,3,5].max()   == 6
	assert [i64(3),1,3,5].max()   == 5
	assert [i64(3),-1,3,-6].max() == 3
}

fn test_max_int() {
	assert [3,6,3,5].max()   == 6
	assert [3,1,3,5].max()   == 5
	assert [3,-1,3,-6].max() == 3
}

fn test_max_i16() {
	assert [i16(3),6,3,5].max()   == 6
	assert [i16(3),1,3,5].max()   == 5
	assert [i16(3),-1,3,-6].max() == 3
}

fn test_max_i8() {
	assert [i8(3),6,3,5].max()   == 6
	assert [i8(3),1,3,5].max()   == 5
	assert [i8(3),-1,3,-6].max() == 3
}

// UNSIGNED

fn test_max_u64() {
	assert [u64(3),6,3,5].max()   == 6
	assert [u64(3),1,3,5].max()   == 5
}

fn test_max_u32() {
	assert [u32(3),6,3,5].max()   == 6
	assert [u32(3),1,3,5].max()   == 5
}

fn test_max_u16() {
	assert [u16(3),6,3,5].max()   == 6
	assert [u16(3),1,3,5].max()   == 5
}

fn test_max_byte() {
	assert [byte(3),6,3,5].max()  == 6
	assert [byte(3),1,3,5].max()  == 5
}

// FLOAT

fn test_max_f32() {
	assert [f32(1.1), 2.1,  3.1].max() == f32(3.1)
	assert [f32(1.2), 3.2, -2.2].max() == f32(3.2)
}

fn test_max_f64() {
	assert [f64(1.1), 2.1,  3.1].max() == f64(3.1)
	assert [f64(1.2), 3.2, -2.2].max() == f64(3.2)
}


// ---[ ARGMIN ]----------------------------------------------------------------


// SIGNED

fn test_argmin_i64() {
	assert [i64(3),6,3,5].argmin()   == 0
	assert [i64(3),1,3,5].argmin()   == 1
	assert [i64(3),-1,3,-6].argmin() == 3
}

fn test_argmin_int() {
	assert [3,6,3,5].argmin()   == 0
	assert [3,1,3,5].argmin()   == 1
	assert [3,-1,3,-6].argmin() == 3
}

fn test_argmin_i16() {
	assert [i16(3),6,3,5].argmin()   == 0
	assert [i16(3),1,3,5].argmin()   == 1
	assert [i16(3),-1,3,-6].argmin() == 3
}

fn test_argmin_i8() {
	assert [i8(3),6,3,5].argmin()   == 0
	assert [i8(3),1,3,5].argmin()   == 1
	assert [i8(3),-1,3,-6].argmin() == 3
}

// UNSIGNED

fn test_argmin_u64() {
	assert [u64(3),6,3,5].argmin()   == 0
	assert [u64(3),1,3,5].argmin()   == 1
}

fn test_argmin_u32() {
	assert [u32(3),6,3,5].argmin()   == 0
	assert [u32(3),1,3,5].argmin()   == 1
}

fn test_argmin_u16() {
	assert [u16(3),6,3,5].argmin()   == 0
	assert [u16(3),1,3,5].argmin()   == 1
}

fn test_argmin_byte() {
	assert [byte(3),6,3,5].argmin()  == 0
	assert [byte(3),1,3,5].argmin()  == 1
}

// FLOAT

fn test_argmin_f32() {
	assert [f32(3.1), 1.1,  2.1].argmin() == 1
	assert [f32(3.2), 1.2, -2.2].argmin() == 2
}

fn test_argmin_f64() {
	assert [f64(3.1), 1.1,  2.1].argmin() == 1
	assert [f64(3.2), 1.2, -2.2].argmin() == 2
}


// ---[ ARGMAX ]----------------------------------------------------------------


// SIGNED

fn test_argmax_i64() {
	assert [i64(3),6,3,5].argmax()   == 1
	assert [i64(3),1,3,5].argmax()   == 3
	assert [i64(3),-1,3,-6].argmax() == 0
}

fn test_argmax_int() {
	assert [3,6,3,5].argmax()   == 1
	assert [3,1,3,5].argmax()   == 3
	assert [3,-1,3,-6].argmax() == 0
}

fn test_argmax_i16() {
	assert [i16(3),6,3,5].argmax()   == 1
	assert [i16(3),1,3,5].argmax()   == 3
	assert [i16(3),-1,3,-6].argmax() == 0
}

fn test_argmax_i8() {
	assert [i8(3),6,3,5].argmax()   == 1
	assert [i8(3),1,3,5].argmax()   == 3
	assert [i8(3),-1,3,-6].argmax() == 0
}

// UNSIGNED

fn test_argmax_u64() {
	assert [u64(3),6,3,5].argmax()   == 1
	assert [u64(3),1,3,5].argmax()   == 3
}

fn test_argmax_u32() {
	assert [u32(3),6,3,5].argmax()   == 1
	assert [u32(3),1,3,5].argmax()   == 3
}

fn test_argmax_u16() {
	assert [u16(3),6,3,5].argmax()   == 1
	assert [u16(3),1,3,5].argmax()   == 3
}

fn test_argmax_byte() {
	assert [byte(3),6,3,5].argmax()  == 1
	assert [byte(3),1,3,5].argmax()  == 3
}

// FLOAT

fn test_argmax_f32() {
	assert [f32(2.1), 1.1,  3.1].argmax() == 2
	assert [f32(3.2), 1.2, -2.2].argmax() == 0
}

fn test_argmax_f64() {
	assert [f64(2.1), 1.1,  3.1].argmax() == 2
	assert [f64(3.2), 1.2, -2.2].argmax() == 0
}
