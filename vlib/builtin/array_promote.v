module builtin


// ---[ MIN ]-------------------------------------------------------------------

// signed

[unsafe]
fn arr_min2_i64(a array, v fn(voidptr) i64) i64 {
	if a.len==0 { panic('.min called on an empty array') } // TODO
	//if a.len==0 { return none } // TODO
	unsafe {
		mut val := v(a.get(0))
		for i in 0..a.len {
			if v(a.get(i)) < val {
				val = v(a.get(i))
			}
		}
		return val
	}
}

fn get_i8(x &i8)   i64 { return *x }
fn get_i16(x &i16) i64 { return *x }
fn get_int(x &int) i64 { return *x }
fn get_i64(x &i64) i64 { return *x }

pub fn (a []i8)   min2() i8   { return i8(arr_min2_i64(a,get_i8))   }
pub fn (a []i16)  min2() i16  { return i16(arr_min2_i64(a,get_i16)) }
pub fn (a []int)  min2() int  { return int(arr_min2_i64(a,get_int)) }
pub fn (a []i64)  min2() i64  { return i64(arr_min2_i64(a,get_i64)) }

// unsigned

[unsafe]
fn arr_min2_u64(a array, v fn(voidptr) u64) u64 {
	if a.len==0 { panic('.min called on an empty array') } // TODO
	unsafe {
		mut val := v(a.get(0))
		for i in 0..a.len {
			if v(a.get(i)) < val {
				val = v(a.get(i))
			}
		}
		return val
	}
}

fn get_byte(x &byte) u64 { return *x }
fn get_u16(x &u16)   u64 { return *x }
fn get_u32(x &u32)   u64 { return *x }
fn get_u64(x &u64)   u64 { return *x }

pub fn (a []byte) min2() byte { return byte(arr_min2_u64(a,get_byte)) }
pub fn (a []u16)  min2() u16  { return u16(arr_min2_u64(a,get_u16))   }
pub fn (a []u32)  min2() u32  { return u32(arr_min2_u64(a,get_u32))   }
pub fn (a []u64)  min2() u64  { return u64(arr_min2_u64(a,get_u64))   }


// float

[unsafe]
fn arr_min2_f64(a array, v fn(voidptr) f64) f64 {
	if a.len==0 { panic('.min called on an empty array') } // TODO
	unsafe {
		mut val := v(a.get(0))
		for i in 0..a.len {
			if v(a.get(i)) < val {
				val = v(a.get(i))
			}
		}
		return val
	}
}

fn get_f32(x &f32) f64 { return *x }
fn get_f64(x &f64) f64 { return *x }

pub fn (a []f32)  min2() f32  { return f32(arr_min2_f64(a,get_f32)) }
pub fn (a []f64)  min2() f64  { return f64(arr_min2_f64(a,get_f64)) }
