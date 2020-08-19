module builtin


// ---[ MIN ]-------------------------------------------------------------------


[unsafe]
fn arr_min<T>(a array) T {
	if a.len==0 { panic('.min called on an empty array') } // TODO
	unsafe {
		data := &T(a.data)
		mut val := data[0]
		for i in 0..a.len {
			if data[i] < val {
				val = data[i]
			}
		}
		return val
	}
}

// signed
pub fn (a []i8)   min() i8   { return arr_min<i8>(a)   }
pub fn (a []i16)  min() i16  { return arr_min<i16>(a)  }
pub fn (a []int)  min() int  { return arr_min<int>(a)  }
pub fn (a []i64)  min() i64  { return arr_min<i64>(a)  }

// unsigned
pub fn (a []byte) min() byte { return arr_min<byte>(a) }
pub fn (a []u16)  min() u16  { return arr_min<u16>(a)  }
pub fn (a []u32)  min() u32  { return arr_min<u32>(a)  }
pub fn (a []u64)  min() u64  { return arr_min<u64>(a)  }

// float
pub fn (a []f32)  min() f32  { return arr_min<f32>(a)  }
pub fn (a []f64)  min() f64  { return arr_min<f64>(a)  }


// ---[ MAX ]-------------------------------------------------------------------


[unsafe]
fn arr_max<T>(a array) T {
	if a.len==0 { panic('.max called on an empty array') } // TODO
	unsafe {
		data := &T(a.data)
		mut val := data[0]
		for i in 0..a.len {
			if data[i] > val {
				val = data[i]
			}
		}
		return val
	}
}

// signed
pub fn (a []i8)   max() i8   { return arr_max<i8>(a)   }
pub fn (a []i16)  max() i16  { return arr_max<i16>(a)  }
pub fn (a []int)  max() int  { return arr_max<int>(a)  }
pub fn (a []i64)  max() i64  { return arr_max<i64>(a)  }

// unsigned
pub fn (a []byte) max() byte { return arr_max<byte>(a) }
pub fn (a []u16)  max() u16  { return arr_max<u16>(a)  }
pub fn (a []u32)  max() u32  { return arr_max<u32>(a)  }
pub fn (a []u64)  max() u64  { return arr_max<u64>(a)  }

// float
pub fn (a []f32)  max() f32  { return arr_max<f32>(a)  }
pub fn (a []f64)  max() f64  { return arr_max<f64>(a)  }


// ---[ ARGMIN ]----------------------------------------------------------------


[unsafe]
fn arr_argmin<T>(a array) int {
	if a.len==0 { panic('.argmin called on an empty array') } // TODO
	mut idx := 0
	unsafe {
		data := &T(a.data)
		mut val := data[0]
		for i in 0..a.len {
			if data[i] < val {
				val = data[i]
				idx = i
			}
		}
		return idx
	}
}

// signed
pub fn (a []i8)   argmin() int { return arr_argmin<i8>(a)   }
pub fn (a []i16)  argmin() int { return arr_argmin<i16>(a)  }
pub fn (a []int)  argmin() int { return arr_argmin<int>(a)  }
pub fn (a []i64)  argmin() int { return arr_argmin<i64>(a)  }

// unsigned
pub fn (a []byte) argmin() int { return arr_argmin<byte>(a) }
pub fn (a []u16)  argmin() int { return arr_argmin<u16>(a)  }
pub fn (a []u32)  argmin() int { return arr_argmin<u32>(a)  }
pub fn (a []u64)  argmin() int { return arr_argmin<u64>(a)  }

// float
pub fn (a []f32)  argmin() int { return arr_argmin<f32>(a)  }
pub fn (a []f64)  argmin() int { return arr_argmin<f64>(a)  }


// ---[ ARGMAX ]----------------------------------------------------------------


[unsafe]
fn arr_argmax<T>(a array) int {
	if a.len==0 { panic('.argmax called on an empty array') } // TODO
	mut idx := 0
	unsafe {
		data := &T(a.data)
		mut val := data[0]
		for i in 0..a.len {
			if data[i] > val {
				val = data[i]
				idx = i
			}
		}
		return idx
	}
}

// signed
pub fn (a []i8)   argmax() int { return arr_argmax<i8>(a)   }
pub fn (a []i16)  argmax() int { return arr_argmax<i16>(a)  }
pub fn (a []int)  argmax() int { return arr_argmax<int>(a)  }
pub fn (a []i64)  argmax() int { return arr_argmax<i64>(a)  }

// unsigned
pub fn (a []byte) argmax() int { return arr_argmax<byte>(a) }
pub fn (a []u16)  argmax() int { return arr_argmax<u16>(a)  }
pub fn (a []u32)  argmax() int { return arr_argmax<u32>(a)  }
pub fn (a []u64)  argmax() int { return arr_argmax<u64>(a)  }

// float
pub fn (a []f32)  argmax() int { return arr_argmax<f32>(a)  }
pub fn (a []f64)  argmax() int { return arr_argmax<f64>(a)  }


// ---[ FOO - COMPILER ERROR EXAMPLE ]------------------------------------------
/*
[unsafe]
fn arr_foo<T>(a array) ?T {
	if a.len==0 { return none } // TODO
	unsafe {
		data := &T(a.data)
		mut val := data[0]
		for i in 0..a.len {
			if data[i] < val {
				val = data[i]
			}
		}
		return val
	}
}

// signed
pub fn (a []i8)   foo() ?i8   { return arr_foo<i8>(a)?  }
pub fn (a []i16)  foo() ?i16  { return arr_foo<i16>(a)? }
pub fn (a []int)  foo() ?int  { return arr_foo<int>(a)? }
pub fn (a []i64)  foo() ?i64  { return arr_foo<i64>(a)? }
*/
