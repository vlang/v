module sync

// ThreadLocalStorage provides thread-local storage for values of type T
@[noinit]
struct ThreadLocalStorage[T] {
mut:
	key    u64  // TLS key identifier. Note: While Linux uses unsigned int, Darwin requires unsigned long. u64 accommodates both.
	in_use bool // Allocation status flag
}

// DataConversion convert voidptr to/from type T
union DataConversion {
mut:
	f_u8  u8
	f_u16 u16
	f_u32 u32
	f_u64 u64

	f_i8  i8
	f_i16 i16
	f_i32 i32
	f_i64 i64

	f_voidptr voidptr
	f_isize   isize
	f_usize   usize
	f_int     int
	f_f32     f32
	f_f64     f64
	f_rune    rune
}

// convert_t_to_voidptr convert value from type T to voidptr
@[inline]
fn convert_t_to_voidptr[T](value T) !voidptr {
	mut f := DataConversion{}
	$if T is i8 {
		f.f_i8 = value
	} $else $if T is i16 {
		f.f_i16 = value
	} $else $if T is i32 {
		f.f_i32 = value
	} $else $if T is i64 {
		f.f_i64 = value
	} $else $if T is u8 {
		f.f_u8 = value
	} $else $if T is u16 {
		f.f_u16 = value
	} $else $if T is u32 {
		f.f_u32 = value
	} $else $if T is u64 {
		f.f_u64 = value
	} $else $if T is $pointer {
		f.f_voidptr = voidptr(value)
	} $else $if T is isize {
		f.f_isize = value
	} $else $if T is usize {
		f.f_usize = value
	} $else $if T is int {
		f.f_int = value
	} $else $if T is f32 {
		f.f_f32 = value
	} $else $if T is f64 {
		f.f_f64 = value
	} $else $if T is rune {
		f.f_rune = value
	} $else {
		return error('Unsupported data type `${T.name}` to voidptr')
	}
	return unsafe { f.f_voidptr }
}

// convert_voidptr_to_t convert value from voidptr to type T
@[inline]
fn convert_voidptr_to_t[T](value voidptr) !T {
	f := DataConversion{
		f_voidptr: value
	}
	$if T is i8 {
		return unsafe { f.f_i8 }
	} $else $if T is i16 {
		return unsafe { f.f_i16 }
	} $else $if T is i32 {
		return unsafe { f.f_i32 }
	} $else $if T is i64 {
		return unsafe { f.f_i64 }
	} $else $if T is u8 {
		return unsafe { f.f_u8 }
	} $else $if T is u16 {
		return unsafe { f.f_u16 }
	} $else $if T is u32 {
		return unsafe { f.f_u32 }
	} $else $if T is u64 {
		return unsafe { f.f_u64 }
	} $else $if T is $pointer {
		return unsafe { f.f_voidptr }
	} $else $if T is isize {
		return unsafe { f.f_isize }
	} $else $if T is usize {
		return unsafe { f.f_usize }
	} $else $if T is int {
		return unsafe { f.f_int }
	} $else $if T is f32 {
		return unsafe { f.f_f32 }
	} $else $if T is f64 {
		return unsafe { f.f_f64 }
	} $else $if T is rune {
		return unsafe { f.f_rune }
	}
	return error('Unsupported data type `${T.name}` from voidptr')
}
