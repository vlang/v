module cnumalias

#include "@VMODROOT/cnumalias/cnumalias.h"

pub type C.CNA_U32 = u32
pub type C.CNA_I16 = i16
pub type C.CNA_LEN = i64

pub struct Holder {
pub mut:
	value C.CNA_U32
	small C.CNA_I16
	len   C.CNA_LEN
	list  []C.CNA_U32
}

// make returns `value` as the C alias, through a call-style cast.
pub fn make(value u32) C.CNA_U32 {
	return C.CNA_U32(value)
}

// widen converts a C alias parameter back to a V number.
pub fn widen(value C.CNA_U32) u64 {
	return u64(value) + 1
}

// same compares two values of the C alias.
pub fn same(a C.CNA_U32, b C.CNA_U32) bool {
	return a == b
}

// local_ops mixes casts, arithmetic and comparisons of locally cast values.
pub fn local_ops() int {
	a := C.CNA_U32(3)
	b := C.CNA_U32(4)
	mut c := a + b
	c += C.CNA_U32(1)
	if a < b && a != b && c == C.CNA_U32(8) {
		return int(c)
	}
	return -1
}

// negative returns a signed C alias value.
pub fn negative() C.CNA_I16 {
	return C.CNA_I16(-5)
}

// row_count mirrors db.mssql: an out value of a C alias type converted to int.
pub fn row_count() int {
	mut count := C.CNA_LEN(0)
	count = C.CNA_LEN(42)
	if count == C.CNA_LEN(-1) {
		return -1
	}
	return int(count)
}
