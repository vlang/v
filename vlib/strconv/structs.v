module strconv

// The structure is filled by parser, then given to converter.
pub struct PrepNumber {
pub mut:
	negative bool // 0 if positive number, 1 if negative
	exponent int  // power of 10 exponent
	mantissa u64  // integer mantissa
}

// dec32 is a floating decimal type representing m * 10^e.
struct Dec32 {
mut:
	m u32
	e int
}

// dec64 is a floating decimal type representing m * 10^e.
struct Dec64 {
mut:
	m u64
	e int
}

struct Uint128 {
mut:
	lo u64
	hi u64
}

// support union for convert f32 to u32
union Uf32 {
mut:
	f f32
	u u32
}

// support union for convert f64 to u64
union Uf64 {
mut:
	f f64
	u u64
}

pub union Float64u {
pub mut:
	f f64
	u u64
}

pub union Float32u {
pub mut:
	f f32
	u u32
}
