module builtin

pub fn (i i8) str() string {
	mut res := ''
	#res.str = i.val.toString()

	return res
}

pub fn (i i16) str() string {
	mut res := ''
	#res.str = i.val.toString()

	return res
}

pub fn (i u16) str() string {
	mut res := ''
	#res.str = i.val.toString()

	return res
}

pub fn (i int) str() string {
	mut res := ''
	#res = new string( i )

	return res
}

pub fn (i i64) str() string {
	mut res := ''
	#res = new string( i )

	return res
}

pub fn (i u32) str() string {
	mut res := ''
	#res = new string( i )

	return res
}

pub fn (i u64) str() string {
	mut res := ''
	#res = new string( i )

	return res
}

pub fn (i bool) str() string {
	mut res := ''
	#res = new string( i )

	return res
}

pub fn (i any) str() string {
	mut res := ''
	#res = new string( i.toString() )

	return res
}

pub fn (i int_literal) str() string {
	res := ''
	#res.str = i.val.toString()

	return res
}

pub fn (x u64) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x i64) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x u32) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x u16) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x i8) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x i16) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x int) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x int_literal) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

pub fn (x byte) hex() string {
	res := ''
	#res.str = x.val.toString(16)

	return res
}

// hex returns a string with the hexadecimal representation
// of the byte elements of the array.
pub fn (b []byte) hex() string {
	mut hex := ''
	for i in b {
		n0 := i >> 4
		println(n0)
		#hex.str += n0.val < 10 ? String.fromCharCode(n0.val) : String.fromCharCode(n0.val + 87)

		n1 := i & 0xF
		#hex.str += n1.val < 10 ? String.fromCharCode(n1.val) : String.fromCharCode(n1.val + 87)
	}
	return hex
}
