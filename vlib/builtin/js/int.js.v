module builtin

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

