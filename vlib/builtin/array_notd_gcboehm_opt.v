// dummy placeholder for functions from `array_d_gcboehm_opt.v`
// that might be needed for compile time
// `$if gcboehm_opt ? { ... } $else { ... }`

module builtin

// this is needed in `string.v`
fn __new_array_noscan(mylen int, cap int, elm_size int) array {
	return __new_array(mylen, cap, elm_size)
}

fn __new_array_with_default_noscan(mylen int, cap int, elm_size int, val voidptr) array {
	return __new_array_with_default(mylen, cap, elm_size, val)
}

fn __new_array_with_array_default_noscan(mylen int, cap int, elm_size int, val array) array {
	return __new_array_with_array_default(mylen, cap, elm_size, val)
}
