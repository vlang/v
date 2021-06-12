// dummy placeholder for functions from `array_d_gcboehm_opt.v`
// that might be needed for compile time
// `$if gcboehm_opt ? { ... } $else { ... }`

module builtin

// this is needed in `string.v`
fn __new_array_noscan(mylen int, cap int, elm_size int) array {
	return array{}
}
