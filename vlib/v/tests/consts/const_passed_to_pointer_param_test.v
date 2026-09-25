struct ConstArgTarget {
	x int
}

const const_arg_ptr = &ConstArgTarget{7}
const const_arg_value = ConstArgTarget{8}
const const_arg_int = 9
const const_arg_cstr = c'abc'

fn const_arg_fn(t &ConstArgTarget) voidptr {
	return voidptr(t)
}

fn const_arg_fn_x(t &ConstArgTarget) int {
	return t.x
}

fn const_arg_fn_int(n &int) int {
	return *n
}

fn const_arg_generic[T](t &T) voidptr {
	return voidptr(t)
}

fn const_arg_generic_value[T](t &T) T {
	return *t
}

struct ConstArgUser {}

fn (u ConstArgUser) ptr(t &ConstArgTarget) voidptr {
	return voidptr(t)
}

fn (mut u ConstArgUser) mut_ptr(t &ConstArgTarget) voidptr {
	return voidptr(t)
}

fn (u ConstArgUser) x(t &ConstArgTarget) int {
	return t.x
}

fn (mut u ConstArgUser) mut_x(t &ConstArgTarget) int {
	return t.x
}

fn (u ConstArgUser) int_value(n &int) int {
	return *n
}

fn (mut u ConstArgUser) mut_int_value(n &int) int {
	return *n
}

fn (u ConstArgUser) cstr(p &u8) voidptr {
	return voidptr(p)
}

struct ConstArgGenericUser[T] {
	value T
}

fn (u ConstArgGenericUser[T]) ptr(t &ConstArgTarget) voidptr {
	return voidptr(t)
}

fn (mut u ConstArgGenericUser[T]) mut_ptr(t &ConstArgTarget) voidptr {
	return voidptr(t)
}

fn (mut u ConstArgGenericUser[T]) mut_x(t &ConstArgTarget) int {
	return t.x
}

fn (mut u ConstArgGenericUser[T]) mut_int_value(n &int) int {
	return *n
}

// A pointer const is passed as the pointer itself, not as the address of a
// copy of its target.
fn test_pointer_const_passed_to_pointer_param_keeps_its_address() {
	mut u := ConstArgUser{}
	mut g := ConstArgGenericUser[int]{}
	assert const_arg_fn(const_arg_ptr) == voidptr(const_arg_ptr)
	assert const_arg_generic(const_arg_ptr) == voidptr(const_arg_ptr)
	assert u.ptr(const_arg_ptr) == voidptr(const_arg_ptr)
	assert u.mut_ptr(const_arg_ptr) == voidptr(const_arg_ptr)
	assert ConstArgUser{}.ptr(const_arg_ptr) == voidptr(const_arg_ptr)
	assert g.ptr(const_arg_ptr) == voidptr(const_arg_ptr)
	assert g.mut_ptr(const_arg_ptr) == voidptr(const_arg_ptr)
	assert u.cstr(const_arg_cstr) == voidptr(const_arg_cstr)
}

fn test_value_consts_passed_to_pointer_params() {
	mut u := ConstArgUser{}
	mut g := ConstArgGenericUser[string]{}
	assert const_arg_fn_x(const_arg_value) == 8
	assert const_arg_fn_int(const_arg_int) == 9
	assert const_arg_generic_value(const_arg_value).x == 8
	assert const_arg_generic_value(const_arg_int) == 9
	assert u.x(const_arg_value) == 8
	assert u.mut_x(const_arg_value) == 8
	assert u.int_value(const_arg_int) == 9
	assert u.mut_int_value(const_arg_int) == 9
	assert g.mut_x(const_arg_value) == 8
	assert g.mut_int_value(const_arg_int) == 9
}
