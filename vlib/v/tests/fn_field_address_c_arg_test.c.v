@[translated]
module main

type Callback = fn (voidptr, int) int

struct Config {
mut:
	log Callback = unsafe { nil }
}

// C translated by c2v stores callbacks with `__atomic_store_n(&config.log, f, order)`.
@[c: '__atomic_store_n']
fn C.store_callback(&voidptr, Callback, int)

fn plus_one(_ voidptr, x int) int {
	return x + 1
}

fn plus_two(_ voidptr, x int) int {
	return x + 2
}

// `&config.log` is the address of the field, `&local` that of the local: a
// `&voidptr` (C's `void **`) parameter cannot take the function itself.
fn test_address_of_a_fn_field_passed_as_a_pointer_to_a_pointer() {
	mut config := Config{}
	C.store_callback(&config.log, plus_one, 0)
	assert config.log(unsafe { nil }, 41) == 42
	C.store_callback((&config.log), plus_two, 0)
	assert config.log(unsafe { nil }, 41) == 43
	// Preserve parentheses around addressed operands as regression inputs.
	// vfmt off
	C.store_callback(&(config.log), plus_one, 0)
	assert config.log(unsafe { nil }, 41) == 42
	C.store_callback(&((config.log)), plus_two, 0)
	assert config.log(unsafe { nil }, 41) == 43
	mut local := Callback(plus_one)
	C.store_callback(&local, plus_two, 0)
	assert local(unsafe { nil }, 41) == 43
	C.store_callback(&((local)), plus_one, 0)
	assert local(unsafe { nil }, 41) == 42
	mut callbacks := [Callback(plus_one), Callback(plus_one)]!
	mut index := 0
	C.store_callback(&((callbacks[index++])), plus_two, 0)
	assert index == 1
	assert callbacks[0](unsafe { nil }, 41) == 43
	assert callbacks[1](unsafe { nil }, 41) == 42
	// vfmt on
}
