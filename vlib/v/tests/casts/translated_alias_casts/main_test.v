@[translated]
module main

fn cast_with_shadow(uintptr_t fn (int) int) int {
	return uintptr_t(41)
}

fn test_translated_lowercase_alias_casts() {
	value := uintptr_t(42)
	assert usize(value) == 42
	pointer := &value
	assert usize(uintptr_t(pointer)) == usize(pointer)
	assert Elf64_Addr(uintptr_t(42)) == Elf64_Addr(42)
	assert cast_with_shadow(fn (x int) int { return x + 1 }) == 42
}

enum Token {
	zero
	one
}

enum token {
	zero
	one
}

fn test_translated_numeric_casts() {
	value := 1
	assert Token(value) == .one
	assert token(value) == .one
	assert token(0) == .zero
	assert bool(value)
	assert !bool(0)
	assert bool(1.5)
}

fn test_local_function_shadows_translated_alias() {
	uintptr_t := fn (value int) int { return value + 2 }
	assert uintptr_t(40) == 42
}

struct debug_info {
mut:
	value int
}

fn test_translated_lowercase_pointer_casts() {
	zero := &debug_info(0)
	assert zero == unsafe { nil }
	raw := unsafe { vcalloc(sizeof(debug_info)) }
	defer { unsafe { free(raw) } }
	mut pointer := &debug_info(raw)
	pointer.value = 42
	assert pointer.value == 42
	value := uintptr_t(42)
	aliased := &uintptr_t(voidptr(&value))
	assert *aliased == 42
}
