// vtest vflags: -nocache

fn C.malloc(uint) voidptr
fn C.free(voidptr)

fn test_builtin_malloc_preserves_byte_pointer_arithmetic() {
	bytes := unsafe { malloc(8) }
	defer { unsafe { free(bytes) } }
	assert typeof(bytes).name == '&u8'
	unsafe {
		vmemset(bytes, 13, 8)
		next := bytes + 3
		assert *next == 13
		*next = 42
		assert bytes[3] == 42
		assert next - bytes == 3
		mut current := bytes + 7
		current -= 4
		assert *current == 42
	}
}

fn test_qualified_c_malloc_keeps_its_void_pointer_result() {
	raw := unsafe { C.malloc(8) }
	defer { unsafe { C.free(raw) } }
	assert typeof(raw).name == 'voidptr'
	assert raw != unsafe { nil }
}
