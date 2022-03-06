// Note: the names are deliberately prefixed with zzz_, so that searching
// for that in the generated C code is easier. Do not rename the consts.
const zzz_an_i8_const = i8(0x28)

const zzz_an_i16_const = i16(0x30)

const zzz_an_int_const = int(89)

const zzz_an_i64_const = i64(123)

//
const zzz_an_byte_const = byte(0x4b)

const zzz_an_u16_const = u16(0x53)

const zzz_an_u32_const = u32(51)

const zzz_an_u64_const = u64(95)

//
const zzz_an_f32_const = f32(1.12)

const zzz_an_f64_const = f64(3.14)

const zzz_an_i8_const_1 = zzz_an_i8_const + i8(1)

const zzz_an_i16_const_1 = zzz_an_i16_const + i16(1)

const zzz_an_int_const_1 = zzz_an_int_const + int(1)

const zzz_an_i64_const_1 = zzz_an_i64_const + i64(1)

//
const zzz_an_byte_const_1 = zzz_an_byte_const + byte(1)

const zzz_an_u16_const_1 = zzz_an_u16_const + u16(1)

const zzz_an_u32_const_1 = zzz_an_u32_const + u32(1)

const zzz_an_u64_const_1 = zzz_an_u64_const + u64(1)

//
const zzz_an_f32_const_1 = zzz_an_f32_const + f32(1.0)

const zzz_an_f64_const_1 = zzz_an_f64_const + f64(1.0)

// This test works by relying on pre_main being called before main, i.e.
// before _vinit has been run. The function pre_main will use calls to
// static_storage/2, in order to save the values of the consts into a static
// variable, and later the actual test_ function will try to read the values
// back, and compare if they are correct.
//
// It is done like that, in order to not use either const hacks or globals,
// so that this test can be run independently from their implementation).
//
// If the consts were initialised in _vinit, that check will fail. If they
// were instead initialised either as C globals, or as C define macros, their
// values will be the same.

[unsafe]
fn static_storage(idx int, value int) byte {
	mut static storage := [256]byte{}
	if value == -1 {
		return storage[idx]
	}
	prev := storage[idx]
	storage[idx] = byte(value)
	return prev
}

// The _constructor attribute ensures that the function will be called
// before main by the C compilers that support it.
// Currently gcc/clang are known to work.
[_constructor; unsafe]
fn pre_main() {
	unsafe {
		static_storage(0, int(zzz_an_i8_const))
		static_storage(1, int(zzz_an_i16_const))
		static_storage(2, int(zzz_an_int_const))
		static_storage(3, int(zzz_an_i64_const))
		//
		static_storage(4, int(zzz_an_byte_const))
		static_storage(5, int(zzz_an_u16_const))
		static_storage(6, int(zzz_an_u32_const))
		static_storage(7, int(zzz_an_u64_const))
		//
		static_storage(8, int(zzz_an_f32_const))
		static_storage(9, int(zzz_an_f64_const))

		static_storage(20, int(zzz_an_i8_const_1))
		static_storage(21, int(zzz_an_i16_const_1))
		static_storage(22, int(zzz_an_int_const_1))
		static_storage(23, int(zzz_an_i64_const_1))
		//
		static_storage(24, int(zzz_an_byte_const_1))
		static_storage(25, int(zzz_an_u16_const_1))
		static_storage(26, int(zzz_an_u32_const_1))
		static_storage(27, int(zzz_an_u64_const_1))
		//
		static_storage(28, int(zzz_an_f32_const_1))
		static_storage(29, int(zzz_an_f64_const_1))
	}
}

fn do_check() {
	unsafe {
		assert static_storage(0, -1) == byte(zzz_an_i8_const)
		assert static_storage(1, -1) == byte(zzz_an_i16_const)
		assert static_storage(2, -1) == byte(zzz_an_int_const)
		// TODO: this should also be initialised, but is not for now,
		// since V has problems with `-9223372036854775808.str()`,
		// The generating code for i64 consts is present, but is disabled
		// for now, so they are still computed/assigned in the _vinit
		// function at runtime.
		//      assert static_storage(3, -1) == byte(zzz_an_i64_const)
		//
		assert static_storage(4, -1) == byte(zzz_an_byte_const)
		assert static_storage(5, -1) == byte(zzz_an_u16_const)
		assert static_storage(6, -1) == byte(zzz_an_u32_const)
		assert static_storage(7, -1) == byte(zzz_an_u64_const)
		// TODO: ensure these work too:
		//      assert static_storage(8, -1) == byte(zzz_an_f32_const)
		//      assert static_storage(9, -1) == byte(zzz_an_f64_const)
		//
		//		assert static_storage(20, -1) == byte(zzz_an_i8_const_1)
		//		assert static_storage(21, -1) == byte(zzz_an_i16_const_1)
		//		assert static_storage(22, -1) == byte(zzz_an_int_const_1)
		//		assert static_storage(23, -1) == byte(zzz_an_i64_const_1)
		assert static_storage(24, -1) == byte(zzz_an_byte_const_1)
		//		assert static_storage(25, -1) == byte(zzz_an_u16_const_1)
		//		assert static_storage(26, -1) == byte(zzz_an_u32_const_1)
		assert static_storage(27, -1) == byte(zzz_an_u64_const_1)
		//      assert static_storage(28, -1) == byte(zzz_an_f32_const_1)
		//      assert static_storage(29, -1) == byte(zzz_an_f64_const_1)
	}
}

fn test_consts_are_already_initialised() {
	$if gcc || clang {
		do_check()
	} $else {
		eprintln('_constructor is not supported by this C compiler, skipping consts initialisation tests...')
	}
}
