// vtest build: gcc
// vtest vflags: -cc gcc -cstrict -no-retry-compilation
import sync.stdatomic

#flag windows -Werror=pointer-sign

fn C.atomic_compare_exchange_strong_u64(voidptr, voidptr, u64) bool

fn test_windows_atomic_u64_compare_exchange_preserves_unsigned_bits() {
	mut object := u64(0)
	stdatomic.store_u64(&object, u64(0x8000000000000000))
	assert stdatomic.load_u64(&object) == u64(0x8000000000000000)

	mut expected := u64(0x8000000000000000)
	assert C.atomic_compare_exchange_strong_u64(voidptr(&object), voidptr(&expected),
		u64(0xffffffffffffffff))
	assert object == u64(0xffffffffffffffff)
	assert expected == u64(0x8000000000000000)

	expected = u64(0x8000000000000000)
	assert !C.atomic_compare_exchange_strong_u64(voidptr(&object), voidptr(&expected),
		u64(0x0123456789abcdef))
	assert object == u64(0xffffffffffffffff)
	assert expected == u64(0xffffffffffffffff)
}
