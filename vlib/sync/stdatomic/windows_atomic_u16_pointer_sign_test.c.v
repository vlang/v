// vtest build: gcc
// vtest vflags: -cc gcc -cstrict -no-retry-compilation
import sync.stdatomic as _

#flag windows -Werror=pointer-sign

fn C.atomic_compare_exchange_strong_u16(voidptr, voidptr, u16) bool
fn C.atomic_load_u16(voidptr) u16
fn C.atomic_store_u16(voidptr, u16)

fn test_windows_atomic_u16_compare_exchange_preserves_unsigned_bits() {
	mut object := u16(0)
	C.atomic_store_u16(voidptr(&object), u16(0x8000))
	assert C.atomic_load_u16(voidptr(&object)) == u16(0x8000)

	mut expected := u16(0x8000)
	assert C.atomic_compare_exchange_strong_u16(voidptr(&object), voidptr(&expected), u16(0xffff))
	assert object == u16(0xffff)
	assert expected == u16(0x8000)

	expected = u16(0x8000)
	assert !C.atomic_compare_exchange_strong_u16(voidptr(&object), voidptr(&expected), u16(0x1234))
	assert object == u16(0xffff)
	assert expected == u16(0xffff)
}
