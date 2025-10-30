import rand
import x.crypto.poly1305

fn bundle(x []u8) !([12]u8, &poly1305.Poly1305) {
	mut x12 := [12]u8{}
	mut x32 := []u8{len: 32}

	unsafe {
		vmemcpy(x12, x.data, 12)
		vmemcpy(x32.data, x.data, 32)
	}
	po := poly1305.new(x32)!
	return x12, po
}

fn test_main() {
	x := rand.bytes(45)!
	a, _ := bundle(x)!
	assert a[0..12] == x[0..12]
}
