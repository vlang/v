// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
import encoding.hex
import x.crypto.ascon

// The material was generated from https://hashing.tools/ascon/ascon-hash
fn main() {
	msg := 'Example of CXof128 message'.bytes()
	cs := 'custom-string-cxof128'.bytes()

	// expected output generated from the tool, with 32, 64 dan 75-bytes output
	digest32 := hex.decode('d71492b816b1ac27f53f9c13be45c1d2d0530b8dde7fde8d34cb563f79b3d3d3')!
	digest64 := hex.decode('d71492b816b1ac27f53f9c13be45c1d2d0530b8dde7fde8d34cb563f79b3d3d3601d03474ec6fe1f6b8dc5dd79bea20aff4c95ca3549202b1aaeb9e66b5df398')!
	digest75 := hex.decode('d71492b816b1ac27f53f9c13be45c1d2d0530b8dde7fde8d34cb563f79b3d3d3601d03474ec6fe1f6b8dc5dd79bea20aff4c95ca3549202b1aaeb9e66b5df3985a88fd8bce0f9570962321')!

	out32 := ascon.cxof128(msg, 32, cs)!
	out64 := ascon.cxof128(msg, 64, cs)!
	out75 := ascon.cxof128(msg, 75, cs)!
	dump(out32 == digest32) // out32 == digest32: true
	dump(out64 == digest64) // out64 == digest64: true
	dump(out75 == digest75) // out75 == digest75: true

	// With object based
	mut x := ascon.new_cxof128(32, cs)!
	s32 := x.sum(msg)
	dump(s32 == digest32) // s32 == digest32: true

	// with sized output
	x.reset()
	_ := x.write(msg)!
	mut b64 := []u8{len: 64}
	_ := x.read(mut b64)!
	dump(b64 == digest64) // b64 == digest64: true

	x.reset()
	_ := x.write(msg)!
	mut b75 := []u8{len: 75}
	_ := x.read(mut b75)!
	dump(b75 == digest75) // b75 == digest75: true
}
