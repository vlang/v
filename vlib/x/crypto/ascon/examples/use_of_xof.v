// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
import encoding.hex
import x.crypto.ascon

// The material was generated from https://hashing.tools/ascon/ascon-hash
fn main() {
	msg := 'Example of Xof128 message'.bytes()
	// expected output generated from the tool, with 32, 64 dan 75-bytes output
	digest32 := hex.decode('424caaf68eb94aa251536bb6b565c0695b8944f932d011b1049df85b7f27d2f3')!
	digest64 := hex.decode('424caaf68eb94aa251536bb6b565c0695b8944f932d011b1049df85b7f27d2f3bf704643a643c3f2dcfb1e0bc73ec55781b5283966d2d1da85d89794ca5c292e')!
	digest75 := hex.decode('424caaf68eb94aa251536bb6b565c0695b8944f932d011b1049df85b7f27d2f3bf704643a643c3f2dcfb1e0bc73ec55781b5283966d2d1da85d89794ca5c292e36260815a8f10088e3804c')!

	out32 := ascon.xof128(msg, 32)!
	out64 := ascon.xof128(msg, 64)!
	out75 := ascon.xof128(msg, 75)!
	dump(out32 == digest32) // out32 == digest32: true
	dump(out64 == digest64) // out64 == digest64: true
	dump(out75 == digest75) // out75 == digest75: true

	// With object based
	mut x := ascon.new_xof128(32)
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
