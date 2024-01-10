module bits

// f32_bits returns the IEEE 754 binary representation of f,
// with the sign bit of f and the result in the same bit position.
// f32_bits(f32_from_bits(x)) == x.
pub fn f32_bits(f f32) u32 {
	p := u32(0)
	#let buffer = new ArrayBuffer(4)
	#let floatArr = new Float32Array(buffer)
	#floatArr[0] = f.val
	#let uintArr = new Uint32Array(buffer)
	#p.val = uintArr[0]

	return p
}

// f32_from_bits returns the floating-point number corresponding
// to the IEEE 754 binary representation b, with the sign bit of b
// and the result in the same bit position.
// f32_from_bits(f32_bits(x)) == x.
pub fn f32_from_bits(b u32) f32 {
	p := f32(0.0)
	#let buffer = new ArrayBuffer(4)
	#let floatArr = new Float32Array(buffer)
	#let uintArr = new Uint32Array(buffer)
	#uintArr[0] = Number(b.val)
	#p.val = floatArr[0]

	return p
}

// f64_bits returns the IEEE 754 binary representation of f,
// with the sign bit of f and the result in the same bit position,
// and f64_bits(f64_from_bits(x)) == x.
pub fn f64_bits(f f64) u64 {
	p := u64(0)
	#let buffer = new ArrayBuffer(8)
	#let floatArr = new Float64Array(buffer)
	#floatArr[0] = f.val
	#let uintArr = new BigUint64Array(buffer)
	#p.val = uintArr[0]

	return p
}

// f64_from_bits returns the floating-point number corresponding
// to the IEEE 754 binary representation b, with the sign bit of b
// and the result in the same bit position.
// f64_from_bits(f64_bits(x)) == x.
pub fn f64_from_bits(b u64) f64 {
	p := 0.0
	#let buffer = new ArrayBuffer(8)
	#let floatArr = new Float64Array(buffer)
	#let uintArr = new BigUint64Array(buffer)
	#uintArr[0] = b.val
	#p.val = floatArr[0]

	return p
}
