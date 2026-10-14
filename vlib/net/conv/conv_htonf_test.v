import math
import net.conv

fn test_htonf32_is_an_involution() {
	assert conv.htonf32(conv.htonf32(f32(1.0))) == f32(1.0)
	assert conv.htonf32(conv.htonf32(f32(0.5))) == f32(0.5)
	assert conv.htonf32(conv.htonf32(f32(-2.5))) == f32(-2.5)
	assert conv.htonf32(conv.htonf32(f32(3.14159))) == f32(3.14159)
}

fn test_htonf64_is_an_involution() {
	assert conv.htonf64(conv.htonf64(1.0)) == 1.0
	assert conv.htonf64(conv.htonf64(1.5)) == 1.5
	assert conv.htonf64(conv.htonf64(-2.5)) == -2.5
	assert conv.htonf64(conv.htonf64(3.14159)) == 3.14159
}

// htonf32 must swap the bytes of the IEEE 754 pattern on a little endian host.
fn test_htonf32_swaps_the_pattern_bytes() {
	$if little_endian {
		assert math.f32_bits(conv.htonf32(f32(1.0))) == u32(0x0000803f)
		assert math.f32_bits(conv.htonf32(f32(0.5))) == u32(0x0000003f)
		assert conv.htonf32(f32(1.0)) != f32(1.0)
	} $else {
		assert conv.htonf32(f32(1.0)) == f32(1.0)
		assert conv.htonf32(f32(0.5)) == f32(0.5)
	}
}

// htonf64 must swap the bytes of the IEEE 754 pattern on a little endian host.
fn test_htonf64_swaps_the_pattern_bytes() {
	$if little_endian {
		assert math.f64_bits(conv.htonf64(1.0)) == u64(0x000000000000f03f)
		assert math.f64_bits(conv.htonf64(-2.5)) == u64(0x00000000000004c0)
		assert conv.htonf64(1.0) != 1.0
	} $else {
		assert conv.htonf64(1.0) == 1.0
		assert conv.htonf64(-2.5) == -2.5
	}
}
