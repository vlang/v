#include "@DIR/client.h"

fn C.v_fabs_client_bits(input u64) u64

struct FabsBits {
	input    u64
	expected u64
}

fn main() {
	// Explicit expected vectors keep the oracle independent of math.abs,
	// copysign, and the implementation's sign-mask expression.
	cases := [
		FabsBits{0x0000000000000000, 0x0000000000000000},
		FabsBits{0x8000000000000000, 0x0000000000000000},
		FabsBits{0x3ff0000000000000, 0x3ff0000000000000},
		FabsBits{0xbff0000000000000, 0x3ff0000000000000},
		FabsBits{0x3ff4000000000000, 0x3ff4000000000000},
		FabsBits{0xbff4000000000000, 0x3ff4000000000000},
		FabsBits{0x400921fb54442d18, 0x400921fb54442d18},
		FabsBits{0xc00921fb54442d18, 0x400921fb54442d18},
		FabsBits{0x0000000000000001, 0x0000000000000001},
		FabsBits{0x8000000000000001, 0x0000000000000001},
		FabsBits{0x000fffffffffffff, 0x000fffffffffffff},
		FabsBits{0x800fffffffffffff, 0x000fffffffffffff},
		FabsBits{0x0010000000000000, 0x0010000000000000},
		FabsBits{0x8010000000000000, 0x0010000000000000},
		FabsBits{0x7fefffffffffffff, 0x7fefffffffffffff},
		FabsBits{0xffefffffffffffff, 0x7fefffffffffffff},
		FabsBits{0x7ff0000000000000, 0x7ff0000000000000},
		FabsBits{0xfff0000000000000, 0x7ff0000000000000},
		FabsBits{0x7ff8000000000000, 0x7ff8000000000000},
		FabsBits{0xfff8000000000000, 0x7ff8000000000000},
		FabsBits{0x7ff8000000000001, 0x7ff8000000000001},
		FabsBits{0xfff8000000000001, 0x7ff8000000000001},
		FabsBits{0x7ff8badcafe12345, 0x7ff8badcafe12345},
		FabsBits{0xfff8badcafe12345, 0x7ff8badcafe12345},
		FabsBits{0x7fffffffffffffff, 0x7fffffffffffffff},
		FabsBits{0xffffffffffffffff, 0x7fffffffffffffff},
	]!
	for index, row in cases {
		actual := C.v_fabs_client_bits(row.input)
		if actual != row.expected {
			println('fabs row ${index}: ${actual.hex()} expected ${row.expected.hex()}')
			exit(1)
		}
	}
	println('freestanding Linux fabs C ABI passed')
}
