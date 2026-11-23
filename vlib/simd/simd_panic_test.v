module main

import os
import simd

const simd_panic_probe_arg = '--vtest-simd-panic'

fn testsuite_begin() {
	if os.args.len != 3 || os.args[1] != simd_panic_probe_arg {
		return
	}
	match os.args[2] {
		'load_at' { println(simd.load_f32x4_at([]f32{len: 6}, 3)) }
		'load_at_negative' { println(simd.load_i8x16_at([]i8{len: 32}, -1)) }
		'store_at' {
			mut dst := []u16{len: 7}
			simd.splat_u16x8(1).store_at(mut dst, 0)
		}
		'lane' { println(simd.splat_i64x2(1)[2]) }
		'set_lane' {
			mut v := simd.splat_f64x4(1)
			v[-1] = 2
		}
		'mask_lane' { println(simd.splat_mask8x16(true)[16]) }
		'div_zero' { println(simd.splat_i32x4(1) / simd.i32x4(1, 2, 0, 4)) }
		'div_zero_unsigned' { println(simd.splat_u8x16(1) / simd.splat_u8x16(0)) }
		else {}
	}
	exit(0)
}

const cases = {
	'load_at':           'simd.load_f32x4_at: offset 3 out of range for length 6'
	'load_at_negative':  'simd.load_i8x16_at: offset -1 out of range for length 32'
	'store_at':          'simd.U16x8.store_at: offset 0 out of range for length 7'
	'lane':              'simd: lane index 2 out of range for 2 lanes'
	'set_lane':          'simd: lane index -1 out of range for 4 lanes'
	'mask_lane':         'simd: lane index 16 out of range for 16 lanes'
	'div_zero':          'division by zero'
	'div_zero_unsigned': 'division by zero'
}

fn test_out_of_range_and_division_by_zero_panic() {
	executable := os.executable()
	for name, message in cases {
		res := os.exec([executable, simd_panic_probe_arg, name])
		assert res.exit_code != 0, name
		assert res.output.contains(message), '${name}: ${res.output}'
	}
}
