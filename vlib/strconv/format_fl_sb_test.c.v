// vtest vflags: -gc vgc
module strconv

import math
import strings

fn test_format_fl_sb_matches_string_formatter() {
	mut sb := strings.new_builder(2048)
	values := [0.0, -0.0, 0.1, -0.125, 0.375, 2.5, 3.5, 9.5, 99.5, 999.5, 0.00001, 10.3, 0.123456,
		200.25, 7.0, 1e23, math.max_f64, math.smallest_non_zero_f64,
		math.f64_from_bits(0x000fffffffffffff), math.inf(1), math.inf(-1), math.nan()]
	for value in values {
		for precision in [-1, 0, 1, 2, 6, 20, 126, 324, 1100] {
			for variant in 0 .. 6 {
				p := BF_param{
					len0:         if variant == 0 { 0 } else { 48 }
					len1:         precision
					positive:     variant % 2 == 0
					sign_flag:    variant % 3 == 0
					pad_ch:       if variant % 2 == 0 { `0` } else { ` ` }
					align:        if variant < 3 { .right } else { .left }
					rm_tail_zero: variant % 2 == 0
				}
				sb.write_string('prefix|')
				format_fl_sb(value, p, mut sb)
				sb.write_string('|suffix')
				assert sb.str() == 'prefix|' + format_fl(value, p) + '|suffix'
			}
		}
	}
}

fn test_format_fl_sb_matches_varied_binary_values() {
	mut sb := strings.new_builder(1024)
	mut bits := u64(0x123456789abcdef)
	for i in 0 .. 200 {
		bits = bits * 6364136223846793005 + 1442695040888963407
		value := math.f64_from_bits(bits)
		p := BF_param{
			len1:         [0, 1, 4, 20, 126, 350][i % 6]
			positive:     bits >> 63 == 0
			rm_tail_zero: i % 2 == 0
		}
		format_fl_sb(value, p, mut sb)
		assert sb.str() == format_fl(value, p)
	}
}

fn test_format_fl_sb_does_not_allocate_for_finite_values() {
	$if vgc ? {
		mut sb := strings.new_builder(2048)
		values := [10.3, 0.123456, 200.25, 7.0, -0.125, 99.5, math.max_f64, math.smallest_non_zero_f64]
		before := gc_heap_usage().total_bytes
		for _ in 0 .. 100 {
			for value in values {
				format_fl_sb(value, BF_param{ len1: 350 }, mut sb)
				sb.clear()
			}
		}
		after := gc_heap_usage().total_bytes
		assert after == before
	}
}
