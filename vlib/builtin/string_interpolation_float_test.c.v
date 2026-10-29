// vtest vflags: -gc vgc
import math

fn test_fixed_precision_float_interpolation() {
	a := 10.3
	b := 0.123456
	c := 200.25
	d := 7.0
	assert 'rect|${a:.1f}|${b:.1f}|${c:.1f}|${d:.1f}|${a:.4f}|${b:.4f}|${c:.4f}|${d:.4f}|${a:.1f}|${b:.1f}' == 'rect|10.3|0.1|200.3|7.0|10.3000|0.1235|200.2500|7.0000|10.3|0.1'
	assert '${f32(a):.1f}|${f32(b):.4f}' == '10.3|0.1235'
	negative := -0.125
	assert '${negative:+010.2f}|${negative:-8.2f}' == '-000000.13|-0.13   '
	negative_zero := math.f64_from_bits(u64(1) << 63)
	assert '${negative_zero:.3f}' == '-0.000'
	rounded := 999.5
	assert '${rounded:.0f}' == '1000'
	exact := 0.1
	assert '${exact:.20f}' == '0.10000000000000000000'
}

fn test_fixed_precision_interpolation_allocation_budget() {
	$if vgc ? {
		a := 10.3
		b := 0.123456
		c := 200.25
		d := 7.0
		mut length := 0
		before := gc_heap_usage().total_bytes
		for _ in 0 .. 1000 {
			s := 'rect|${a:.1f}|${b:.1f}|${c:.1f}|${d:.1f}|${a:.4f}|${b:.4f}|${c:.4f}|${d:.4f}|${a:.1f}|${b:.1f}'
			length += s.len
		}
		after := gc_heap_usage().total_bytes
		assert length == 63000
		// Allow the result and builder storage, while rejecting per-hole temporary strings.
		assert after - before <= 1000 * 256
	}
}

fn test_str_intp_fixed_float_builder_path() {
	data := [StrIntpData{
		str: 'exact|'
		fmt: get_str_intp_u32_format(.si_f64, 0, 20, false, false, ` `, 0, false)
		d:   StrIntpMem{ d_f64: 0.1 }
	}, StrIntpData{
		str: '|'
		fmt: get_str_intp_u32_format(.si_f64, 0, 2, false, false, ` `, 0, true)
		d:   StrIntpMem{ d_f64: math.inf(1) }
	}]
	assert str_intp(data.len, &data[0]) == 'exact|0.10000000000000000555|+INF'
}

fn next_interpolated_float(mut calls []int) f64 {
	calls[0]++
	return f64(calls[0]) + 0.25
}

fn test_float_interpolation_buffers_preserve_evaluation_order() {
	mut calls := [0]
	assert '${next_interpolated_float(mut calls):.1f}|${next_interpolated_float(mut calls):.2f}|${next_interpolated_float(mut calls):.0f}' == '1.3|2.25|3'
	assert calls[0] == 3
}

fn generic_fixed_float[T](value T) string {
	return 'value|${value:.2f}'
}

fn test_generic_fixed_float_interpolation_keeps_conversion_helpers() {
	assert generic_fixed_float(1.25) == 'value|1.25'
	assert generic_fixed_float(f32(2.5)) == 'value|2.50'
}
