import math

fn test_around() {
	// do not pass 0
	for f in [f32(1e-12), 1e-9, 1e-6, 1e-4, 0.1, 0.2, 0.5, 1, 2, 5, 10, 1e6, 1e7, 1e9, 1e12] {
		check_around(f, 1000)
	}
}

fn check_around(fval f32, total int) {
	check(fval, total)
	check(-fval, total)
}

fn check(fval f32, total int) {
	middle_bits := math.f32_bits(fval)
	start_bits := middle_bits - u32(total / 2)
	end_bits := middle_bits + u32(total / 2)
	min, max := if start_bits < end_bits { start_bits, end_bits } else { end_bits, start_bits }
	println('> check_around ${total} f32 values around fval: ${fval:26.12f}, middle_bits for fval: ${middle_bits:12} | min: ${min:12} | max: ${max:12}')
	for ux in min .. max {
		x := math.f32_from_bits(ux)
		if math.is_nan(x) {
			continue
		}
		uy := ux - 1
		y := math.f32_from_bits(uy)
		if x == y {
			continue
		}
		sx := x.str()
		sy := y.str()
		assert sx != sy, 'math.f32_from_bits(${ux})'
	}
}
