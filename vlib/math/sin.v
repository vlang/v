module math

import math.internal

const (
	sin_data = [
		-0.3295190160663511504173,
		0.0025374284671667991990,
		0.0006261928782647355874,
		-4.6495547521854042157541e-06,
		-5.6917531549379706526677e-07,
		3.7283335140973803627866e-09,
		3.0267376484747473727186e-10,
		-1.7400875016436622322022e-12,
		-1.0554678305790849834462e-13,
		5.3701981409132410797062e-16,
		2.5984137983099020336115e-17,
		-1.1821555255364833468288e-19,
	]
	sin_cs   = ChebSeries{
		c: sin_data
		order: 11
		a: -1
		b: 1
	}
	cos_data = [
		0.165391825637921473505668118136,
		-0.00084852883845000173671196530195,
		-0.000210086507222940730213625768083,
		1.16582269619760204299639757584e-6,
		1.43319375856259870334412701165e-7,
		-7.4770883429007141617951330184e-10,
		-6.0969994944584252706997438007e-11,
		2.90748249201909353949854872638e-13,
		1.77126739876261435667156490461e-14,
		-7.6896421502815579078577263149e-17,
		-3.7363121133079412079201377318e-18,
	]
	cos_cs   = ChebSeries{
		c: cos_data
		order: 10
		a: -1
		b: 1
	}
)

// sin calculates the sine of the angle in radians
pub fn sin(x f64) f64 {
	p1 := 7.85398125648498535156e-1
	p2 := 3.77489470793079817668e-8
	p3 := 2.69515142907905952645e-15
	sgn_x := if x < 0 { -1 } else { 1 }
	abs_x := abs(x)
	if abs_x < internal.root4_f64_epsilon {
		x2 := x * x
		return x * (1.0 - x2 / 6.0)
	} else {
		mut sgn_result := sgn_x
		mut y := floor(abs_x / (0.25 * pi))
		mut octant := int(y - ldexp(floor(ldexp(y, -3)), 3))
		if (octant & 1) == 1 {
			octant++
			octant &= 7
			y += 1.0
		}
		if octant > 3 {
			octant -= 4
			sgn_result = -sgn_result
		}
		z := ((abs_x - y * p1) - y * p2) - y * p3
		mut result := 0.0
		if octant == 0 {
			t := 8.0 * abs(z) / pi - 1.0
			sin_cs_val, _ := math.sin_cs.eval_e(t)
			result = z * (1.0 + z * z * sin_cs_val)
		} else {
			t := 8.0 * abs(z) / pi - 1.0
			cos_cs_val, _ := math.cos_cs.eval_e(t)
			result = 1.0 - 0.5 * z * z * (1.0 - z * z * cos_cs_val)
		}
		result *= sgn_result
		return result
	}
}

// cos calculates the cosine of the angle in radians
pub fn cos(x f64) f64 {
	p1 := 7.85398125648498535156e-1
	p2 := 3.77489470793079817668e-8
	p3 := 2.69515142907905952645e-15
	abs_x := abs(x)
	if abs_x < internal.root4_f64_epsilon {
		x2 := x * x
		return 1.0 - 0.5 * x2
	} else {
		mut sgn_result := 1
		mut y := floor(abs_x / (0.25 * pi))
		mut octant := int(y - ldexp(floor(ldexp(y, -3)), 3))
		if (octant & 1) == 1 {
			octant++
			octant &= 7
			y += 1.0
		}
		if octant > 3 {
			octant -= 4
			sgn_result = -sgn_result
		}
		if octant > 1 {
			sgn_result = -sgn_result
		}
		z := ((abs_x - y * p1) - y * p2) - y * p3
		mut result := 0.0
		if octant == 0 {
			t := 8.0 * abs(z) / pi - 1.0
			cos_cs_val, _ := math.cos_cs.eval_e(t)
			result = 1.0 - 0.5 * z * z * (1.0 - z * z * cos_cs_val)
		} else {
			t := 8.0 * abs(z) / pi - 1.0
			sin_cs_val, _ := math.sin_cs.eval_e(t)
			result = z * (1.0 + z * z * sin_cs_val)
		}
		result *= sgn_result
		return result
	}
}

// cosf calculates cosine in radians (float32).
[inline]
pub fn cosf(a f32) f32 {
	return f32(cos(a))
}

// sinf calculates sine in radians (float32)
[inline]
pub fn sinf(a f32) f32 {
	return f32(sin(a))
}

// sincos calculates the sine and cosine of the angle in radians
pub fn sincos(x f64) (f64, f64) {
	p1 := 7.85398125648498535156e-1
	p2 := 3.77489470793079817668e-8
	p3 := 2.69515142907905952645e-15
	sgn_x := if x < 0 { -1 } else { 1 }
	abs_x := abs(x)
	if abs_x < internal.root4_f64_epsilon {
		x2 := x * x
		return x * (1.0 - x2 / 6.0), 1.0 - 0.5 * x2
	} else {
		mut sgn_result_sin := sgn_x
		mut sgn_result_cos := 1
		mut y := floor(abs_x / (0.25 * pi))
		mut octant := int(y - ldexp(floor(ldexp(y, -3)), 3))
		if (octant & 1) == 1 {
			octant++
			octant &= 7
			y += 1.0
		}
		if octant > 3 {
			octant -= 4
			sgn_result_sin = -sgn_result_sin
			sgn_result_cos = -sgn_result_cos
		}
		sgn_result_cos = if octant > 1 { -sgn_result_cos } else { sgn_result_cos }
		z := ((abs_x - y * p1) - y * p2) - y * p3
		t := 8.0 * abs(z) / pi - 1.0
		sin_cs_val, _ := math.sin_cs.eval_e(t)
		cos_cs_val, _ := math.cos_cs.eval_e(t)
		mut result_sin := 0.0
		mut result_cos := 0.0
		if octant == 0 {
			result_sin = z * (1.0 + z * z * sin_cs_val)
			result_cos = 1.0 - 0.5 * z * z * (1.0 - z * z * cos_cs_val)
		} else {
			result_sin = 1.0 - 0.5 * z * z * (1.0 - z * z * cos_cs_val)
			result_cos = z * (1.0 + z * z * sin_cs_val)
		}
		result_sin *= sgn_result_sin
		result_cos *= sgn_result_cos
		return result_sin, result_cos
	}
}
