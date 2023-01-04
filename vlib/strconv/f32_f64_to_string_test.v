/**********************************************************************
*
* Float to string Test
*
**********************************************************************/
import strconv
import math

union Ufloat32 {
mut:
	f f32 = f32(0)
	b u32
}

union Ufloat64 {
mut:
	f f64 = f64(0)
	b u64
}

fn f64_from_bits1(b u64) f64 {
	mut x := Ufloat64{}
	x.b = b
	// C.printf("bin: %016llx\n",x.f)
	return unsafe { x.f }
}

fn f32_from_bits1(b u32) f32 {
	mut x := Ufloat32{}
	x.b = b
	// C.printf("bin: %08x\n",x.f)
	return unsafe { x.f }
}

fn test_float_to_str() {
	test_cases_f32 := [
		f32_from_bits1(0x0000_0000), // +0
		f32_from_bits1(0x8000_0000), // -0
		f32_from_bits1(0xFFC0_0001), // sNan
		f32_from_bits1(0xFF80_0001), // qNan
		f32_from_bits1(0x7F80_0000), // +inf
		f32_from_bits1(0xFF80_0000), // -inf
		1,
		-1,
		10,
		-10,
		0.3,
		-0.3,
		1000000,
		123456.7,
		123e35,
		-123.45,
		1e23,
		f32_from_bits1(0x0080_0000), // smallest float32
		math.max_f32,
		383260575764816448.0,
	]

	exp_result_f32 := [
		'0e+00',
		'-0e+00',
		'nan',
		'nan',
		'+inf',
		'-inf',
		'1.e+00',
		'-1.e+00',
		'1.e+01',
		'-1.e+01',
		'3.e-01',
		'-3.e-01',
		'1.e+06',
		'1.234567e+05',
		'1.23e+37',
		'-1.2345e+02',
		'1.e+23',
		'1.1754944e-38', // aprox from 1.1754943508 × 10−38,
		'3.4028235e+38',
		'3.8326058e+17',
	]

	test_cases_f64 := [
		f64_from_bits1(0x0000_0000_0000_0000), // +0
		f64_from_bits1(0x8000_0000_0000_0000), // -0
		f64_from_bits1(0x7FF0_0000_0000_0001), // sNan
		f64_from_bits1(0x7FF8_0000_0000_0001), // qNan
		f64_from_bits1(0x7FF0_0000_0000_0000), // +inf
		f64_from_bits1(0xFFF0_0000_0000_0000), // -inf
		1,
		-1,
		10,
		-10,
		0.3,
		-0.3,
		1000000,
		123456.7,
		123e45,
		-123.45,
		1e23,
		f64_from_bits1(0x0010_0000_0000_0000), // smallest float64
		math.max_f32,
		383260575764816448,
		383260575764816448,
		// C failing cases
		123e300,
		123e-300,
		5.e-324,
		-5.e-324,
	]

	exp_result_f64 := [
		'0e+00',
		'-0e+00',
		'nan',
		'nan',
		'+inf',
		'-inf',
		'1.e+00',
		'-1.e+00',
		'1.e+01',
		'-1.e+01',
		'3.e-01',
		'-3.e-01',
		'1.e+06',
		'1.234567e+05',
		'1.23e+47',
		'-1.2345e+02',
		'1.e+23',
		'2.2250738585072014e-308',
		'3.4028234663852886e+38',
		'3.8326057576481645e+17',
		'3.8326057576481645e+17',
		'1.23e+302', // this test is failed from C sprintf!!
		'1.23e-298',
		'5.e-324',
		'-5.e-324',
	]

	// test f32
	for c, x in test_cases_f32 {
		// println(x)
		s := strconv.f32_to_str(x, 8)
		s1 := exp_result_f32[c]
		// println("$s1 $s")
		assert s == s1
	}

	// test f64
	for c, x in test_cases_f64 {
		s := strconv.f64_to_str(x, 17)
		s1 := exp_result_f64[c]
		// println("$s1 $s")
		assert s == s1
	}

	// test long format
	assert strconv.f64_to_str_l('1e1'.f64()).len == 4 // '10.0'
	assert strconv.f64_to_str_l('1e-1'.f64()).len == 3 // '0.1'

	for exp := 2; exp < 120; exp++ {
		a := strconv.f64_to_str_l(('1e' + exp.str()).f64())
		// println(a)
		assert a.len == exp + 3

		b := strconv.f64_to_str_l(('1e-' + exp.str()).f64())
		// println(b)
		assert b.len == exp + 2
	}

	// test rounding str conversion
	// println( ftoa.f64_to_str(0.3456789123456, 4) )
	// assert ftoa.f64_to_str(0.3456789123456, 4)=="3.4568e-01"
	// assert ftoa.f32_to_str(0.345678, 3)=="3.457e-01"
}
