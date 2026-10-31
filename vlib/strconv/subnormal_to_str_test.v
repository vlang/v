// Subnormal f32/f64 values have fewer significant bits than normal ones, so their shortest
// round-trip digits (`1e-45`, `5e-324`) can not be padded or rounded again to get a fixed
// precision: the digits come from the exact value instead (issue #29943).
// The expected strings are C's `printf("%.*e", n_digit, x)` on macOS, cross-checked with Python.
import math
import strconv

struct F32Case {
	bits    u32
	n_digit int
	want    string
}

struct F64Case {
	bits    u64
	n_digit int
	want    string
}

const f32_cases = [
	F32Case{0x00000001, 0, '1e-45'},
	F32Case{0x00000001, 1, '1.4e-45'},
	F32Case{0x00000001, 2, '1.40e-45'},
	F32Case{0x00000001, 3, '1.401e-45'},
	F32Case{0x00000001, 5, '1.40130e-45'},
	F32Case{0x00000001, 8, '1.40129846e-45'},
	F32Case{0x00000001, 12, '1.401298464325e-45'},
	F32Case{0x00000001, 20, '1.40129846432481707092e-45'},
	F32Case{0x00000002, 0, '3e-45'},
	F32Case{0x00000002, 1, '2.8e-45'},
	F32Case{0x00000002, 2, '2.80e-45'},
	F32Case{0x00000002, 3, '2.803e-45'},
	F32Case{0x00000002, 5, '2.80260e-45'},
	F32Case{0x00000002, 8, '2.80259693e-45'},
	F32Case{0x00000002, 12, '2.802596928650e-45'},
	F32Case{0x00000002, 20, '2.80259692864963414185e-45'},
	F32Case{0x00000003, 0, '4e-45'},
	F32Case{0x00000003, 1, '4.2e-45'},
	F32Case{0x00000003, 2, '4.20e-45'},
	F32Case{0x00000003, 3, '4.204e-45'},
	F32Case{0x00000003, 5, '4.20390e-45'},
	F32Case{0x00000003, 8, '4.20389539e-45'},
	F32Case{0x00000003, 12, '4.203895392974e-45'},
	F32Case{0x00000003, 20, '4.20389539297445121277e-45'},
	F32Case{0x00000020, 0, '4e-44'},
	F32Case{0x00000020, 1, '4.5e-44'},
	F32Case{0x00000020, 2, '4.48e-44'},
	F32Case{0x00000020, 3, '4.484e-44'},
	F32Case{0x00000020, 5, '4.48416e-44'},
	F32Case{0x00000020, 8, '4.48415509e-44'},
	F32Case{0x00000020, 12, '4.484155085839e-44'},
	F32Case{0x00000020, 20, '4.48415508583941462696e-44'},
	F32Case{0x00003039, 0, '2e-41'},
	F32Case{0x00003039, 1, '1.7e-41'},
	F32Case{0x00003039, 2, '1.73e-41'},
	F32Case{0x00003039, 3, '1.730e-41'},
	F32Case{0x00003039, 5, '1.72990e-41'},
	F32Case{0x00003039, 8, '1.72990295e-41'},
	F32Case{0x00003039, 12, '1.729902954209e-41'},
	F32Case{0x00003039, 20, '1.72990295420898667406e-41'},
	F32Case{0x00400000, 0, '6e-39'},
	F32Case{0x00400000, 1, '5.9e-39'},
	F32Case{0x00400000, 2, '5.88e-39'},
	F32Case{0x00400000, 3, '5.877e-39'},
	F32Case{0x00400000, 5, '5.87747e-39'},
	F32Case{0x00400000, 8, '5.87747175e-39'},
	F32Case{0x00400000, 12, '5.877471754111e-39'},
	F32Case{0x00400000, 20, '5.87747175411143753984e-39'},
	F32Case{0x00555555, 0, '8e-39'},
	F32Case{0x00555555, 1, '7.8e-39'},
	F32Case{0x00555555, 2, '7.84e-39'},
	F32Case{0x00555555, 3, '7.837e-39'},
	F32Case{0x00555555, 5, '7.83663e-39'},
	F32Case{0x00555555, 8, '7.83662854e-39'},
	F32Case{0x00555555, 12, '7.836628538382e-39'},
	F32Case{0x00555555, 20, '7.83662853838242861152e-39'},
	F32Case{0x007ffffe, 0, '1e-38'},
	F32Case{0x007ffffe, 1, '1.2e-38'},
	F32Case{0x007ffffe, 2, '1.18e-38'},
	F32Case{0x007ffffe, 3, '1.175e-38'},
	F32Case{0x007ffffe, 5, '1.17549e-38'},
	F32Case{0x007ffffe, 8, '1.17549407e-38'},
	F32Case{0x007ffffe, 12, '1.175494070563e-38'},
	F32Case{0x007ffffe, 20, '1.17549407056259464301e-38'},
	F32Case{0x007fffff, 0, '1e-38'},
	F32Case{0x007fffff, 1, '1.2e-38'},
	F32Case{0x007fffff, 2, '1.18e-38'},
	F32Case{0x007fffff, 3, '1.175e-38'},
	F32Case{0x007fffff, 5, '1.17549e-38'},
	F32Case{0x007fffff, 8, '1.17549421e-38'},
	F32Case{0x007fffff, 12, '1.175494210692e-38'},
	F32Case{0x007fffff, 20, '1.17549421069244107549e-38'},
	F32Case{0x80000001, 0, '-1e-45'},
	F32Case{0x80000001, 1, '-1.4e-45'},
	F32Case{0x80000001, 2, '-1.40e-45'},
	F32Case{0x80000001, 3, '-1.401e-45'},
	F32Case{0x80000001, 5, '-1.40130e-45'},
	F32Case{0x80000001, 8, '-1.40129846e-45'},
	F32Case{0x80000001, 12, '-1.401298464325e-45'},
	F32Case{0x80000001, 20, '-1.40129846432481707092e-45'},
	F32Case{0x8012d687, 0, '-2e-39'},
	F32Case{0x8012d687, 1, '-1.7e-39'},
	F32Case{0x8012d687, 2, '-1.73e-39'},
	F32Case{0x8012d687, 3, '-1.730e-39'},
	F32Case{0x8012d687, 5, '-1.73000e-39'},
	F32Case{0x8012d687, 8, '-1.72999684e-39'},
	F32Case{0x8012d687, 12, '-1.729996841206e-39'},
	F32Case{0x8012d687, 20, '-1.72999684120609643680e-39'},
]

const f64_cases = [
	F64Case{0x0000000000000001, 0, '5e-324'},
	F64Case{0x0000000000000001, 1, '4.9e-324'},
	F64Case{0x0000000000000001, 2, '4.94e-324'},
	F64Case{0x0000000000000001, 3, '4.941e-324'},
	F64Case{0x0000000000000001, 5, '4.94066e-324'},
	F64Case{0x0000000000000001, 10, '4.9406564584e-324'},
	F64Case{0x0000000000000001, 16, '4.9406564584124654e-324'},
	F64Case{0x0000000000000001, 17, '4.94065645841246544e-324'},
	F64Case{0x0000000000000001, 20, '4.94065645841246544177e-324'},
	F64Case{0x0000000000000001, 25, '4.9406564584124654417656879e-324'},
	F64Case{0x0000000000000002, 0, '1e-323'},
	F64Case{0x0000000000000002, 1, '9.9e-324'},
	F64Case{0x0000000000000002, 2, '9.88e-324'},
	F64Case{0x0000000000000002, 3, '9.881e-324'},
	F64Case{0x0000000000000002, 5, '9.88131e-324'},
	F64Case{0x0000000000000002, 10, '9.8813129168e-324'},
	F64Case{0x0000000000000002, 16, '9.8813129168249309e-324'},
	F64Case{0x0000000000000002, 17, '9.88131291682493088e-324'},
	F64Case{0x0000000000000002, 20, '9.88131291682493088353e-324'},
	F64Case{0x0000000000000002, 25, '9.8813129168249308835313759e-324'},
	F64Case{0x0000000000000003, 0, '1e-323'},
	F64Case{0x0000000000000003, 1, '1.5e-323'},
	F64Case{0x0000000000000003, 2, '1.48e-323'},
	F64Case{0x0000000000000003, 3, '1.482e-323'},
	F64Case{0x0000000000000003, 5, '1.48220e-323'},
	F64Case{0x0000000000000003, 10, '1.4821969375e-323'},
	F64Case{0x0000000000000003, 16, '1.4821969375237396e-323'},
	F64Case{0x0000000000000003, 17, '1.48219693752373963e-323'},
	F64Case{0x0000000000000003, 20, '1.48219693752373963253e-323'},
	F64Case{0x0000000000000003, 25, '1.4821969375237396325297064e-323'},
	F64Case{0x0000000000000005, 0, '2e-323'},
	F64Case{0x0000000000000005, 1, '2.5e-323'},
	F64Case{0x0000000000000005, 2, '2.47e-323'},
	F64Case{0x0000000000000005, 3, '2.470e-323'},
	F64Case{0x0000000000000005, 5, '2.47033e-323'},
	F64Case{0x0000000000000005, 10, '2.4703282292e-323'},
	F64Case{0x0000000000000005, 16, '2.4703282292062327e-323'},
	F64Case{0x0000000000000005, 17, '2.47032822920623272e-323'},
	F64Case{0x0000000000000005, 20, '2.47032822920623272088e-323'},
	F64Case{0x0000000000000005, 25, '2.4703282292062327208828440e-323'},
	F64Case{0x0000000040000000, 0, '5e-315'},
	F64Case{0x0000000040000000, 1, '5.3e-315'},
	F64Case{0x0000000040000000, 2, '5.30e-315'},
	F64Case{0x0000000040000000, 3, '5.305e-315'},
	F64Case{0x0000000040000000, 5, '5.30499e-315'},
	F64Case{0x0000000040000000, 10, '5.3049894774e-315'},
	F64Case{0x0000000040000000, 16, '5.3049894774131808e-315'},
	F64Case{0x0000000040000000, 17, '5.30498947741318079e-315'},
	F64Case{0x0000000040000000, 20, '5.30498947741318078778e-315'},
	F64Case{0x0000000040000000, 25, '5.3049894774131807877784555e-315'},
	F64Case{0x0008000000000000, 0, '1e-308'},
	F64Case{0x0008000000000000, 1, '1.1e-308'},
	F64Case{0x0008000000000000, 2, '1.11e-308'},
	F64Case{0x0008000000000000, 3, '1.113e-308'},
	F64Case{0x0008000000000000, 5, '1.11254e-308'},
	F64Case{0x0008000000000000, 10, '1.1125369293e-308'},
	F64Case{0x0008000000000000, 16, '1.1125369292536007e-308'},
	F64Case{0x0008000000000000, 17, '1.11253692925360069e-308'},
	F64Case{0x0008000000000000, 20, '1.11253692925360069155e-308'},
	F64Case{0x0008000000000000, 25, '1.1125369292536006915451164e-308'},
	F64Case{0x0005555555555555, 0, '7e-309'},
	F64Case{0x0005555555555555, 1, '7.4e-309'},
	F64Case{0x0005555555555555, 2, '7.42e-309'},
	F64Case{0x0005555555555555, 3, '7.417e-309'},
	F64Case{0x0005555555555555, 5, '7.41691e-309'},
	F64Case{0x0005555555555555, 10, '7.4169128617e-309'},
	F64Case{0x0005555555555555, 16, '7.4169128616906696e-309'},
	F64Case{0x0005555555555555, 17, '7.41691286169066963e-309'},
	F64Case{0x0005555555555555, 20, '7.41691286169066963008e-309'},
	F64Case{0x0005555555555555, 25, '7.4169128616906696300819563e-309'},
	F64Case{0x000ffffffffffffe, 0, '2e-308'},
	F64Case{0x000ffffffffffffe, 1, '2.2e-308'},
	F64Case{0x000ffffffffffffe, 2, '2.23e-308'},
	F64Case{0x000ffffffffffffe, 3, '2.225e-308'},
	F64Case{0x000ffffffffffffe, 5, '2.22507e-308'},
	F64Case{0x000ffffffffffffe, 10, '2.2250738585e-308'},
	F64Case{0x000ffffffffffffe, 16, '2.2250738585072004e-308'},
	F64Case{0x000ffffffffffffe, 17, '2.22507385850720039e-308'},
	F64Case{0x000ffffffffffffe, 20, '2.22507385850720039496e-308'},
	F64Case{0x000ffffffffffffe, 25, '2.2250738585072003949589410e-308'},
	F64Case{0x000fffffffffffff, 0, '2e-308'},
	F64Case{0x000fffffffffffff, 1, '2.2e-308'},
	F64Case{0x000fffffffffffff, 2, '2.23e-308'},
	F64Case{0x000fffffffffffff, 3, '2.225e-308'},
	F64Case{0x000fffffffffffff, 5, '2.22507e-308'},
	F64Case{0x000fffffffffffff, 10, '2.2250738585e-308'},
	F64Case{0x000fffffffffffff, 16, '2.2250738585072009e-308'},
	F64Case{0x000fffffffffffff, 17, '2.22507385850720089e-308'},
	F64Case{0x000fffffffffffff, 20, '2.22507385850720088902e-308'},
	F64Case{0x000fffffffffffff, 25, '2.2250738585072008890245869e-308'},
	F64Case{0x8000000000000001, 0, '-5e-324'},
	F64Case{0x8000000000000001, 1, '-4.9e-324'},
	F64Case{0x8000000000000001, 2, '-4.94e-324'},
	F64Case{0x8000000000000001, 3, '-4.941e-324'},
	F64Case{0x8000000000000001, 5, '-4.94066e-324'},
	F64Case{0x8000000000000001, 10, '-4.9406564584e-324'},
	F64Case{0x8000000000000001, 16, '-4.9406564584124654e-324'},
	F64Case{0x8000000000000001, 17, '-4.94065645841246544e-324'},
	F64Case{0x8000000000000001, 20, '-4.94065645841246544177e-324'},
	F64Case{0x8000000000000001, 25, '-4.9406564584124654417656879e-324'},
	F64Case{0x800abcdef0123456, 0, '-1e-308'},
	F64Case{0x800abcdef0123456, 1, '-1.5e-308'},
	F64Case{0x800abcdef0123456, 2, '-1.49e-308'},
	F64Case{0x800abcdef0123456, 3, '-1.493e-308'},
	F64Case{0x800abcdef0123456, 5, '-1.49327e-308'},
	F64Case{0x800abcdef0123456, 10, '-1.4932716480e-308'},
	F64Case{0x800abcdef0123456, 16, '-1.4932716480206689e-308'},
	F64Case{0x800abcdef0123456, 17, '-1.49327164802066885e-308'},
	F64Case{0x800abcdef0123456, 20, '-1.49327164802066885332e-308'},
	F64Case{0x800abcdef0123456, 25, '-1.4932716480206688533181420e-308'},
]

fn test_f32_to_str_pad_subnormal() {
	for c in f32_cases {
		got := strconv.f32_to_str_pad(math.f32_from_bits(c.bits), c.n_digit)
		assert got == c.want, 'bits: 0x${c.bits:08x}, n_digit: ${c.n_digit}'
	}
}

fn test_f64_to_str_pad_subnormal() {
	for c in f64_cases {
		got := strconv.f64_to_str_pad(math.f64_from_bits(c.bits), c.n_digit)
		assert got == c.want, 'bits: 0x${c.bits:016x}, n_digit: ${c.n_digit}'
	}
}

fn test_v_sprintf_e_subnormal() {
	x := math.f64_from_bits(1)
	assert unsafe { strconv.v_sprintf('%.1e', x) } == '4.9e-324'
	assert unsafe { strconv.v_sprintf('%.3e', -x) } == '-4.941e-324'
	assert unsafe { strconv.v_sprintf('%.2e', math.f64_from_bits(0x000f_ffff_ffff_ffff)) } == '2.23e-308'
}

fn test_max_digits_keep_the_shortest_subnormal_digits() {
	// `f32_to_str` and `f64_to_str` print at most `n_digit` digits after the dot: when the
	// shortest round-trip digits fit, they are printed as they are.
	assert strconv.f32_to_str(math.f32_from_bits(1), 8) == '1e-45'
	assert strconv.f64_to_str(math.f64_from_bits(1), 17) == '5e-324'
	assert strconv.f64_to_str(math.f64_from_bits(5), 1) == '2.5e-323'
	assert strconv.f32_to_str(math.f32_from_bits(0x528), 2) == '1.85e-42'
	assert strconv.ftoa_32(math.f32_from_bits(0x7f_ffff)) == '1.1754942e-38'
	assert math.f32_from_bits(1).str() == '1e-45'
	assert math.f64_from_bits(1).str() == '5e-324'
	assert (-math.f64_from_bits(1)).str() == '-5e-324'
}

fn test_max_digits_round_the_exact_subnormal_value() {
	// when the shortest digits do not fit, the exact value is rounded, not the shortest digits
	assert strconv.f32_to_str(math.f32_from_bits(0x20), 0) == '4e-44' // 4.48e-44, shortest 4.5e-44
	assert strconv.f32_to_str(math.f32_from_bits(0x528), 1) == '1.8e-42' // shortest 1.85e-42
	assert strconv.f32_to_str(math.f32_from_bits(0x400), 2) == '1.43e-42' // shortest 1.435e-42
	assert strconv.f64_to_str(math.f64_from_bits(3), 0) == '1e-323' // 1.48e-323, shortest 1.5e-323
	assert strconv.f64_to_str(math.f64_from_bits(5), 0) == '2e-323' // 2.47e-323, shortest 2.5e-323
	assert strconv.f64_to_str(math.f64_from_bits(0xa9), 1) == '8.3e-322' // shortest 8.35e-322
	assert strconv.f64_to_str(-math.f64_from_bits(0x100), 2) == '-1.26e-321' // shortest 1.265e-321
	assert math.f32_from_bits(0x528).strsci(1) == '1.8e-42'
	assert math.f64_from_bits(0x176).strsci(1) == '1.8e-321' // shortest 1.85e-321
}

fn test_smallest_normal_is_unchanged() {
	smallest_normal_f32 := math.f32_from_bits(0x0080_0000)
	assert strconv.f32_to_str_pad(smallest_normal_f32, 0) == '1e-38'
	assert strconv.f32_to_str_pad(smallest_normal_f32, 1) == '1.2e-38'
	assert strconv.f32_to_str_pad(smallest_normal_f32, 7) == '1.1754944e-38'
	assert strconv.f32_to_str(smallest_normal_f32, 8) == '1.1754944e-38'
	smallest_normal_f64 := math.f64_from_bits(0x0010_0000_0000_0000)
	assert strconv.f64_to_str_pad(smallest_normal_f64, 0) == '2e-308'
	assert strconv.f64_to_str_pad(smallest_normal_f64, 1) == '2.2e-308'
	assert strconv.f64_to_str_pad(smallest_normal_f64, 16) == '2.2250738585072014e-308'
	assert strconv.f64_to_str(smallest_normal_f64, 17) == '2.2250738585072014e-308'
}
