module math

// Inputs to test trig_reduce, from huge_test.go of the Go standard library.
const trig_huge_ = [ldexp(1.0, 28), ldexp(1.0, 29), ldexp(1.0, 30), ldexp(1.0, 35), ldexp(1.0, 120),
	ldexp(1.0, 240), ldexp(1.0, 480), ldexp(1234567891234567.0, 180), ldexp(1234567891234567.0, 300),
	max_f64]

// Results for trig_huge_[i], calculated there with https://github.com/robpike/ivy
// using 4096 bits of working precision.
const cos_huge_ = [f64(-0.16556897949057876), -0.94517382606089662, 0.78670712294118812,
	-0.76466301249635305, -0.92587902285483787, 0.93601042593353793, -0.28282777640193788,
	-0.14616431394103619, -0.79456058210671406, -0.99998768942655994]
const sin_huge_ = [f64(-0.98619821183697566), 0.32656766301856334, -0.61732641504604217,
	-0.64443035102329113, 0.37782010936075202, -0.35197227524865778, 0.95917070894368716,
	0.98926032637023618, -0.60718488235646949, 0.00496195478918406]
const tan_huge_ = [f64(5.95641897939639421), -0.34551069233430392, -0.78469661331920043,
	0.84276385870875983, -0.40806638884180424, -0.37603456702698076, -3.39135965054779932,
	-6.76813854009065030, 0.76417695016604922, -0.00496201587444489]

// The last f64 below 1 << 29, which still goes through the three part
// reduction, and powers of ten, among them the ones of
// https://github.com/vlang/v/issues/30022. 1e40, 1e60 and 1e300 are given by
// their bits, so that they do not depend on how a C compiler rounds such a
// literal. The results come from a 2600 bit fixed point calculation.
const trig_issue_ = [f64_from_bits(0x41bfffffffffffff), 1e10, 1e12, 1e13, 1e17, 1e22,
	f64_from_bits(0x483d6329f1c35ca5), f64_from_bits(0x4c63e9e4e4c2f344),
	f64_from_bits(0x7e37e43c8800759c)]
const sin_issue_ = [f64(3.2656771935531291030e-01), -4.8750602508751069153e-01,
	-6.1123870237688949819e-01, -2.8888529481752512226e-01, -4.6453010483537269615e-01,
	-8.5220084976718880177e-01, 6.4678458842683437610e-01, -5.6078663590264580335e-01,
	-8.1788191211590859705e-01]
const cos_issue_ = [f64(-9.4517380659594539230e-01), 8.7311962267685600118e-01,
	7.9144630185289027005e-01, 9.5736371690083993528e-01, -8.8555732829763068505e-01,
	5.2321478539513894550e-01, -7.6267273202437915903e-01, 8.2796035472297426233e-01,
	-5.7538611195754904669e-01]
const tan_issue_ = [f64(-3.4551075905441180354e-01), -5.5834963781124184656e-01,
	-7.7230596813187614161e-01, -3.0175082856983471200e-01, 5.2456243090255001593e-01,
	-1.6287782256068988785e+00, -8.4804997119807822235e-01, -6.7731097594676302599e-01,
	1.4214488238747244124e+00]

// Arguments next to a multiple of pi/2, where the sine or the cosine is tiny
// and takes all its digits from the low bits of the reduced argument:
// 6381956970095103 * 2^797, the f64 that is closest to such a multiple, and
// the f64 nearest to k * pi/2 for k = 341782638, 683565277, 1099511627777
// and 1099511627778 (the first two are just above 1 << 29 and 1 << 30).
// The results come from the same calculation.
const trig_near_ = [f64_from_bits(0x7506ac5b262ca1ff), f64_from_bits(0x41c00000002a94ed),
	f64_from_bits(0x41d00000008f1cda), f64_from_bits(0x427921fb5444463a),
	f64_from_bits(0x427921fb54445f5c)]
const sin_near_ = [f64(1.0), -2.7059608751609932032e-09, 9.9999999999999754398e-01,
	9.9999999802360518325e-01, 5.8416759539072478388e-05]
const cos_near_ = [f64(-4.6871659242546276111e-19), -9.9999999999999999634e-01,
	7.0085977798596777901e-08, 6.2871214634258504752e-05, -9.9999999829374110102e-01]
const tan_near_ = [f64(-2.1334853857537038437e+18), 2.7059608751609932131e-09,
	1.4268189321316980792e+07, 1.5905530119640880996e+04, -5.8416759638746594371e-05]

// check_sincos_tan_cot compares sincos, tan and cot of x and of -x with the expected values.
fn check_sincos_tan_cot(x f64, sin_x f64, cos_x f64, tan_x f64) {
	s, c := sincos(x)
	assert close(s, sin_x), 'sincos(${x}) = ${s}, ${c}, want ${sin_x}, ${cos_x}'
	assert close(c, cos_x), 'sincos(${x}) = ${s}, ${c}, want ${sin_x}, ${cos_x}'
	ns, nc := sincos(-x)
	assert close(ns, -sin_x), 'sincos(${-x}) = ${ns}, ${nc}, want ${-sin_x}, ${cos_x}'
	assert close(nc, cos_x), 'sincos(${-x}) = ${ns}, ${nc}, want ${-sin_x}, ${cos_x}'
	assert close(tan(x), tan_x), 'tan(${x}) = ${tan(x)}, want ${tan_x}'
	assert close(tan(-x), -tan_x), 'tan(${-x}) = ${tan(-x)}, want ${-tan_x}'
	assert close(cot(x), 1.0 / tan_x), 'cot(${x}) = ${cot(x)}, want ${1.0 / tan_x}'
	assert close(cot(-x), -1.0 / tan_x), 'cot(${-x}) = ${cot(-x)}, want ${-1.0 / tan_x}'
}

// Check that trig values of huge angles return accurate results.
// This confirms that argument reduction works for very large values
// up to max_f64.
fn test_huge_sincos_tan_cot() {
	for i := 0; i < trig_huge_.len; i++ {
		check_sincos_tan_cot(trig_huge_[i], sin_huge_[i], cos_huge_[i], tan_huge_[i])
	}
}

fn test_issue_30022_sincos_tan_cot() {
	for i := 0; i < trig_issue_.len; i++ {
		check_sincos_tan_cot(trig_issue_[i], sin_issue_[i], cos_issue_[i], tan_issue_[i])
	}
}

fn test_near_multiples_of_half_pi_sincos_tan_cot() {
	for i := 0; i < trig_near_.len; i++ {
		check_sincos_tan_cot(trig_near_[i], sin_near_[i], cos_near_[i], tan_near_[i])
	}
}

fn test_cot_of_infinity_is_nan() {
	assert is_nan(cot(inf(1)))
	assert is_nan(cot(inf(-1)))
	assert is_nan(cot(nan()))
}

// check_trig_reduce rebuilds the sine and the cosine of x from the octant j
// and the rest z that trig_reduce returns for it.
fn check_trig_reduce(x f64, sin_x f64, cos_x f64) {
	j, z := trig_reduce(x)
	assert j in [0, 2, 4, 6], 'trig_reduce(${x}) = ${j}, ${z}'
	assert abs(z) <= pi_4, 'trig_reduce(${x}) = ${j}, ${z}'
	mut s := sin(z)
	mut c := cos(z)
	if j == 2 || j == 6 {
		s, c = c, -s
	}
	if j > 3 {
		s, c = -s, -c
	}
	assert close(s, sin_x), 'trig_reduce(${x}) = ${j}, ${z}, which gives the sine ${s}, want ${sin_x}'
	assert close(c, cos_x), 'trig_reduce(${x}) = ${j}, ${z}, which gives the cosine ${c}, want ${cos_x}'
}

fn test_trig_reduce() {
	for i := 0; i < trig_huge_.len; i++ {
		check_trig_reduce(trig_huge_[i], sin_huge_[i], cos_huge_[i])
	}
	for i := 0; i < trig_issue_.len; i++ {
		check_trig_reduce(trig_issue_[i], sin_issue_[i], cos_issue_[i])
	}
	for i := 0; i < trig_near_.len; i++ {
		check_trig_reduce(trig_near_[i], sin_near_[i], cos_near_[i])
	}
	// an argument below pi/4 is already reduced
	j, z := trig_reduce(0.5)
	assert j == 0
	assert z == 0.5
	// there is nothing to reduce in an infinite argument
	_, zinf := trig_reduce(inf(1))
	assert is_nan(zinf)
}
