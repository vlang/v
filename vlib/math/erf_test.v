import math

fn almost_eq(a f64, b f64, acc f64) bool {
	if math.is_inf(a, 1) || math.is_inf(a, -1) || math.is_inf(b, 1) || math.is_inf(b, -1) {
		return a == b
	}

	if math.is_nan(a) || math.is_nan(b) {
		return false
	}

	return math.abs(a - b) < acc
}

fn test_erf() {
	assert math.is_nan(math.erf(math.nan()))
	assert almost_eq(math.erf(-1.0), -0.8427007888650501, 1e-11)
	assert math.erf(0.0) == 0.0
	assert math.erf(1e-15) == 0.0000000000000011283791670955126615773132947717431253912942469337536
	assert math.erf(0.1) == 0.11246291601917208
	assert math.erf(0.3) == 0.32862677677789676
	assert almost_eq(math.erf(0.5), 0.5204998778130465376827466538919645287364515757579637,
		1e-9)
	assert math.erf(1.0) == 0.8427007888650501
	assert math.erf(1.5) == 0.966105146259005
	assert math.erf(6.0) == 0.99999999999999997848026328750108688340664960081261537
	assert math.erf(5.0) == 0.99999999999846254020557196514981165651461662110988195
	assert math.erf(4.0) == 0.999999984582742
	assert math.erf(math.inf(1)) == 1.0
	assert math.erf(math.inf(-1)) == -1.0
}

fn test_erfc() {
	assert almost_eq(math.erfc(-1.0), 1.84270078886505, 1e-11)
	assert math.erfc(0.0) == 1.0
	assert math.erfc(0.1) == 0.8875370839808279
	assert math.erfc(0.2) == 0.7772974103342554
}

fn test_erfc_inv() {
	assert math.erfc_inv(0.0) == math.inf(1)
	assert math.erfc_inv(1e-100) == 15.060286697120752
	assert math.erfc_inv(1.0) == 0.0
	assert math.erfc_inv(0.5) == 0.47660913088024937
}
