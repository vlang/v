module math

fn test_erf() {
	assert is_nan(erf(nan()))
	assert tolerance(erf(-1.0), -0.8427007888650501, 1e-8)
	assert tolerance(erf(0.0), 0.0, 1e-11)
	assert tolerance(erf(1e-15), 0.0000000000000011283791670955126615773132947717431253912942469337536,
		1e-11)
	assert tolerance(erf(0.1), 0.11246291601917208, 1e-11)
	assert tolerance(erf(0.3), 0.32862677677789676, 1e-7)
	assert tolerance(erf(0.5), 0.5204998778130465376827466538919645287364515757579637,
		1e-9)
	assert tolerance(erf(1.0), 0.8427007888650501, 1e-8)
	assert tolerance(erf(1.5), 0.966105146259005, 1e-9)
	assert tolerance(erf(6.0), 0.99999999999999997848026328750108688340664960081261537,
		1e-12)
	assert tolerance(erf(5.0), 0.99999999999846254020557196514981165651461662110988195,
		1e-12)
	assert tolerance(erf(4.0), 0.999999984582742, 1e-12)
	assert tolerance(erf(inf(1)), 1.0, 1e-12)
	assert tolerance(erf(inf(-1)), -1.0, 1e-12)
}

fn test_erfc() {
	assert tolerance(erfc(-1.0), 1.84270078886505, 1e-8)
	assert tolerance(erfc(0.0), 1.0, 1e-11)
	assert tolerance(erfc(0.1), 0.8875370839808279, 1e-11)
	assert tolerance(erfc(0.2), 0.7772974103342554, 1e-9)
}
