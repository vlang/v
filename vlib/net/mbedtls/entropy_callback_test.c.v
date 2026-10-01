module mbedtls

fn test_entropy_callback_preserves_integer_result() {
	mut entropy := C.mbedtls_entropy_context{}
	C.mbedtls_entropy_init(&entropy)
	defer { C.mbedtls_entropy_free(&entropy) }
	mut output := [32]u8{}
	callback := C.mbedtls_entropy_func
	status := callback(voidptr(&entropy), &output[0], usize(output.len))
	assert status == 0
}

fn reject_entropy(_ voidptr, _ &u8, _ usize) int {
	return -1
}

fn test_seed_callback_preserves_entropy_failure() {
	mut generator := C.mbedtls_ctr_drbg_context{}
	C.mbedtls_ctr_drbg_init(&generator)
	defer { C.mbedtls_ctr_drbg_free(&generator) }
	status := C.mbedtls_ctr_drbg_seed(&generator, reject_entropy, unsafe { nil }, unsafe { nil }, 0)
	assert status == C.MBEDTLS_ERR_CTR_DRBG_ENTROPY_SOURCE_FAILED
}
