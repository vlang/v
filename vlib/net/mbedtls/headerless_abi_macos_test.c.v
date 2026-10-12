module mbedtls

fn C.v_mbedtls_context_size(type_id i32) usize
fn C.v_mbedtls_context_aligned(type_id i32, context voidptr) i32

fn test_headerless_context_storage_matches_native_library() {
	assert usize(sizeof(C.mbedtls_ctr_drbg_context)) == C.v_mbedtls_context_size(0)
	assert C.v_mbedtls_context_aligned(0, &C.mbedtls_ctr_drbg_context{}) == 1
	assert usize(sizeof(C.mbedtls_entropy_context)) == C.v_mbedtls_context_size(4)
	assert C.v_mbedtls_context_aligned(4, &C.mbedtls_entropy_context{}) == 1
	assert usize(sizeof(C.mbedtls_net_context)) == C.v_mbedtls_context_size(6)
	assert C.v_mbedtls_context_aligned(6, &C.mbedtls_net_context{}) == 1
	assert usize(sizeof(C.mbedtls_pk_context)) == C.v_mbedtls_context_size(7)
	assert C.v_mbedtls_context_aligned(7, &C.mbedtls_pk_context{}) == 1
	assert usize(sizeof(C.mbedtls_pk_rsassa_pss_options)) == C.v_mbedtls_context_size(8)
	assert C.v_mbedtls_context_aligned(8, &C.mbedtls_pk_rsassa_pss_options{}) == 1
	assert usize(sizeof(C.mbedtls_ssl_config)) == C.v_mbedtls_context_size(9)
	assert C.v_mbedtls_context_aligned(9, &C.mbedtls_ssl_config{}) == 1
	assert usize(sizeof(C.mbedtls_ssl_context)) == C.v_mbedtls_context_size(10)
	assert C.v_mbedtls_context_aligned(10, &C.mbedtls_ssl_context{}) == 1
	assert usize(sizeof(C.mbedtls_x509_crl)) == C.v_mbedtls_context_size(11)
	assert C.v_mbedtls_context_aligned(11, &C.mbedtls_x509_crl{}) == 1
	assert usize(sizeof(C.mbedtls_x509_crt)) == C.v_mbedtls_context_size(12)
	assert C.v_mbedtls_context_aligned(12, &C.mbedtls_x509_crt{}) == 1
}
