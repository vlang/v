module ecdsa

fn C.v_mbedtls_context_size(type_id i32) usize
fn C.v_mbedtls_context_aligned(type_id i32, context voidptr) i32

fn test_headerless_context_storage_matches_native_library() {
	$if !use_openssl ? {
		assert usize(sizeof(C.mbedtls_ctr_drbg_context)) == C.v_mbedtls_context_size(0)
		assert C.v_mbedtls_context_aligned(0, &C.mbedtls_ctr_drbg_context{}) == 1
		assert usize(sizeof(C.mbedtls_ecdsa_context)) == C.v_mbedtls_context_size(1)
		assert C.v_mbedtls_context_aligned(1, &C.mbedtls_ecdsa_context{}) == 1
		assert usize(sizeof(C.mbedtls_ecp_group)) == C.v_mbedtls_context_size(2)
		assert C.v_mbedtls_context_aligned(2, &C.mbedtls_ecp_group{}) == 1
		assert usize(sizeof(C.mbedtls_ecp_point)) == C.v_mbedtls_context_size(3)
		assert C.v_mbedtls_context_aligned(3, &C.mbedtls_ecp_point{}) == 1
		assert usize(sizeof(C.mbedtls_entropy_context)) == C.v_mbedtls_context_size(4)
		assert C.v_mbedtls_context_aligned(4, &C.mbedtls_entropy_context{}) == 1
		assert usize(sizeof(C.mbedtls_mpi)) == C.v_mbedtls_context_size(5)
		assert C.v_mbedtls_context_aligned(5, &C.mbedtls_mpi{}) == 1
	}
}
