import os
import strings

// Exercise the ordinary compiler's linker, including the default skip_fastc
// build. Calling the FastC signer directly would miss the regression in #31486.
fn test_tcc_macos_driver_preserves_zero_globals_and_psa_keys() {
	$if !(macos && arm64) {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_tcc_macos_psa_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.c.v')
	executable := os.join_path(dir, 'psa_runtime')
	os.write_file(os.join_path(dir, 'bss_psa.h'), tcc_macos_psa_fixture())!
	os.write_file(source, 'module main

import net.mbedtls as _

#include "@DIR/bss_psa.h"

fn C.v_test_zero_globals() int
fn C.v_test_psa_keys() int

fn main() {
	assert C.v_test_zero_globals() == 0
	assert C.v_test_psa_keys() == 0
	println("zeroed globals and PSA keys")
}
')!
	// A fresh cache and disabled compiler fallback keep the test on the actual
	// TinyCC link path. Test the default signer regardless of the caller's env.
	build := os.exec(['env', '-u', 'VFLAGS', '-u', 'VOSARGS', '-u', 'V3_TCC_APPLE_CODESIGN',
		'V_MACOS_V3_NO_FALLBACK=1', 'VCACHE=${os.join_path(dir, 'cache')}', @VEXE, '-cc', 'tcc',
		'-nocache', '-no-retry-compilation', '-gc', 'none', '-o', executable, source])
	assert build.exit_code == 0, build.output
	verify := os.exec(['codesign', '--verify', '--strict', executable])
	assert verify.exit_code == 0, verify.output
	run := os.exec([executable])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'zeroed globals and PSA keys', run.output
}

fn tcc_macos_psa_fixture() string {
	mut source := strings.new_builder(400000)
	source.write_string('#include <psa/crypto.h>
char v_test_data[100] = {1};
char *v_test_data_ptr = v_test_data;
static unsigned char v_test_zero_bytes[32768];
')
	// The large symbol table keeps __LINKEDIT long enough for macOS 27 to map
	// TinyCC's erroneous chained-fixup pages over the zero-fill __bss pages.
	for i in 0 .. 6000 {
		source.writeln('int padding_function_with_a_long_name_${i}(void) { return ${i}; }')
	}
	source.write_string('int v_test_zero_globals(void) {
	int bad = 0;
	for (unsigned long i = 0; i < sizeof(v_test_zero_bytes); i++) {
		bad += v_test_zero_bytes[i] != 0;
	}
	return bad != 0 || v_test_data_ptr != v_test_data || v_test_data_ptr[0] != 1;
}

int v_test_psa_keys(void) {
	psa_status_t status = psa_crypto_init();
	if (status != PSA_SUCCESS) return status;
	psa_key_attributes_t attributes = PSA_KEY_ATTRIBUTES_INIT;
	psa_set_key_usage_flags(&attributes, PSA_KEY_USAGE_DERIVE);
	psa_set_key_algorithm(&attributes, PSA_ALG_ECDH);
	psa_set_key_type(&attributes, PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_MONTGOMERY));
	psa_set_key_bits(&attributes, 255);
	/* TLS 1.3 uses this volatile-key path before sending its ClientHello.
	 * Exercise allocation, export and slot reuse without a network server. */
	for (int i = 0; i < 32; i++) {
		mbedtls_svc_key_id_t key = MBEDTLS_SVC_KEY_ID_INIT;
		status = psa_generate_key(&attributes, &key);
		if (status != PSA_SUCCESS) break;
		unsigned char public_key[32];
		size_t public_key_length = 0;
		status = psa_export_public_key(key, public_key, sizeof(public_key), &public_key_length);
		psa_status_t destroy_status = psa_destroy_key(key);
		if (status == PSA_SUCCESS && public_key_length != sizeof(public_key)) {
			status = PSA_ERROR_CORRUPTION_DETECTED;
		}
		if (status == PSA_SUCCESS) status = destroy_status;
		if (status != PSA_SUCCESS) break;
	}
	psa_reset_key_attributes(&attributes);
	return status;
}
')
	return source.str()
}
