module x509

import os

// pki_test_helpers holds helpers shared by the pki_* test files. They
// live in a non-test file so every pki test stays runnable both
// standalone and in full-directory runs (sibling _test.v files are not
// compiled for single-file runs).

// pki_openssl_bin locates an OpenSSL CLI: PATH first, then the common
// Windows install path. Returns '' when unavailable; callers skip
// gracefully.
fn pki_openssl_bin() string {
	res := os.exec(['openssl', 'version'])
	if res.exit_code == 0 {
		return 'openssl'
	}
	win := 'C:\\Program Files\\OpenSSL-Win64\\bin\\openssl.exe'
	res2 := os.exec([win, 'version'])
	if res2.exit_code == 0 {
		return win
	}
	return ''
}
