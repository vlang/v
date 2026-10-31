module testing

fn test_modern_openssl_version_output() {
	for output in [
		'OpenSSL 3.5.0 1 Jan 2026',
		'OpenSSL 3.5.0-beta1 1 Jan 2026',
		'OpenSSL 3.6.0 10 Feb 2026',
		'OpenSSL 4.0.0 1 Jan 2027',
		'  OpenSSL\t3.5.0\t1 Jan 2026\n',
	] {
		assert has_modern_openssl_version(output), output
	}
	for output in [
		'OpenSSL 3.0.2 15 Mar 2022',
		'OpenSSL 1.1.1k 25 Mar 2021',
		'OpenSSL 3.4.9 1 Jan 2026',
		'LibreSSL 3.5.0',
		'OpenSSL invalid',
		'OpenSSL',
		'',
	] {
		assert !has_modern_openssl_version(output), output
	}
}
