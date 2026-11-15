module tls

import os

// test_winroots_parse_certutil_sample locks PEM scraping: a fake certutil
// header wrapped around the real RSA test CA must yield exactly that CA.
fn test_winroots_parse_certutil_sample() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	ca_text := os.read_file(os.join_path(td, 'rsa_ca_cert.pem')) or { panic('read ca') }
	sample := 'Root "Trusted Root Certification Authorities"\n================ Certificate 0 ================\nSerial Number: 00\n' +
		ca_text + '\n================ Certificate 1 ================\n'
	roots := parse_certutil_output(sample)
	assert roots.len == 1
	direct := parse_bundle_pem(ca_text)
	assert direct.len == 1
	assert roots[0].raw == direct[0].raw
}

// test_winroots_parse_empty_never_fails locks the never-fail-hard contract:
// garbage in yields an empty set, never an error or panic.
fn test_winroots_parse_empty_never_fails() {
	assert parse_certutil_output('').len == 0
	assert parse_certutil_output('no certificates here\n================\n').len == 0
}

// test_winroots_enumeration runs the live Windows store enumeration. It
// skips gracefully when the store is empty or unavailable; the embedded
// bundle fallback (covered by test_certstore_default_loads) still applies.
fn test_winroots_enumeration() {
	$if windows {
		roots := load_windows_roots()
		if roots.len == 0 {
			println('winroots: Windows store empty or unavailable, skipping (embedded fallback covers)')
			return
		}
		assert roots.len > 0
	} $else {
		assert true
	}
}

// test_winroots_fallback_loads ensures default loading still yields roots
// with no env overrides (embedded bundle guarantees non-empty).
fn test_winroots_fallback_loads() {
	old_file := os.getenv('SSL_CERT_FILE')
	old_dir := os.getenv('SSL_CERT_DIR')
	os.setenv('SSL_CERT_FILE', '', true)
	os.setenv('SSL_CERT_DIR', '', true)
	defer {
		os.setenv('SSL_CERT_FILE', old_file, true)
		os.setenv('SSL_CERT_DIR', old_dir, true)
	}
	store := load_system_roots() or { panic('default roots: ${err}') }
	assert store.roots.len > 0
}

// test_winroots_env_overrides_win locks the documented priority: an
// explicit SSL_CERT_FILE or SSL_CERT_DIR beats every built-in source. A
// regression that ignores the overrides still passes the fallback test.
fn test_winroots_env_overrides_win() {
	old_file := os.getenv('SSL_CERT_FILE')
	old_dir := os.getenv('SSL_CERT_DIR')
	defer {
		os.setenv('SSL_CERT_FILE', old_file, true)
		os.setenv('SSL_CERT_DIR', old_dir, true)
	}
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	ca_path := os.join_path(td, 'rsa_ca_cert.pem')
	os.setenv('SSL_CERT_FILE', ca_path, true)
	os.setenv('SSL_CERT_DIR', '', true)
	store := load_system_roots() or { panic('file override: ${err}') }
	assert store.roots.len == 1
	dir := os.join_path(os.temp_dir(), 'tls_winroots_test')
	os.mkdir(dir) or {}
	ca_text := os.read_file(ca_path) or { panic('read ca: ${err}') }
	ca_copy := os.join_path(dir, 'ca.pem')
	os.write_file(ca_copy, ca_text) or { panic('write ca: ${err}') }
	defer {
		os.rm(ca_copy) or {}
		os.rmdir(dir) or {}
	}
	os.setenv('SSL_CERT_FILE', '', true)
	os.setenv('SSL_CERT_DIR', dir, true)
	store2 := load_system_roots() or { panic('dir override: ${err}') }
	assert store2.roots.len == 1
}
