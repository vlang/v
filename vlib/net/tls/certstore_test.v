module tls

import crypto.pem
import crypto.x509
import os

// with_env sets SSL_CERT_FILE/DIR for the duration of a test, restoring the
// previous values afterwards (tests in one file share a process).
fn with_env(file string, dir string) (string, string) {
	old_file := os.getenv('SSL_CERT_FILE')
	old_dir := os.getenv('SSL_CERT_DIR')
	os.setenv('SSL_CERT_FILE', file, true)
	os.setenv('SSL_CERT_DIR', dir, true)
	return old_file, old_dir
}

fn restore_env(old_file string, old_dir string) {
	os.setenv('SSL_CERT_FILE', old_file, true)
	os.setenv('SSL_CERT_DIR', old_dir, true)
}

// test_certstore_embedded_bundle locks the vendored Mozilla bundle: pointing
// SSL_CERT_FILE at it must yield exactly the audited root set.
fn test_certstore_embedded_bundle() {
	bundle := os.join_path(@VEXEROOT, 'vlib/net/tls/cacert.pem')
	old_file, old_dir := with_env(bundle, '')
	defer {
		restore_env(old_file, old_dir)
	}
	store := load_system_roots() or { panic('load bundle: ${err}') }
	assert store.roots.len == 118
}

// test_certstore_ssl_cert_dir locks directory scanning: every CERTIFICATE
// block in testdata must land in the store, including the RSA test CA.
fn test_certstore_ssl_cert_dir() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	old_file, old_dir := with_env('', td)
	defer {
		restore_env(old_file, old_dir)
	}
	store := load_system_roots() or { panic('load dir: ${err}') }
	ca_text := os.read_file(os.join_path(td, 'rsa_ca_cert.pem')) or { panic('read ca') }
	ca_block, _ := pem.decode(ca_text) or { panic('decode ca') }
	ca_cert := x509.parse_der(ca_block.data) or { panic(err) }
	assert store.contains(ca_cert)
}

// test_certstore_default_loads locks the portable fallback: with no env
// overrides, roots load on every OS (embedded bundle guarantees non-empty).
fn test_certstore_default_loads() {
	old_file, old_dir := with_env('', '')
	defer {
		restore_env(old_file, old_dir)
	}
	store := load_system_roots() or { panic('default roots: ${err}') }
	assert store.roots.len > 0
}
