module tls

import crypto.pem
import crypto.rsa
import crypto.x509
import os
import time

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

// test_explicit_roots_orders_store_file_pem locks trust-source precedence:
// ca_store wins, then the ca_cert bundle file, then ca_cert_pem bytes,
// then empty (verify_peer_chain falls back to system roots on empty).
fn test_explicit_roots_orders_store_file_pem() {
	bundle := os.join_path(@VEXEROOT, 'vlib/crypto/x509/testdata/cert.pem')
	text := os.read_file(bundle) or { panic('read bundle: ${err}') }
	file_cert := parse_bundle_pem(text)
	assert file_cert.len == 1
	// File and PEM forms agree.
	assert explicit_roots(Config{ ca_cert: bundle }).len == 1
	assert explicit_roots(Config{ ca_cert_pem: text.bytes() }).len == 1
	// An explicit store wins over both.
	store_cert := file_cert[0]
	got := explicit_roots(Config{
		ca_store:    CertStore{ roots: [store_cert] }
		ca_cert:     bundle
		ca_cert_pem: text.bytes()
	})
	assert got.len == 1
	assert got[0].raw == store_cert.raw
	// Nothing configured: empty (system-root fallback happens downstream).
	assert explicit_roots(Config{}).len == 0
}

// test_ca_cert_pem_loopback proves the Config.ca_cert_pem path end to end:
// a server with a self-signed leaf verifies against the PEM bundle with
// no ca_store configured (this failed closed before ca_cert was wired).
fn test_ca_cert_pem_loopback() {
	key := rsa.generate_key(rsa.KeyGenOptions{ bits: 1024 }) or { panic('keygen: ${err}') }
	now := time.now()
	cert, cert_der := rsa.generate_self_signed(x509.CertGenOptions{
		common_name: 'localhost'
		dns_names:   ['localhost']
		not_before:  now
		not_after:   now.add_days(30)
	}, key) or { panic('cert gen: ${err}') }
	key_pem := key.to_pem_pkcs8() or { panic('key pem: ${err}') }
	cert_pem := pem.Block{
		block_type: 'CERTIFICATE'
		data:       cert_der
	}.encode(pem.EncodeConfig{}) or { panic('cert pem: ${err}') }
	mut l := listen(Config{ cert: cert_pem, key: key_pem }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	done := chan string{cap: 1}
	spawn fn (mut l Listener, done chan string) {
		mut conn := l.accept() or {
			done <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		done <- 'ok'
	}(mut l, done)
	mut c := dial('127.0.0.1', port, Config{
		server_name: 'localhost'
		ca_cert_pem: cert_pem.bytes()
	}) or { panic('dial: ${err}') }
	c.handshake() or { panic('handshake with ca_cert_pem: ${err}') }
	c.close() or {}
	assert <-done == 'ok'
	l.close() or {}
}
