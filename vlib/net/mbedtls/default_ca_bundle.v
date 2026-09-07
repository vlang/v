module mbedtls

import os

// default_ca_bundle_pem is a maintained snapshot of Mozilla's trusted root
// CA certificates (curl's ca-bundle distribution,
// https://curl.se/docs/caextract.html), vendored into
// thirdparty/cacert/cacert.pem and embedded into the binary at compile
// time — matching how thirdparty/mbedtls itself is vendored.
//
// Used as the trust anchor for OUTBOUND (client) TLS connections whenever
// a caller does not supply their own CA bundle/file: see SSLConn.init's
// client-only cert-loading block (ssl_connection.c.v) and
// verify_certificate_chain (x509_standalone.c.v, net.quic's HTTP/3 client).
// Deliberately NOT wired into SSLListener.init (also ssl_connection.c.v):
// that path's `verify` means "the CA that authenticates an mTLS CLIENT
// certificate", a fundamentally different, server-specific trust decision
// this public root store must never silently satisfy.
pub const default_ca_bundle_pem = $embed_file('../../../thirdparty/cacert/cacert.pem').to_string()

// linux_system_ca_bundle_paths lists the well-known locations a Linux
// distro's own CA bundle can live at, in the same priority order Go's
// x509.SystemCertPool() uses (crypto/x509/root_linux.go) -- deliberately
// reusing an already-battle-tested list rather than inventing a new one.
// Every one of these is a single flat, PEM-concatenated file assembled by
// that distro's own `ca-certificates`-equivalent package (apt/dnf/pacman/
// apk), kept current by the OS's own update mechanism -- unlike this
// module's vendored snapshot, which only changes when a human re-vendors
// it.
const linux_system_ca_bundle_paths = [
	'/etc/ssl/certs/ca-certificates.crt', // Debian/Ubuntu/Gentoo/Arch
	'/etc/pki/tls/certs/ca-bundle.crt', // Fedora/RHEL 6
	'/etc/ssl/ca-bundle.pem', // OpenSUSE
	'/etc/pki/tls/cacert.pem', // OpenELEC
	'/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem', // CentOS/RHEL 7
	'/etc/ssl/cert.pem', // Alpine Linux
]

// system_or_default_ca_bundle_pem returns the first Linux system CA bundle
// found at one of linux_system_ca_bundle_paths, read fresh from disk (so
// it stays current with whatever the OS's own package manager has
// installed, unlike the vendored default). Falls back to
// default_ca_bundle_pem on every other platform, and on a Linux system
// where none of those paths exist or none of them read back as a
// non-empty file (a minimal/embedded/container image with no
// `ca-certificates`-equivalent package installed).
//
// No equivalent exists for Windows or macOS: neither ships a single flat
// PEM file as its system trust store (Windows' is the CryptoAPI
// certificate store, macOS' is Keychain -- both binary/database-backed,
// reachable only through their own native APIs, not a file path). Adding
// real OS-native trust-store support for either platform needs new C
// bindings against those APIs, not a path list -- tracked as a distinct,
// larger follow-up (vlang/v#28405) rather than attempted here.
pub fn system_or_default_ca_bundle_pem() string {
	$if linux {
		for path in linux_system_ca_bundle_paths {
			content := os.read_file(path) or { continue }
			if content.len > 0 {
				return content
			}
		}
	}
	return default_ca_bundle_pem
}
