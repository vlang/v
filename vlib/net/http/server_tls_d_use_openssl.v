// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// TLS termination for net.http.Server is currently provided by the default
// mbedtls backend. Under `-d use_openssl`, the OpenSSL backend does not yet
// expose a server-side listener; this file replaces the implementation in
// `server_tls_notd_use_openssl.v` with a stub that reports a clear error.

fn (mut s Server) listen_and_serve_tls() {
	eprintln('net.http.Server: TLS termination is not supported on -d use_openssl yet; remove -d use_openssl or omit cert/cert_key to fall back to plain HTTP')
}
