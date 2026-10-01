// kvd: the key-value service of kv.proto served over native gRPC.
//
//   v run examples/grpc/server.v
//   v run examples/grpc/client.v
//
// The certificate in cert/ is self-signed, so it only serves to a client that
// pins it explicitly (see client.v). See README.md for how to generate your own.
module main

import net.grpc
import os
import kv

const listen_addr = '127.0.0.1:50051'

// main serves KV over HTTP/2 with TLS until the process is killed.
fn main() {
	// @FILE is this source file's path at compile time, so the certificate is
	// found no matter where the built executable ends up.
	cert := os.join_path(os.dir(@FILE), 'cert', 'server.crt')
	cert_key := os.join_path(os.dir(@FILE), 'cert', 'server.key')
	for path in [cert, cert_key] {
		if !os.exists(path) {
			eprintln('missing ${path}')
			eprintln('See examples/grpc/README.md for how to create the certificate.')
			exit(1)
		}
	}

	mut server := grpc.GrpcServer{
		addr:     listen_addr
		// Both set means TLS: net.http then advertises `h2` over ALPN, which is
		// the only way gRPC works in V. Leave them empty for cleartext h2c.
		// These are file paths; add in_memory_verification: true to hand over
		// PEM strings instead.
		cert:     cert
		cert_key: cert_key
	}
	server.mount(kv.new_service())
	println('kvd listening on https://${listen_addr} (gRPC over TLS/h2)')
	println('try: v run examples/grpc/client.v')
	server.listen_and_serve() or { eprintln('kvd stopped: ${err}') }
}
