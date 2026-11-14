module tls

import crypto.sha256
import x.crypto.curve25519

fn test_handshake_integration() {
	// Client
	mut client := new_client_handshake('example.com', []string{}, .off) or { panic(err) }
	ch_msg := client.build_client_hello() or { panic(err) }

	// Server
	mut server := new_server_handshake(.off) or { panic(err) }
	flight := server.process_client_hello(ch_msg) or { panic(err) }

	// Split flight into individual handshake messages
	mut off := 0
	mut msgs := [][]u8{}
	for off < flight.len {
		parsed, n := parse_handshake_message(flight[off..]) or { panic(err) }
		msgs << flight[off..off + n]
		off += n
	}
	assert msgs.len == 5

	// Client processes ServerHello (first message)
	client.process_server_hello(msgs[0]) or { panic(err) }

	// Client processes EE, Cert, CertVerify, Finished (append transcript)
	for i in 1 .. msgs.len {
		parsed, _ := parse_handshake_message(msgs[i]) or { panic(err) }
		match parsed.typ {
			.finished {
				client.transcript << msgs[i]
			}
			else {
				client.transcript << msgs[i]
			}
		}
	}

	// Verify client can derive app keys
	th := sha256.sum256(client.transcript)
	as_ := derive_application_secrets(client.handshake_keys.handshake_secret, th) or { panic(err) }
	assert as_.client_secret.len == 32

	// Server verifies client Finished: build client finished
	cf_vd := finished_verify_data(client.handshake_keys.client_secret, sha256.sum256(client.transcript)) or { panic(err) }
	client_fin_msg := encode_handshake_message(.finished, encode_finished(Finished{ verify_data: cf_vd })) or { panic(err) }
	server.verify_client_finished(client_fin_msg) or { panic(err) }
	assert server.app_keys.client_secret.len == 32
	_ = curve25519.PrivateKey.new
}
