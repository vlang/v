// HTTP Message Signatures example: signs an outbound request with
// Ed25519 (using the RFC 9421 §B.1.4 test key in PEM form) and then
// verifies the result with the matching public key.
//
// Run with:  v run examples/http_signature.v
import net.http
import net.http.signature

const ed25519_private_pem = '-----BEGIN PRIVATE KEY-----
MC4CAQAwBQYDK2VwBCIEIJ+DYvh6SEqVTm50DFtMDoQikTmiCqirVv9mWG9qfSnF
-----END PRIVATE KEY-----'

const ed25519_public_pem = '-----BEGIN PUBLIC KEY-----
MCowBQYDK2VwAyEAJrQLj5P/89iXES9+vFgrIy29clF9CC/oPPsw3c5D0bs=
-----END PUBLIC KEY-----'

fn main() {
	demo_sign_and_verify_request()!
	demo_two_signatures()!
}

// demo_sign_and_verify_request walks through the common path: the
// client signs a request before sending; the server verifies before
// processing.
fn demo_sign_and_verify_request() ! {
	priv := signature.Key.from_pem(ed25519_private_pem)!.with_keyid('test-key-ed25519')
	pub_key := signature.Key.from_pem(ed25519_public_pem)!

	mut req := http.Request{
		method: .post
		url:    'https://example.com/foo'
	}
	req.header.add_custom('Host', 'example.com')!
	req.header.add_custom('Date', 'Tue, 20 Apr 2021 02:07:55 GMT')!
	req.header.add_custom('Content-Type', 'application/json')!
	req.header.add_custom('Content-Length', '18')!

	signature.sign_request(mut req, priv,
		components: ['date', '@method', '@path', '@authority', 'content-type', 'content-length']
		created:    1618884473
		label:      'sig-b26'
	)!

	si := req.header.get_custom('Signature-Input') or { '' }
	sig := req.header.get_custom('Signature') or { '' }
	println('Signature-Input: ${si}')
	println('Signature:       ${sig}')

	signature.verify_request(req, pub_key)!
	println('  ✓ verified with the matching public key')
}

// demo_two_signatures shows a TLS-terminating proxy scenario: the
// client signs the original request, the proxy adds its own signature
// over the same message under a different label, and the backend
// verifies both independently.
fn demo_two_signatures() ! {
	client_key := signature.Key.hmac_sha256('client-shared-secret'.bytes()).with_keyid('client')
	proxy_key := signature.Key.hmac_sha256('proxy-shared-secret'.bytes()).with_keyid('proxy')

	mut req := http.Request{
		method: .get
		url:    'https://api.example.com/orders/42'
	}
	req.header.add_custom('Host', 'api.example.com')!
	req.header.add_custom('Date', 'Tue, 20 Apr 2021 02:07:55 GMT')!

	// `created` defaults to `time.now().unix()` when not set —
	// fine for a real client. Pinned here so the example output
	// is reproducible across runs.
	signature.sign_request(mut req, client_key,
		components: ['@method', '@target-uri', 'date']
		label:      'client-sig'
		created:    1618884473
	)!
	signature.sign_request(mut req, proxy_key,
		components: ['@method', '@authority', 'date']
		label:      'proxy-sig'
		created:    1618884480
	)!

	si := req.header.get_custom('Signature-Input') or { '' }
	println('\nMerged Signature-Input: ${si}')

	signature.verify_request(req, client_key, label: 'client-sig')!
	signature.verify_request(req, proxy_key, label: 'proxy-sig')!
	println('  ✓ both labelled signatures verified')
}
