module jwt

import crypto.hmac
import crypto.sha256
import encoding.base64

fn signed_security_token(header string, payload string) string {
	input := '${base64.url_encode(header.bytes())}.${base64.url_encode(payload.bytes())}'
	signature := hmac.new('secret'.bytes(), input.bytes(), sha256.sum, sha256.block_size)
	return '${input}.${base64.url_encode(signature)}'
}

fn test_security_rejects_invalid_claims_and_headers() {
	for payload in ['{"exp":"200"}', '{"exp":true}', '{"exp":1.5}', 'null', '[]', '{"iss":123}',
		'{"aud":["service"]}', '{"exp":null}', '{"nbf":null}', '{"aud":null}', '{"exp":1,"exp":200}',
		r'{"exp":1,"\u0065xp":200}', '{"sub":"user"} {}', '{"sub":"user"} trailing'] {
		token := signed_security_token('{"alg":"HS256"}', payload)
		if _ := verify_raw(token, 'secret'.bytes(), VerifyOptions{ algorithm: .hs256, current_time: 100 }) {
			assert false, 'accepted invalid claims: ${payload}'
		}
	}
	for header in ['{}', '{"alg":null}', '{"alg":123}', '{"alg":"none"}',
		'{"alg":"HS256","crit":["custom"],"custom":true}', '{"alg":"HS256","b64":false}',
		'{"alg":"HS256","b64":true}', '{"alg":"HS512","alg":"HS256"}',
		r'{"alg":"HS512","\u0061lg":"HS256"}', '{"alg":"HS256"} {}'] {
		token := signed_security_token(header, '{}')
		if _ := verify_raw(token, 'secret'.bytes(), VerifyOptions{ algorithm: .hs256 }) {
			assert false, 'accepted invalid header: ${header}'
		}
	}
}

fn test_security_rejects_noncanonical_base64url_segments() {
	token := signed_security_token('{"alg":"HS256"}', '{}')
	parts := token.split('.')
	for segment in 0 .. 3 {
		mut changed := parts.clone()
		changed[segment] += '='
		if _ := verify_raw(changed.join('.'), 'secret'.bytes(), VerifyOptions{ algorithm: .hs256 }) {
			assert false, 'accepted padded segment ${segment}'
		}
	}
	for malformed in ['', 'A', 'AA=', 'AA+', 'AA/', 'AA\n', 'AB'] {
		if _ := decode_segment(malformed) {
			assert false, 'accepted malformed segment ${malformed}'
		}
	}
}

fn test_security_leeway_does_not_overflow() {
	maximum := i64(9223372036854775807)
	minimum := i64(-9223372036854775807) - 1
	for entry in [StandardClaims{ exp: minimum }, StandardClaims{ nbf: maximum }] {
		token := sign_with_secret(entry, 'secret', SignOptions{})!
		now := if entry.exp != none { maximum } else { minimum }
		if _ := verify_raw(token, 'secret'.bytes(), VerifyOptions{
			algorithm:    .hs256
			current_time: now
			leeway:       maximum
		}) {
			assert false, 'overflow bypassed a timestamp bound'
		}
	}
	token := sign_with_secret(StandardClaims{ exp: maximum }, 'secret', SignOptions{})!
	assert verify_raw(token, 'secret'.bytes(), VerifyOptions{
		algorithm:    .hs256
		current_time: maximum
		leeway:       1
	})!.len > 0
	if _ := verify_raw(token, 'secret'.bytes(), VerifyOptions{ algorithm: .hs256, leeway: -1 }) {
		assert false, 'negative leeway accepted'
	}
}
