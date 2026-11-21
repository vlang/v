// oidc_private_key_jwt shows RSA JSON Web Tokens with an OpenID provider, such
// as Keycloak:
//
// - the client authenticates with a JWT it signs with its RSA key, the
//   `private_key_jwt` method of OpenID Connect (RFC 7523);
// - it then verifies the access token it receives with the key set the
//   provider publishes.
//
// Create a key for the client signature algorithm (RS256 or PS256), and print
// the JWKS to register with the client:
//   v run examples/oidc_private_key_jwt.v keygen client.pem PS256
// Get and verify a token, with the client signature algorithm and the token
// signature algorithm configured in the provider:
//   v run examples/oidc_private_key_jwt.v token \
//     http://localhost:8080/realms/demo my-client client.pem PS256 RS256
import crypto.rand
import crypto.rsa
import encoding.hex
import json2
import net.http
import os
import time
import x.crypto.jwt

struct Discovery {
	issuer         string
	token_endpoint string
	jwks_uri       string
}

struct TokenResponse {
	access_token string
	expires_in   int
}

struct ClientAssertion {
	iss string
	sub string
	aud string
	jti string
	iat i64
	exp i64
}

struct AccessClaims {
	iss                string
	sub                string
	exp                i64
	azp                ?string
	preferred_username ?string
}

fn main() {
	args := os.args[1..]
	if args.len == 3 && args[0] == 'keygen' {
		keygen(args[1], jwt.parse_algorithm(args[2])!)!
		return
	}
	if args.len == 6 && args[0] == 'token' {
		token(args[1], args[2], args[3], jwt.parse_algorithm(args[4])!, jwt.parse_algorithm(args[5])!)!
		return
	}
	eprintln('usage: oidc_private_key_jwt keygen <key.pem> <client_alg>')
	eprintln('       oidc_private_key_jwt token <issuer> <client_id> <key.pem> <client_alg> <token_alg>')
	exit(1)
}

// keygen writes a new RSA key, readable only by its owner, and prints its
// public JWK for `alg`. It never replaces an existing file.
fn keygen(path string, alg jwt.Algorithm) ! {
	key := rsa.generate_key(2048)!
	// Write the key in a private directory, then copy it to `path`, which
	// `os.cp` creates exclusively, with the mode of the source.
	dir := os.join_path(os.temp_dir(), 'oidc-key-${hex.encode(rand.bytes(8)!)}')
	os.mkdir(dir, mode: 0o700)!
	defer {
		os.rmdir_all(dir) or {}
	}
	tmp := os.join_path(dir, 'key.pem')
	mut f := os.open_file(tmp, 'w', 0o600)!
	f.write_string(key.to_pem())!
	f.close()
	os.cp(tmp, path, fail_if_exists: true) or { return error('cannot create ${path}: ${err.msg()}') }
	jwk := jwt.Jwk.from_rsa_public_key(key.public_key(), 'client-key', alg)
	println(jwt.Jwks{ keys: [jwk] }.encode())
}

fn token(issuer string, client_id string, key_path string, client_alg jwt.Algorithm, token_alg jwt.Algorithm) ! {
	key := rsa.PrivateKey.from_pem(os.read_file(key_path)!)!
	discovery := json2.decode[Discovery](get('${issuer}/.well-known/openid-configuration')!)!
	// The provider must announce the issuer we asked for (OpenID Connect
	// Discovery 1.0, section 4.3).
	if discovery.issuer != issuer {
		return error('the provider announces the issuer ${discovery.issuer}, not ${issuer}')
	}

	// The client assertion is a short-lived JWT, signed with the client key, for
	// the token endpoint.
	now := time.now().unix()
	assertion := jwt.sign_with_rsa_key(ClientAssertion{
		iss: client_id
		sub: client_id
		aud: discovery.token_endpoint
		jti: hex.encode(rand.bytes(16)!)
		iat: now
		exp: now + 60
	}, key, algorithm: client_alg, kid: 'client-key')!

	response := http.fetch(
		method:         .post
		url:            discovery.token_endpoint
		header:         http.new_header(
			key:   .content_type
			value: 'application/x-www-form-urlencoded'
		)
		data:           http.url_encode_form_data({
			'grant_type':            'client_credentials'
			'client_id':             client_id
			'client_assertion_type': 'urn:ietf:params:oauth:client-assertion-type:jwt-bearer'
			'client_assertion':      assertion
		})
		validate:       true
		allow_redirect: false
	)!
	if response.status_code != 200 {
		return error('token endpoint answered ${response.status_code}: ${response.body}')
	}
	access_token := json2.decode[TokenResponse](response.body)!.access_token

	// The provider signs its tokens with one of the keys of its JWKS; the
	// algorithm comes from our configuration, never from the token.
	jwks := jwt.Jwks.parse(get(discovery.jwks_uri)!)!
	verified := jwt.verify_with_jwks[AccessClaims](access_token, jwks,
		algorithm:       token_alg
		expected_issuer: issuer
	)!
	println('token signed with ${verified.header.alg}, key ${verified.header.kid or { '-' }}')
	println('subject: ${verified.claims.sub}, client: ${verified.claims.azp or { '-' }}')
	println('expires in ${verified.claims.exp - now} s')
}

// get returns the body of a successful GET request, with certificate validation
// for HTTPS URLs, and without following redirects.
fn get(url string) !string {
	response := http.fetch(url: url, validate: true, allow_redirect: false)!
	if response.status_code != 200 {
		return error('${url} answered ${response.status_code}')
	}
	return response.body
}
