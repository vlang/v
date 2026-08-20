// Conformance vectors for the SCRAM exchange.
//
// The first two are the normative examples of RFC 5802 §5 (SCRAM-SHA-1) and
// RFC 7677 §3 (SCRAM-SHA-256), reproduced verbatim from the RFC text. The
// others cover what the RFCs leave without an example: SHA-512, a user name
// needing `saslname` escaping, an authorization identity, channel binding,
// and a non-ASCII password.
//
// Every vector was produced by an independent implementation written straight
// from RFC 5802 §3, and every one of them — the two normative ones included —
// was then replayed through `github.com/xdg-go/scram` v1.2.0, the library the
// MongoDB Go driver authenticates with, which agrees on all four messages.
module scram

import crypto.pbkdf2
import crypto.sha256
import crypto.sha512
import encoding.base64
import encoding.hex

// server_nonce_of returns the part of a vector's combined nonce that the
// server contributed, so a `Server` can be pinned to reproduce the vector.
fn server_nonce_of(v Vector) string {
	nonce := v.server_first.all_after('r=').all_before(',')
	return nonce[v.client_nonce.len..]
}

// client_for builds a Client pinned to a vector's nonce and channel binding.
fn client_for(v Vector) !&Client {
	mut binding := ChannelBinding{}
	if v.cbind_name != '' {
		binding = ChannelBinding{
			mode: .required
			name: v.cbind_name
			data: v.cbind_data
		}
	}
	return new_client(
		username:        v.username
		password:        v.password
		mechanism:       v.mechanism
		authzid:         v.authzid
		channel_binding: binding
		nonce:           v.client_nonce
	)
}

// credentials_for rebuilds the server side record of a vector's user.
fn credentials_for(v Vector) !Credentials {
	salt := base64.decode(v.server_first.all_after(',s=').all_before(',i='))
	return derive_credentials(v.mechanism, v.password, salt, v.iterations)!
}

fn test_client_reproduces_the_reference_messages() {
	for v in vectors {
		mut client := client_for(v)!
		assert client.first()! == v.client_first, v.name
		assert client.final(v.server_first)! == v.client_final, v.name
		client.verify(v.server_final) or { assert false, '${v.name}: ${err}' }
		assert client.done(), v.name
	}
}

fn test_server_reproduces_the_reference_messages() {
	for v in vectors {
		creds := credentials_for(v)!
		mut binding := ChannelBinding{}
		if v.cbind_name != '' {
			binding = ChannelBinding{
				mode: .required
				name: v.cbind_name
				data: v.cbind_data
			}
		}
		mut server := new_server(
			mechanism:       v.mechanism
			channel_binding: binding
			nonce:           server_nonce_of(v)
			lookup:          fn [creds] (username string) !Credentials {
				return creds
			}
		)!
		assert server.first(v.client_first)! == v.server_first, v.name
		assert server.final(v.client_final)! == v.server_final, v.name
		assert server.done(), v.name
		assert server.username() == v.username, v.name
		assert server.authzid() == v.authzid, v.name
	}
}

fn test_derive_credentials_matches_the_reference_keys() {
	for v in vectors {
		creds := credentials_for(v)!
		assert creds.stored_key.hex() == v.stored_hex, v.name
		assert creds.server_key.hex() == v.server_hex, v.name
		assert creds.iterations == v.iterations, v.name
		assert creds.mechanism == v.mechanism, v.name
	}
}

fn test_hi_matches_the_reference_salted_passwords() {
	for v in vectors {
		salt := base64.decode(v.server_first.all_after(',s=').all_before(',i='))
		salted := v.mechanism.hi(v.password.bytes(), salt, v.iterations)
		assert salted.hex() == v.salted_hex, v.name
		assert salted.len == v.mechanism.size(), v.name
	}
}

// Hi() is PBKDF2 with a single output block, so it must agree with the
// unrelated implementation already in vlib.
fn test_hi_agrees_with_crypto_pbkdf2() {
	password := 'pencil'.bytes()
	salt := hex.decode('4142434445464748')!
	for iterations in [1, 2, 1000, 4096] {
		sha256_expected := pbkdf2.key(password, salt, iterations, sha256.size, sha256.new())!
		assert Mechanism.sha256.hi(password, salt, iterations) == sha256_expected
		sha512_expected := pbkdf2.key(password, salt, iterations, sha512.size, sha512.new())!
		assert Mechanism.sha512.hi(password, salt, iterations) == sha512_expected
	}
}

struct Vector {
	name         string
	mechanism    Mechanism
	username     string
	password     string
	authzid      string
	cbind_name   string
	cbind_data   []u8
	client_nonce string
	iterations   int
	client_first string
	server_first string
	client_final string
	server_final string
	salted_hex   string
	stored_hex   string
	server_hex   string
}

const vectors = [
	Vector{
		name:         'rfc5802_sha1'
		mechanism:    .sha1
		username:     'user'
		password:     'pencil'
		authzid:      ''
		cbind_name:   ''
		cbind_data:   []u8{}
		client_nonce: 'fyko+d2lbbFgONRv9qkxdawL'
		iterations:   4096
		client_first: 'n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL'
		server_first: 'r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096'
		client_final: 'c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts='
		server_final: 'v=rmF9pqV8S7suAoZWja4dJRkFsKQ='
		salted_hex:   '1d96ee3a529b5a5f9e47c01f229a2cb8a6e15f7d'
		stored_hex:   'e9d94660c39d65c38fbad91c358f14da0eef2bd6'
		server_hex:   '0fe09258b3ac852ba502cc62ba903eaacdbf7d31'
	},
	Vector{
		name:         'rfc7677_sha256'
		mechanism:    .sha256
		username:     'user'
		password:     'pencil'
		authzid:      ''
		cbind_name:   ''
		cbind_data:   []u8{}
		client_nonce: 'rOprNGfwEbeRWgbNEkqO'
		iterations:   4096
		client_first: 'n,,n=user,r=rOprNGfwEbeRWgbNEkqO'
		server_first: 'r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF\$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096'
		client_final: 'c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF\$k0,p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ='
		server_final: 'v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4='
		salted_hex:   'c4a49510323ab4f952cac1fa99441939e78ea74d6be81ddf7096e87513dc615d'
		stored_hex:   '586e5df283e6dceb5c3e791d8b8528ec191e664045ce971792e2e6b5bb13e2a6'
		server_hex:   'c1f3cbc1c13a9d35a14c0990eed97629ea225863e566a4314ab99f3f00e5d9d5'
	},
	Vector{
		name:         'sha512'
		mechanism:    .sha512
		username:     'user'
		password:     'pencil'
		authzid:      ''
		cbind_name:   ''
		cbind_data:   []u8{}
		client_nonce: 'rOprNGfwEbeRWgbNEkqO'
		iterations:   4096
		client_first: 'n,,n=user,r=rOprNGfwEbeRWgbNEkqO'
		server_first: 'r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF\$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096'
		client_final: 'c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF\$k0,p=gMGXRcevScNtxZ6/8lQYpGtnsNAc3mGcmNomv+xnoOMw+3R2xNJdMNnzMlTN8PPC6wdp6dybEmDYXYTxwnYPJQ=='
		server_final: 'v=ZQnYEgWQMFmmsM8aQMF0nDDCy/AgCzkwk8CmMZYcMg0vSVlKDanekLtifDSeVGT4+5ZxXnJq199RVG2rR7N7Zw=='
		salted_hex:   'f16efe1be67f1d09502ebd5ed9262fddffba5a377ab4f0b687e5ed5ba0f50686b8a4ae166476da8ab3b951d2fa9238b63998f45461bc33a464814949cec9631d'
		stored_hex:   'e8002e6f7d3ae446119b216933644dc2a2be7869eb918b8459b5e7d7d2ec12606aceef106825cd735170a675fd3611f684affad1dce3f43a0ee43bd590e1dbbe'
		server_hex:   '8d91db6230b5687874fe129bc7206e1858c3ae08e02934f57ac03b6b05a229c459d28ff46f5c9611e6c179256490215ec1ff759cb0df285db89af0f99e613aac'
	},
	Vector{
		name:         'escaped_username'
		mechanism:    .sha256
		username:     'a,b=c'
		password:     'pencil'
		authzid:      ''
		cbind_name:   ''
		cbind_data:   []u8{}
		client_nonce: 'clientnoncevalue'
		iterations:   4096
		client_first: 'n,,n=a=2Cb=3Dc,r=clientnoncevalue'
		server_first: 'r=clientnoncevalueservernoncevalue,s=c2FsdHNhbHRzYWx0,i=4096'
		client_final: 'c=biws,r=clientnoncevalueservernoncevalue,p=mAxDUCU7uqrX8ugJFx0YeyeR6SpsPh8NwIg3VVhYOPI='
		server_final: 'v=bs9NAJ5oBl5qTt4nBsh+jI8HTkH4f00eDZnDbuCO/NA='
		salted_hex:   '1ae0aa4d817a79c294fe005e1c565d2240d9a26eb79e762d67a5ed080c0c446a'
		stored_hex:   '5f256a1f2488e060b439c6441c60acee66179c113baf08b221600a03cb121bd2'
		server_hex:   '73f468f717e0f4d36ee0a62acd79f87e30a604745e1e24525cdb54d5b8c3445b'
	},
	Vector{
		name:         'authzid'
		mechanism:    .sha256
		username:     'user'
		password:     'pencil'
		authzid:      'admin'
		cbind_name:   ''
		cbind_data:   []u8{}
		client_nonce: 'clientnoncevalue'
		iterations:   4096
		client_first: 'n,a=admin,n=user,r=clientnoncevalue'
		server_first: 'r=clientnoncevalueservernoncevalue,s=c2FsdHNhbHRzYWx0,i=4096'
		client_final: 'c=bixhPWFkbWluLA==,r=clientnoncevalueservernoncevalue,p=3FfNqGToGB71FEKlZCL+JtZgjEJvYdT9R5sKBAO3AfM='
		server_final: 'v=1RSa3Iu5z5Iet9kidUeNJmAKeOdyBzwQMIXtAfuvWtU='
		salted_hex:   '1ae0aa4d817a79c294fe005e1c565d2240d9a26eb79e762d67a5ed080c0c446a'
		stored_hex:   '5f256a1f2488e060b439c6441c60acee66179c113baf08b221600a03cb121bd2'
		server_hex:   '73f468f717e0f4d36ee0a62acd79f87e30a604745e1e24525cdb54d5b8c3445b'
	},
	Vector{
		name:         'channel_binding'
		mechanism:    .sha256
		username:     'user'
		password:     'pencil'
		authzid:      ''
		cbind_name:   'tls-server-end-point'
		cbind_data:   [u8(0x00), 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b,
			0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
			0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f]
		client_nonce: 'clientnoncevalue'
		iterations:   4096
		client_first: 'p=tls-server-end-point,,n=user,r=clientnoncevalue'
		server_first: 'r=clientnoncevalueservernoncevalue,s=c2FsdHNhbHRzYWx0,i=4096'
		client_final: 'c=cD10bHMtc2VydmVyLWVuZC1wb2ludCwsAAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=,r=clientnoncevalueservernoncevalue,p=Bl9ZPqOc0PRnfQbcsxDBdHFvs/irfY5cAY9///2hXEw='
		server_final: 'v=ctaZtb0RgoPWp5lsaMAammetaiK6ZitqSzKlJMWRv6U='
		salted_hex:   '1ae0aa4d817a79c294fe005e1c565d2240d9a26eb79e762d67a5ed080c0c446a'
		stored_hex:   '5f256a1f2488e060b439c6441c60acee66179c113baf08b221600a03cb121bd2'
		server_hex:   '73f468f717e0f4d36ee0a62acd79f87e30a604745e1e24525cdb54d5b8c3445b'
	},
	Vector{
		name:         'utf8_password'
		mechanism:    .sha256
		username:     'rené'
		password:     'pässwörd'
		authzid:      ''
		cbind_name:   ''
		cbind_data:   []u8{}
		client_nonce: 'clientnoncevalue'
		iterations:   10000
		client_first: 'n,,n=rené,r=clientnoncevalue'
		server_first: 'r=clientnoncevalueservernoncevalue,s=c2FsdHNhbHRzYWx0,i=10000'
		client_final: 'c=biws,r=clientnoncevalueservernoncevalue,p=NrtHNv7awL5KZ/rOvK46mzfMvAxvk4UiNkTBxGaEK1I='
		server_final: 'v=faJh6FpEnTwYd397AElAHqIB5g5AwZIFNA5uT6Fv2Vo='
		salted_hex:   'd6c4b3c07b85efc838b771f7fbdc0456d41226b2d13e9c7577199a3571d272fb'
		stored_hex:   '59c2e0b1992277a63efacab663766bc8476fb36389eda40c8ee65ed1c470794a'
		server_hex:   '90a296069bfd8fb1d08394eab964701a562c316ae8343c122524628d30518b0d'
	},
	Vector{
		name:         'min_iterations'
		mechanism:    .sha1
		username:     'user'
		password:     'pencil'
		authzid:      ''
		cbind_name:   ''
		cbind_data:   []u8{}
		client_nonce: 'clientnoncevalue'
		iterations:   4096
		client_first: 'n,,n=user,r=clientnoncevalue'
		server_first: 'r=clientnoncevalueservernoncevalue,s=c2FsdHNhbHRzYWx0,i=4096'
		client_final: 'c=biws,r=clientnoncevalueservernoncevalue,p=gAEjRo87i97AqQag7Xa7LkE/Ohc='
		server_final: 'v=7jImVhD+ep8a6fy+p1OzhHs4ND0='
		salted_hex:   '0885e928b73f79ce2d1e05ef72d6a3148c3b66e8'
		stored_hex:   'c46c6e5e4034522126ce29b6f1b3427d5e0998bf'
		server_hex:   '591299d40b67907afa1d913afffa25e01c5b13b6'
	},
]
