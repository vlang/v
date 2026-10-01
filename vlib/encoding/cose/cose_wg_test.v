// Runs the cose-wg/Examples vectors bundled under `tests/cose_wg`
// (Unlicense) against this module. Every file in that directory must be
// classified in `wg_cases` below, and every entry of `wg_cases` must
// name a file that exists: a vector dropped into the directory cannot
// sit there unexercised, and a vector that is removed cannot leave a
// stale expectation behind.
//
// Vectors outside this module's scope are kept rather than deleted, as
// refusals with a written reason. They document where the boundary is,
// and they fail loudly if the boundary ever moves.
module cose

import encoding.base64
import encoding.cbor
import encoding.hex
import json2
import os

// WgOutcome is what this module must do with one bundled vector.
enum WgOutcome {
	// verifies: the message verifies and returns the fixture plaintext.
	verifies
	// rejected: the vector was deliberately tampered with and must be
	// refused.
	rejected
	// unsupported: the vector is valid COSE but uses something this
	// module does not implement, so it must also be refused.
	unsupported
}

// WgCase is the expectation attached to one vector file.
struct WgCase {
	outcome WgOutcome
	// reason documents a `rejected` or `unsupported` classification. It
	// is what makes a refusal an assertion rather than a shrug.
	reason string
	// error_contains is a fragment the module's own refusal must carry.
	// Without it a vector would pass on any error at all, including one
	// this test file caused by mis-preparing the fixture.
	error_contains string
}

const wg_cases = {
	'ecdsa-sig-01.json': WgCase{
		outcome: .verifies
	}
	'ecdsa-sig-03.json': WgCase{
		outcome: .verifies
	}
	'ecdsa-sig-04.json': WgCase{
		outcome:        .unsupported
		reason:         'ES512 over a P-256 key; this module binds each ECDSA algorithm to one curve'
		error_contains: 'ES512 requires crv=p_521'
	}
	'eddsa-01.json':     WgCase{
		outcome: .verifies
	}
	'eddsa-02.json':     WgCase{
		outcome:        .unsupported
		reason:         'Ed448, which cose.Curve does not model'
		error_contains: 'OKP key uses unsupported curve 7'
	}
	'eddsa-sig-01.json': WgCase{
		outcome: .verifies
	}
	'eddsa-sig-02.json': WgCase{
		outcome:        .unsupported
		reason:         'Ed448, which cose.Curve does not model'
		error_contains: 'OKP key uses unsupported curve 7'
	}
	'HMac-01.json':      WgCase{
		outcome: .verifies
	}
	'HMac-04.json':      WgCase{
		outcome:        .rejected
		reason:         'tag altered (ChangeTag)'
		error_contains: 'verification failed'
	}
	'HMac-05.json':      WgCase{
		outcome: .verifies
	}
	'HMac-enc-01.json':  WgCase{
		outcome: .verifies
	}
	'HMac-enc-02.json':  WgCase{
		outcome: .verifies
	}
	'HMac-enc-03.json':  WgCase{
		outcome: .verifies
	}
	'HMac-enc-04.json':  WgCase{
		outcome:        .rejected
		reason:         'tag altered (ChangeTag)'
		error_contains: 'verification failed'
	}
	'HMac-enc-05.json':  WgCase{
		outcome: .verifies
	}
	'sign-fail-01.json': WgCase{
		outcome:        .rejected
		reason:         'CBOR tag 998 instead of the Sign1 tag 18'
		error_contains: 'tag'
	}
	'sign-fail-02.json': WgCase{
		outcome:        .rejected
		reason:         'last payload byte altered'
		error_contains: 'verification failed'
	}
	'sign-fail-03.json': WgCase{
		outcome:        .rejected
		reason:         'protected alg replaced by the unregistered code -999'
		error_contains: 'algorithm missing from Sign1 headers'
	}
	'sign-fail-04.json': WgCase{
		outcome:        .rejected
		reason:         'protected alg replaced by the text value "unknown"'
		error_contains: 'algorithm missing from Sign1 headers'
	}
	'sign-pass-01.json': WgCase{
		outcome: .verifies
	}
	'sign-pass-02.json': WgCase{
		outcome: .verifies
	}
}

fn test_cose_wg_vectors() {
	dir := os.join_path(os.dir(@FILE), 'tests', 'cose_wg')
	mut names := os.ls(dir)!
	names.sort()
	mut ran := map[string]bool{}
	for name in names {
		if !name.ends_with('.json') {
			continue
		}
		expected := wg_cases[name] or {
			assert false, 'vector ${name} is not classified in wg_cases'
			continue
		}
		ran[name] = true
		run_wg_vector(os.join_path(dir, name), expected) or {
			assert false, '${name}: ${err.msg()}'
		}
	}
	for name, _ in wg_cases {
		assert name in ran, 'wg_cases classifies ${name}, which is not in ${dir}'
	}
}

// run_wg_vector verifies one vector and compares the outcome with its
// classification.
//
// Preparing the fixture and exercising the module are kept apart on
// purpose: anything that goes wrong while reading the JSON or building
// the key fails the test outright, and only an error raised by the
// module itself is allowed to satisfy a `rejected` or `unsupported`
// classification. Otherwise a fixture this file mis-read would pass as
// a refusal the module never made.
fn run_wg_vector(path string, expected WgCase) ! {
	doc := json2.decode[json2.Any](os.read_file(path)!)!.as_map()
	input := wg_field(doc, 'input')!.as_map()
	output := wg_field(doc, 'output')!.as_map()
	message := hex.decode(wg_field(output, 'cbor')!.str())!
	want := wg_plaintext(input)!
	prepared := wg_prepare(input)!
	// A COSE_Sign vector is verified signer by signer, so a fixture that
	// lost one would silently shrink the loop instead of failing. Pin the
	// count here, where an error is a fault of this file rather than a
	// verdict about the module. A message that does not decode at all is
	// left to `wg_invoke`, whose refusal is the module's own.
	if prepared.kind == .sign {
		if decoded := SignMessage.decode(message) {
			if prepared.keys.len != decoded.signatures.len {
				return error('prepared ${prepared.keys.len} signer keys for ${decoded.signatures.len} signatures')
			}
		}
	}

	payload := wg_invoke(prepared, message) or {
		if expected.outcome == .verifies {
			return error('expected to verify, got: ${err.msg()}')
		}
		if !err.msg().contains(expected.error_contains) {
			return error('expected a refusal mentioning "${expected.error_contains}", got: ${err.msg()}')
		}
		return
	}
	if expected.outcome != .verifies {
		return error('expected a refusal (${expected.reason}), but it verified')
	}
	if payload != want {
		return error('verified payload does not match the fixture plaintext')
	}
}

// WgKind is the COSE message type a vector describes.
enum WgKind {
	sign0
	sign
	mac0
	mac
	encrypted
}

// WgInvocation is everything the module needs, read out of the fixture
// before any module call is made.
struct WgInvocation {
	kind     WgKind
	keys     []Key
	external []u8
}

// wg_prepare reads a vector into the arguments of a module call. Every
// error it raises is a fault in this file or in the fixture, never a
// verdict about the module.
fn wg_prepare(input map[string]json2.Any) !WgInvocation {
	if entry := input['sign0'] {
		m := entry.as_map()
		return WgInvocation{
			kind:     .sign0
			keys:     [wg_key(wg_field(m, 'key')!.as_map(), m)!]
			external: wg_external(m)!
		}
	}
	if entry := input['sign'] {
		m := entry.as_map()
		signers := wg_field(m, 'signers')!.as_array()
		if signers.len == 0 {
			// A COSE_Sign vector with no signer would make the verify
			// loop below a no-op and report success without ever calling
			// the module.
			return error('the vector declares no signer')
		}
		mut keys := []Key{}
		for signer in signers {
			sm := signer.as_map()
			keys << wg_key(wg_field(sm, 'key')!.as_map(), sm)!
		}
		return WgInvocation{
			kind:     .sign
			keys:     keys
			external: wg_external(m)!
		}
	}
	for name, kind in {
		'mac0': WgKind.mac0
		'mac':  WgKind.mac
	} {
		if entry := input[name] {
			m := entry.as_map()
			return WgInvocation{
				kind:     kind
				keys:     [wg_recipient_key(m)!]
				external: wg_external(m)!
			}
		}
	}
	if _ := input['encrypted'] {
		return WgInvocation{
			kind: .encrypted
		}
	}
	return error('the vector has no message this file knows how to feed the module')
}

// wg_invoke performs the module call under test and nothing else, so
// that the error it returns — if any — is the module's own verdict.
fn wg_invoke(prepared WgInvocation, message []u8) ![]u8 {
	match prepared.kind {
		.sign0 {
			return verify1(message, prepared.keys[0], external_aad: prepared.external)
		}
		.sign {
			msg := SignMessage.decode(message)!
			for i, key in prepared.keys {
				msg.verify(i, key, external_aad: prepared.external)!
			}
			return msg.payload or { return error('the message carries no attached payload') }
		}
		.mac0 {
			return verify_mac0(message, prepared.keys[0], external_aad: prepared.external)
		}
		.mac {
			return verify_mac(message, prepared.keys[0], external_aad: prepared.external)
		}
		.encrypted {
			// There is no encryption API to call, so the closest the
			// module can be asked is whether it mistakes the message for
			// one of the types it does implement.
			Sign1Message.decode(message)!
			return error('COSE_Encrypt0 decoded as a Sign1 message')
		}
	}
}

// wg_recipient_key returns the shared symmetric key of a MAC vector,
// which the fixtures carry on the single direct-mode recipient.
fn wg_recipient_key(m map[string]json2.Any) !Key {
	recipients := wg_field(m, 'recipients')!.as_array()
	if recipients.len != 1 {
		return error('expected exactly one recipient, got ${recipients.len}')
	}
	return wg_key(wg_field(recipients[0].as_map(), 'key')!.as_map(), m)!
}

// wg_key builds a `Key` from the fixture's JWK-shaped key object. The
// surrounding `owner` object supplies the algorithm the verifier is
// meant to use, which is bound to the key: a vector whose `alg` lives in
// the unprotected header only verifies when the key constrains it.
//
// A curve this module does not model is not a reason to give up here:
// the key is assembled as a COSE_Key and read back through `Key.decode`,
// which preserves the unsupported identifier. The refusal then comes
// from the module, at the point where the key is actually used.
fn wg_key(k map[string]json2.Any, owner map[string]json2.Any) !Key {
	kty := wg_field(k, 'kty')!.str()
	mut key := match kty {
		'EC' { wg_ec_key(k, i64(KeyType.ec2), [key_label_x, key_label_y, key_label_d])! }
		'OKP' { wg_ec_key(k, i64(KeyType.okp), [key_label_x, key_label_d])! }
		'oct' { Key.symmetric(wg_bytes(k, 'k')!) }
		else { return error('the vector uses a key type this file cannot build: ${kty}') }
	}
	if alg := owner['alg'] {
		key.alg = wg_algorithm(alg.str())!
	}
	return key
}

// wg_ec_key assembles an EC2 or OKP COSE_Key and decodes it with the
// module's own codec, so that the resulting Key — including one on a
// curve the module does not support — is one the module produced.
fn wg_ec_key(k map[string]json2.Any, kty i64, labels []i64) !Key {
	names := {
		key_label_x: 'x'
		key_label_y: 'y'
		key_label_d: 'd'
	}
	mut pairs := [
		cbor.MapPair{
			key:   cbor.new_int(key_label_kty)
			value: cbor.new_int(kty)
		},
		cbor.MapPair{
			key:   cbor.new_int(key_label_crv)
			value: cbor.new_int(wg_curve(wg_field(k, 'crv')!.str())!)
		},
	]
	for label in labels {
		pairs << cbor.MapPair{
			key:   cbor.new_int(label)
			value: cbor.new_bytes(wg_bytes(k, names[label])!)
		}
	}
	return Key.decode(cbor.encode(cbor.Value(cbor.Map{ pairs: pairs }), cbor.EncodeOpts{
		canonical: true
	})!)!
}

// wg_bytes reads one key parameter, which the fixtures spell either
// base64url (`x`) or hex (`x_hex`) depending on the vector.
fn wg_bytes(k map[string]json2.Any, name string) ![]u8 {
	if v := k[name] {
		return base64.url_decode(v.str())
	}
	if v := k['${name}_hex'] {
		return hex.decode(v.str())!
	}
	return error('the vector key has no ${name} parameter')
}

// wg_external returns the external AAD a vector signs over, empty when
// the vector does not exercise one.
fn wg_external(m map[string]json2.Any) ![]u8 {
	external := m['external'] or { return []u8{} }
	return hex.decode(external.str())!
}

// wg_plaintext returns the payload a vector is expected to yield, spelled
// either as text or as hex.
fn wg_plaintext(input map[string]json2.Any) ![]u8 {
	if v := input['plaintext'] {
		return v.str().bytes()
	}
	if v := input['plaintext_hex'] {
		return hex.decode(v.str())!
	}
	return error('the vector has no plaintext')
}

// wg_curve maps a JWK curve name to its IANA "COSE Elliptic Curves"
// code. Curves the module does not model are mapped too — Ed448 is 7 —
// precisely so that the refusal comes from the module rather than from
// this file.
fn wg_curve(name string) !i64 {
	return match name {
		'P-256' { i64(1) }
		'P-384' { i64(2) }
		'P-521' { i64(3) }
		'X25519' { i64(4) }
		'X448' { i64(5) }
		'Ed25519' { i64(6) }
		'Ed448' { i64(7) }
		else { error('the vector uses a curve this file does not know: ${name}') }
	}
}

// wg_algorithm maps a COSE algorithm name to the typed identifier.
fn wg_algorithm(name string) !Algorithm {
	return match name {
		'ES256' { Algorithm.es256 }
		'ES384' { Algorithm.es384 }
		'ES512' { Algorithm.es512 }
		'EdDSA' { Algorithm.eddsa }
		'HS256/64' { Algorithm.hmac_256_64 }
		'HS256' { Algorithm.hmac_256_256 }
		'HS384' { Algorithm.hmac_384_384 }
		'HS512' { Algorithm.hmac_512_512 }
		else { error('the vector uses an algorithm this file does not know: ${name}') }
	}
}

// wg_field reads a required member of a fixture object.
fn wg_field(m map[string]json2.Any, name string) !json2.Any {
	return m[name] or { error('the vector has no "${name}" member') }
}
