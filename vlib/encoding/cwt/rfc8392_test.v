// Runs the RFC 8392 Appendix A vectors bundled under `tests/rfc8392`
// against this module. Every file in that directory must be classified
// in `rfc8392_cases` below, and every entry must name a file that
// exists, so a vector can neither sit unexercised nor leave a stale
// expectation behind.
//
// The encrypted vectors are kept rather than deleted, as refusals with a
// written reason: they mark where this module stops.
module cwt

import encoding.cose
import encoding.hex
import json2
import os

// VectorOutcome is what this module must do with one bundled vector.
enum VectorOutcome {
	// verifies: the token verifies and yields the fixture claims.
	verifies
	// unsupported: valid CWT, but built on a COSE message type this
	// module does not implement, so it must be refused.
	unsupported
}

// VectorCase is the expectation attached to one vector file.
struct VectorCase {
	outcome VectorOutcome
	// reason documents an `unsupported` classification.
	reason string
	// error_contains is a fragment the module's own refusal must carry,
	// so that a vector cannot pass on an error this file caused while
	// preparing the fixture.
	error_contains string
}

const rfc8392_cases = {
	'A_3.json': VectorCase{
		outcome: .verifies
	}
	'A_4.json': VectorCase{
		outcome: .verifies
	}
	'A_5.json': VectorCase{
		outcome:        .unsupported
		reason:         'COSE_Encrypt0 (AES-CCM-16-128/64), which encoding.cose does not implement'
		error_contains: 'tag'
	}
	'A_6.json': VectorCase{
		outcome:        .unsupported
		reason:         'a signed CWT wrapped in COSE_Encrypt0, which encoding.cose does not implement'
		error_contains: 'tag'
	}
	'A_7.json': VectorCase{
		outcome:        .unsupported
		reason:         'iat is the fractional NumericDate 1443944944.5; ClaimsSet models time as whole i64 seconds'
		// `verify_mac` authenticates before decoding the claims, so this
		// fragment also proves the refusal is the claim model and not a
		// MAC that failed to check out.
		error_contains: 'fractional NumericDate'
	}
}

fn test_rfc8392_vectors() {
	dir := os.join_path(os.dir(@FILE), 'tests', 'rfc8392')
	mut names := os.ls(dir)!
	names.sort()
	mut ran := map[string]bool{}
	for name in names {
		if !name.ends_with('.json') {
			continue
		}
		expected := rfc8392_cases[name] or {
			assert false, 'vector ${name} is not classified in rfc8392_cases'
			continue
		}
		ran[name] = true
		run_rfc8392_vector(os.join_path(dir, name), expected) or {
			assert false, '${name}: ${err.msg()}'
		}
	}
	for name, _ in rfc8392_cases {
		assert name in ran, 'rfc8392_cases classifies ${name}, which is not in ${dir}'
	}
}

// run_rfc8392_vector verifies one vector and compares the outcome with
// its classification. The claims are checked against the fixture's own
// plaintext, decoded independently, so a token that verifies but decodes
// into different claims still fails.
//
// Reading the fixture and exercising the module are kept apart: a fault
// in this file fails the test outright, and only an error the module
// raised may satisfy an `unsupported` classification.
fn run_rfc8392_vector(path string, expected VectorCase) ! {
	doc := json2.decode[json2.Any](os.read_file(path)!)!.as_map()
	input := vector_field(doc, 'input')!.as_map()
	output := vector_field(doc, 'output')!.as_map()
	token := hex.decode(vector_field(output, 'cbor')!.str())!
	prepared := prepare_rfc8392_vector(input)!

	claims := invoke_rfc8392_vector(prepared, token) or {
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
	want := ClaimsSet.decode(hex.decode(vector_field(input, 'plaintext_hex')!.str())!)!
	if claims.encode()! != want.encode()! {
		return error('the verified claims do not match the fixture payload')
	}
}

// VectorKind is the COSE message a vector wraps its claims in.
enum VectorKind {
	signed
	maced
	encrypted
}

// VectorInvocation is what the module is called with, read out of the
// fixture before any module call is made.
struct VectorInvocation {
	kind VectorKind
	key  cose.Key
}

// prepare_rfc8392_vector reads a vector into the arguments of a module
// call. Every error it raises is a fault in this file or in the fixture.
fn prepare_rfc8392_vector(input map[string]json2.Any) !VectorInvocation {
	if entry := input['sign0'] {
		k := vector_field(entry.as_map(), 'key')!.as_map()
		return VectorInvocation{
			kind: .signed
			key:  cose.Key.ec2_private(.p_256, vector_bytes(k, 'x')!, vector_bytes(k, 'y')!,
				vector_bytes(k, 'd')!)
		}
	}
	if entry := input['mac0'] {
		return VectorInvocation{
			kind: .maced
			key:  vector_recipient_key(entry.as_map())!
		}
	}
	if entry := input['encrypted'] {
		// The symmetric key is prepared even though no API consumes it:
		// the module must refuse the message, not the key.
		return VectorInvocation{
			kind: .encrypted
			key:  vector_recipient_key(entry.as_map())!
		}
	}
	return error('the vector has no message this file knows how to feed the module')
}

// invoke_rfc8392_vector performs the module call under test and nothing
// else. An encrypted vector is offered to `verify_mac`, which is what a
// consumer holding that symmetric key would reach for, so the refusal is
// the module's.
fn invoke_rfc8392_vector(prepared VectorInvocation, token []u8) !ClaimsSet {
	return match prepared.kind {
		.signed { verify(token, prepared.key)! }
		.maced, .encrypted { verify_mac(token, prepared.key)! }
	}
}

// vector_recipient_key returns the shared symmetric key a vector carries
// on its single direct-mode recipient.
fn vector_recipient_key(m map[string]json2.Any) !cose.Key {
	recipients := vector_field(m, 'recipients')!.as_array()
	if recipients.len != 1 {
		return error('expected exactly one recipient, got ${recipients.len}')
	}
	k := vector_field(recipients[0].as_map(), 'key')!.as_map()
	return cose.Key.symmetric(vector_bytes(k, 'k')!)
}

// vector_bytes reads one key parameter, which the RFC 8392 fixtures
// spell in hex.
fn vector_bytes(k map[string]json2.Any, name string) ![]u8 {
	value := k['${name}_hex'] or { return error('the vector key has no ${name}_hex') }
	return hex.decode(value.str())!
}

// vector_field reads a required member of a fixture object.
fn vector_field(m map[string]json2.Any, name string) !json2.Any {
	return m[name] or { error('the vector has no "${name}" member') }
}
