// Construction of the canonical signature base string per
// RFC 9421 §2.5. The signature base is the data passed to the
// signing primitive; both signer and verifier MUST produce identical
// bytes here for verification to succeed.
//
// Format (one line per covered component, separated by LF):
//
//   "<component>": <component-value>
//   …
//   "@signature-params": <inner-list>;<params>
//
// The final line is the only one without a trailing newline.
module signature

// SignatureParams holds the parameters that go after the inner-list
// in the @signature-params line and on the Signature-Input header.
// `components` is the *ordered* list the verifier walks; mutating
// the order changes the wire bytes and breaks verification.
@[params]
pub struct SignatureParams {
pub mut:
	components []string
	keyid      ?string
	alg        ?string
	created    ?i64
	expires    ?i64
	nonce      ?string
	tag        ?string
}

// signature_base_string returns the bytes that go into the signing
// primitive. RFC 9421 §2.5 step 7 forbids duplicate covered components
// (the verifier rejects them), so we enforce that here too.
pub fn signature_base_string(c Components, p SignatureParams) !string {
	return build_signature_base(c, p.components, serialize_signature_params(p))!
}

// build_signature_base is the shared core. Both `signature_base_string`
// (signer side - canonical params from the struct) and the verify
// path (verifier side - params replayed verbatim from the wire) drive
// it; only the way they obtain `signature_params_value` differs.
fn build_signature_base(c Components, components []string, signature_params_value string) !string {
	mut seen := map[string]bool{}
	mut lines := []string{cap: components.len + 1}
	for name in components {
		if name in seen {
			return MalformedMessage{
				reason: 'covered components list contains duplicate "${name}"'
			}
		}
		seen[name] = true
		value := c.component_value(name)!
		lines << '"' + name + '": ' + value
	}
	lines << '"@signature-params": ' + signature_params_value
	return lines.join('\n')
}

// serialize_signature_params formats the inner-list-with-parameters
// segment that goes both into the @signature-params line and into
// the Signature-Input header value. Parameter order is fixed
// (created, expires, nonce, alg, keyid, tag) for diff-stability;
// RFC 9421 doesn't constrain order and verifiers reparse anyway.
pub fn serialize_signature_params(p SignatureParams) string {
	mut pairs := []ParamPair{cap: 6}
	if v := p.created {
		pairs << ParamPair{
			name:  'created'
			value: v
		}
	}
	if v := p.expires {
		pairs << ParamPair{
			name:  'expires'
			value: v
		}
	}
	if v := p.nonce {
		pairs << ParamPair{
			name:  'nonce'
			value: v
		}
	}
	if v := p.alg {
		pairs << ParamPair{
			name:  'alg'
			value: v
		}
	}
	if v := p.keyid {
		pairs << ParamPair{
			name:  'keyid'
			value: v
		}
	}
	if v := p.tag {
		pairs << ParamPair{
			name:  'tag'
			value: v
		}
	}
	return serialize_inner_list(p.components) + serialize_params(pairs)
}

// signature_input_value returns the full Signature-Input value for
// `label`, ready to be put into the header `Signature-Input: <…>`.
pub fn signature_input_value(label string, p SignatureParams) string {
	return label + '=' + serialize_signature_params(p)
}

// signature_header_value returns the full Signature value for `label`,
// ready to be put into the header `Signature: <…>`.
pub fn signature_header_value(label string, sig []u8) string {
	return label + '=' + encode_byte_sequence(sig)
}

// signature_base_from_entry replays the signature base for a parsed
// SignatureEntry. The `@signature-params` line uses the entry's raw
// wire bytes verbatim so verification matches exactly what the signer
// signed, regardless of the order in which the signer emitted its
// parameters.
fn signature_base_from_entry(c Components, entry SignatureEntry) !string {
	return build_signature_base(c, entry.components, entry.signature_params_value)!
}
