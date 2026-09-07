// HTTP Message Signatures (RFC 9421) - module entry point.
//
// Two API layers:
//
//   * **Components-based** - `signature_base_string`, `sign`, `verify`
//     work over a generic `Components` value. Use this when you have
//     parsed the HTTP message yourself or when signing offline.
//
//   * **`http.Request` / `http.Response` integration** - the
//     `sign_request`, `verify_request`, `sign_response`,
//     `verify_response` helpers wrap the components layer for code
//     that already speaks `vlib/net/http`.
//
// The output of `sign` is the literal pair of header values you set
// on the message - one for `Signature-Input`, one for `Signature` -
// not a higher-level "signed message" object. Symmetry is the goal -
// the verifier reparses the same headers and recomputes the base.
module signature

// SignedHeaders bundles the two HTTP header values produced by
// `sign`. The signer attaches these as `Signature-Input` and
// `Signature` respectively.
pub struct SignedHeaders {
pub:
	signature_input string
	signature       string
}

// sign computes the signature base, signs it with `key`, and returns
// the two header values to attach to the message under `label`.
//
// Required parameters in `p`:
//   * `components` - the ordered covered-components list
//   * either `keyid` (commonly required for routing) or none (rare)
//
// `key.algorithm` selects the signing routine; if `p.alg` is also set
// it MUST match the algorithm of `key` (RFC 9421 §3.1 step 3).
pub fn sign(c Components, p SignatureParams, key Key, label string) !SignedHeaders {
	check_label(label)!
	check_alg_consistency(p, key)!
	mut p2 := SignatureParams{
		components: p.components.clone()
		keyid:      p.keyid
		alg:        p.alg
		created:    p.created
		expires:    p.expires
		nonce:      p.nonce
		tag:        p.tag
	}
	if p2.keyid == none {
		if kid := key.keyid {
			p2.keyid = kid
		}
	}
	base := signature_base_string(c, p2)!
	sig := sign_base(base.bytes(), key)!
	return SignedHeaders{
		signature_input: signature_input_value(label, p2)
		signature:       signature_header_value(label, sig)
	}
}

// VerifyOptions tweaks `verify`. `now_unix` enables the optional
// `expires` parameter check (any value > 0 turns it on); leave it at
// the default to skip the expiry check entirely. Kept as a single
// option struct so future toggles (clock skew tolerance, allowed
// algorithm list…) land here without breaking signatures.
@[params]
pub struct VerifyOptions {
pub:
	now_unix i64
}

// verify checks the signature for `label` against `c` using `key`.
// Both header values are taken as-they-appear-on-the-wire (i.e. the
// raw `Signature-Input` and `Signature` field values).
//
// `label` selects which signature to check when several are present.
// Pass an empty string to verify the only signature - the call fails
// with `MalformedMessage` if zero or more than one is found. When
// `opts.now_unix > 0` the `expires` parameter is also enforced.
pub fn verify(c Components, sig_input_header string, signature_header string, label string, key Key, opts VerifyOptions) ! {
	entries := parse_signature_input(sig_input_header)!
	signatures := parse_signature(signature_header)!
	wanted := pick_label(entries, signatures, label)!
	entry := find_entry(entries, wanted) or {
		return MalformedMessage{
			reason: 'Signature-Input has no entry for label "${wanted}"'
		}
	}
	sig := signatures[wanted] or {
		return MalformedMessage{
			reason: 'Signature header has no entry for label "${wanted}"'
		}
	}
	check_alg_param(entry, key)!
	base := signature_base_from_entry(c, entry)!
	verify_base(base.bytes(), sig, key, wanted)!
	if opts.now_unix > 0 {
		if exp_v := entry.params['expires'] {
			if exp_v is i64 {
				if opts.now_unix >= exp_v {
					return SignatureExpired{
						expires: exp_v
						now:     opts.now_unix
					}
				}
			}
		}
	}
}

fn pick_label(entries []SignatureEntry, signatures map[string][]u8, requested string) !string {
	if requested != '' {
		return requested
	}
	if entries.len == 1 && signatures.len == 1 {
		return entries[0].label
	}
	if entries.len == 0 {
		return MalformedMessage{
			reason: 'Signature-Input is empty'
		}
	}
	return MalformedMessage{
		reason: 'multiple signatures present; pass a label to choose one'
	}
}

fn find_entry(entries []SignatureEntry, label string) ?SignatureEntry {
	for e in entries {
		if e.label == label {
			return e
		}
	}
	return none
}

fn check_label(label string) ! {
	if label == '' {
		return MalformedMessage{
			reason: 'signature label cannot be empty'
		}
	}
	for c in label {
		if !((c >= `a` && c <= `z`) || (c >= `0` && c <= `9`) || c == `-` || c == `_` || c == `*`) {
			return MalformedMessage{
				reason: 'signature label "${label}" must match the Structured Field key grammar (lowercase + digits + - _ *)'
			}
		}
	}
	first := label[0]
	if !((first >= `a` && first <= `z`) || first == `*`) {
		return MalformedMessage{
			reason: 'signature label "${label}" must start with a lowercase letter or "*"'
		}
	}
}

fn check_alg_consistency(p SignatureParams, key Key) ! {
	if alg_str := p.alg {
		want := algorithm_from_name(alg_str) or {
			return UnsupportedAlgorithm{
				name: alg_str
			}
		}
		if want != key.algorithm {
			return MalformedMessage{
				reason: 'alg parameter "${alg_str}" does not match key algorithm "${key.algorithm.name()}"'
			}
		}
	}
}

fn check_alg_param(entry SignatureEntry, key Key) ! {
	if alg_v := entry.params['alg'] {
		if alg_v is string {
			want := algorithm_from_name(alg_v) or {
				return UnsupportedAlgorithm{
					name: alg_v
				}
			}
			if want != key.algorithm {
				return MalformedMessage{
					reason: 'signature alg "${alg_v}" does not match key algorithm "${key.algorithm.name()}"'
				}
			}
		}
	}
}
