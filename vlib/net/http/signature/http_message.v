// http.Request / http.Response integration. These helpers are thin
// wrappers - they translate the message into a `Components` value and
// delegate to `sign` / `verify`. Keeping the conversion isolated here
// means the components-level API stays the canonical surface.
module signature

import net.http
import net.urllib
import time

// SignRequestOptions parametrises `sign_request`. `components` is
// optional - when omitted we sign the conservative default
// (`@method`, `@target-uri`, `@authority`, plus the `Date` header if
// present). RFC 9421 doesn't mandate a default; this one mirrors what
// most production deployments use.
@[params]
pub struct SignRequestOptions {
pub:
	components []string
	label      string = 'sig1'
	keyid      ?string
	created    ?i64
	expires    ?i64
	nonce      ?string
	tag        ?string
	// include_alg, when true, emits the `alg` signature parameter on
	// the wire. Most signers leave it off (the verifier looks the alg
	// up by `keyid`); set to true for explicit signalling.
	include_alg bool
}

// sign_request signs an HTTP request in place by appending the
// `Signature-Input` and `Signature` header fields. Existing values of
// these headers are preserved (RFC 9421 §4.3 - multiple signatures
// can coexist), so calling this twice with different labels yields
// two co-existing signatures.
//
// `created` defaults to `time.now().unix()` when omitted, since
// RFC 9421 §7.2.1 RECOMMENDS the parameter for replay protection.
// Pass an explicit `created: 0` only if you know you don't want it.
pub fn sign_request(mut req http.Request, key Key, opts SignRequestOptions) ! {
	c := request_components(req)!
	mut comps := opts.components.clone()
	if comps.len == 0 {
		comps = default_request_components(req)
	}
	mut alg := ?string(none)
	if opts.include_alg {
		alg = key.algorithm.name()
	}
	p := SignatureParams{
		components: comps
		keyid:      opts.keyid
		alg:        alg
		created:    opts.created or { time.now().unix() }
		expires:    opts.expires
		nonce:      opts.nonce
		tag:        opts.tag
	}
	out := sign(c, p, key, opts.label)!
	append_dict_header(mut req.header, 'Signature-Input', out.signature_input)!
	append_dict_header(mut req.header, 'Signature', out.signature)!
}

// VerifyRequestOptions parametrises `verify_request`. `label` selects
// which labelled signature to verify when several are present.
@[params]
pub struct VerifyRequestOptions {
pub:
	label    string
	now_unix i64
}

// verify_request verifies a labelled signature on an HTTP request. If
// `opts.label` is empty and exactly one signature is present, that
// one is checked. If `opts.now_unix > 0`, the `expires` parameter is
// also enforced.
pub fn verify_request(req http.Request, key Key, opts VerifyRequestOptions) ! {
	c := request_components(req)!
	sig_input := merged_dict_field(req.header, 'Signature-Input') or {
		return MalformedMessage{
			reason: 'request has no Signature-Input header'
		}
	}
	sig_value := merged_dict_field(req.header, 'Signature') or {
		return MalformedMessage{
			reason: 'request has no Signature header'
		}
	}
	verify(c, sig_input, sig_value, opts.label, key, now_unix: opts.now_unix)!
}

// SignResponseOptions / VerifyResponseOptions mirror their request
// counterparts. Defaults assume a status-code-and-content scenario.
@[params]
pub struct SignResponseOptions {
pub:
	components  []string
	label       string = 'sig1'
	keyid       ?string
	created     ?i64
	expires     ?i64
	nonce       ?string
	tag         ?string
	include_alg bool
}

// sign_response signs an HTTP response in place. Like `sign_request`
// it preserves any pre-existing Signature-Input / Signature values
// and defaults `created` to the current time.
pub fn sign_response(mut resp http.Response, key Key, opts SignResponseOptions) ! {
	c := response_components(resp)
	mut comps := opts.components.clone()
	if comps.len == 0 {
		comps = ['@status']
	}
	mut alg := ?string(none)
	if opts.include_alg {
		alg = key.algorithm.name()
	}
	p := SignatureParams{
		components: comps
		keyid:      opts.keyid
		alg:        alg
		created:    opts.created or { time.now().unix() }
		expires:    opts.expires
		nonce:      opts.nonce
		tag:        opts.tag
	}
	out := sign(c, p, key, opts.label)!
	append_dict_header(mut resp.header, 'Signature-Input', out.signature_input)!
	append_dict_header(mut resp.header, 'Signature', out.signature)!
}

@[params]
pub struct VerifyResponseOptions {
pub:
	label    string
	now_unix i64
}

// verify_response verifies a labelled signature on an HTTP response.
pub fn verify_response(resp http.Response, key Key, opts VerifyResponseOptions) ! {
	c := response_components(resp)
	sig_input := merged_dict_field(resp.header, 'Signature-Input') or {
		return MalformedMessage{
			reason: 'response has no Signature-Input header'
		}
	}
	sig_value := merged_dict_field(resp.header, 'Signature') or {
		return MalformedMessage{
			reason: 'response has no Signature header'
		}
	}
	verify(c, sig_input, sig_value, opts.label, key, now_unix: opts.now_unix)!
}

// append_dict_header merges `addition` into the existing dictionary
// field `name`, separating with ", " per RFC 8941 §3.2 - this keeps
// multiple labelled signatures in a single `Signature-Input` /
// `Signature` field as the spec recommends, even when `add_custom`
// would otherwise create separate field instances.
fn append_dict_header(mut h http.Header, name string, addition string) ! {
	existing := h.get_custom(name) or {
		h.add_custom(name, addition)!
		return
	}
	h.set_custom(name, existing + ', ' + addition)!
}

// merged_dict_field returns the concatenation of all values of `name`
// joined with ", ". HTTP/1.1 §3.2.2 lets a Structured Field appear on
// multiple field-lines; verifiers MUST reassemble them before parsing.
fn merged_dict_field(h http.Header, name string) ?string {
	values := h.custom_values(name)
	if values.len == 0 {
		return none
	}
	return values.join(', ')
}

// request_components extracts the derived-component values from an
// http.Request. The url field is parsed once; if it is not a valid
// URL we surface a typed error rather than silently dropping
// components that depend on it (@scheme, @authority, @path, @query).
fn request_components(req http.Request) !Components {
	mut c := Components{
		method:     req.method.str()
		target_uri: req.url
	}
	parsed := urllib.parse(req.url) or {
		return MalformedMessage{
			reason: 'request url "${req.url}" is not a valid URL: ${err.msg()}'
		}
	}
	authority := if parsed.host != '' {
		parsed.host
	} else {
		req.host
	}
	if authority != '' {
		c.authority = authority
	}
	if parsed.scheme != '' {
		c.scheme = parsed.scheme
	}
	if parsed.path != '' {
		c.path = parsed.path
	} else {
		c.path = '/'
	}
	c.query = if parsed.raw_query != '' { '?' + parsed.raw_query } else { '?' }
	mut request_target := c.path or { '/' }
	if rq := c.query {
		if rq != '?' {
			request_target += rq
		}
	}
	c.request_target = request_target
	for k in req.header.keys() {
		values := req.header.custom_values(k)
		if values.len > 0 {
			c.fields[k.to_lower()] = values
		}
	}
	return c
}

fn response_components(resp http.Response) Components {
	mut c := Components{
		status: resp.status_code
	}
	for k in resp.header.keys() {
		values := resp.header.custom_values(k)
		if values.len > 0 {
			c.fields[k.to_lower()] = values
		}
	}
	return c
}

fn default_request_components(req http.Request) []string {
	mut comps := ['@method', '@target-uri', '@authority']
	if req.header.contains_custom('Date') {
		comps << 'date'
	}
	return comps
}
