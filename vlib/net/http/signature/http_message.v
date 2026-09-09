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
	// scheme is used to reconstruct `@target-uri` and `@scheme` when
	// `req.url` is origin-form (e.g. `/foo`, as produced by
	// `http.parse_request*`). The default matches the common signing
	// scenario (TLS-protected APIs). Ignored when `req.url` already
	// carries a scheme.
	scheme string = 'https'
}

// sign_request signs an HTTP request in place by appending the
// `Signature-Input` and `Signature` header fields. Existing values of
// these headers are preserved (RFC 9421 §4.3 - multiple signatures
// can coexist), so calling this twice with different labels yields
// two co-existing signatures. Automatic redirects are disabled because
// the signature only covers this request target and method.
//
// `created` defaults to `time.now().unix()` when omitted, since
// RFC 9421 §7.2.1 RECOMMENDS the parameter for replay protection.
// Pass an explicit `created: 0` only if you know you don't want it.
pub fn sign_request(mut req http.Request, key Key, opts SignRequestOptions) ! {
	ensure_signature_label_available(req.header, opts.label)!
	c := request_components(req, opts.scheme, .outgoing)!
	mut comps := opts.components.clone()
	if comps.len == 0 {
		comps = default_request_components(req)
	}
	validate_stable_signature_components(comps)!
	validate_request_component_coverage(req, comps, opts.scheme)!
	ensure_signature_header_capacity(req.header, 0)!
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
	req.allow_redirect = false
}

// VerifyRequestOptions parametrises `verify_request`. `label` selects
// which labelled signature to verify when several are present.
@[params]
pub struct VerifyRequestOptions {
pub:
	label    string
	now_unix i64
	// scheme — see SignRequestOptions.scheme. Both ends of the
	// signature must agree on the scheme used to reconstruct the
	// target URI, otherwise the signature bases differ.
	scheme string = 'https'
}

// verify_request verifies a labelled signature on an HTTP request. If
// `opts.label` is empty and exactly one signature is present, that
// one is checked. If `opts.now_unix > 0`, the `expires` parameter is
// also enforced.
pub fn verify_request(req http.Request, key Key, opts VerifyRequestOptions) ! {
	c := request_components(req, opts.scheme, .incoming)!
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
	ensure_signature_label_available(resp.header, opts.label)!
	mut comps := opts.components.clone()
	if comps.len == 0 {
		comps = ['@status']
	}
	validate_stable_signature_components(comps)!
	validate_response_component_coverage(comps)!
	adds_content_length := comps.any(it.to_lower() == 'content-length')
		&& !resp.header.contains(.content_length)
	ensure_signature_header_capacity(resp.header, if adds_content_length { 1 } else { 0 })!
	if adds_content_length {
		// Insert the field so both HTTP/1.x and HTTP/2 actually transmit the
		// value covered by the signature.
		resp.header.set(.content_length, resp.body.len.str())
	}
	c := response_components(resp)
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
	existing_values := h.custom_values(name)
	if existing_values.len == 0 {
		h.add_custom(name, addition)!
		return
	}
	// Collapse all case-insensitive field lines before appending. Keeping the
	// first spelling lets Header.set_custom reuse that entry after duplicates
	// have been cleared instead of consuming another fixed-capacity slot.
	mut matching_keys := []string{}
	for key in h.keys() {
		if key.to_lower() == name.to_lower() {
			matching_keys << key
		}
	}
	for key in matching_keys {
		h.delete_custom(key)
	}
	key := if matching_keys.len > 0 { matching_keys[0] } else { name }
	h.set_custom(key, existing_values.join(', ') + ', ' + addition)!
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

fn ensure_signature_label_available(h http.Header, label string) ! {
	check_label(label)!
	if input := merged_dict_field(h, 'Signature-Input') {
		entries := parse_signature_input(input)!
		if entries.any(it.label == label) {
			return MalformedMessage{
				reason: 'signature label "${label}" already exists in Signature-Input'
			}
		}
	}
	if signatures := merged_dict_field(h, 'Signature') {
		parsed := parse_signature(signatures)!
		if label in parsed {
			return MalformedMessage{
				reason: 'signature label "${label}" already exists in Signature'
			}
		}
	}
}

fn ensure_signature_header_capacity(h http.Header, extra_slots int) ! {
	input_count := h.custom_values('Signature-Input').len
	signature_count := h.custom_values('Signature').len
	mut current_slots := 0
	for key in h.keys() {
		current_slots += h.custom_values(key, exact: true).len
	}
	// Each signature field is collapsed to one slot when appending. Account for
	// those replacements as well as any earlier mutation sign_response needs.
	future_slots := current_slots - input_count - signature_count + 2 + extra_slots
	if future_slots > http.max_headers {
		return MalformedMessage{
			reason: 'signing requires ${future_slots} header slots, but http.Header supports ${http.max_headers}'
		}
	}
}

fn validate_stable_signature_components(components []string) ! {
	for component in components {
		name := component.to_lower()
		if name in ['signature-input', 'signature'] {
			return MalformedMessage{
				reason: 'covered field "${name}" changes when the signature fields are appended'
			}
		}
	}
}

fn validate_request_component_coverage(req http.Request, components []string, default_scheme string) ! {
	if req.disable_connection_reuse && components.any(it.to_lower() == 'connection') {
		return MalformedMessage{
			reason: 'covered field "connection" changes when connection reuse is disabled'
		}
	}
	if !req.enable_http2 {
		return
	}
	parsed := urllib.parse(req.url) or {
		return MalformedMessage{
			reason: 'request url "${req.url}" is not a valid URL: ${err.msg()}'
		}
	}
	scheme := if parsed.scheme != '' { parsed.scheme } else { default_scheme }
	if scheme != 'https' {
		return
	}
	for component in components {
		name := component.to_lower()
		if name == 'host' {
			return MalformedMessage{
				reason: 'covered field "host" is replaced by "@authority" during HTTP/2 negotiation'
			}
		}
		if name in ['connection', 'keep-alive', 'proxy-connection', 'transfer-encoding', 'upgrade'] {
			return MalformedMessage{
				reason: 'covered field "${name}" may be removed during HTTP/2 negotiation'
			}
		}
		if name == 'te'
			&& req.header.custom_values('TE').any(it.trim_space().to_lower() != 'trailers') {
			return MalformedMessage{
				reason: 'covered field "te" contains a value that may be removed during HTTP/2 negotiation'
			}
		}
	}
}

fn validate_response_component_coverage(components []string) ! {
	for component in components {
		name := component.to_lower()
		if name in ['connection', 'keep-alive', 'proxy-connection', 'transfer-encoding', 'upgrade'] {
			return MalformedMessage{
				reason: 'covered response field "${name}" may be removed by HTTP/2 transport'
			}
		}
	}
}

enum RequestComponentsMode {
	outgoing
	incoming
}

// request_components extracts the derived-component values from an
// http.Request. Outgoing values match net.http's request-line and header
// serialization, while incoming values preserve the parsed request target.
// `default_scheme` reconstructs `@target-uri` for origin-form input.
fn request_components(req http.Request, default_scheme string, mode RequestComponentsMode) !Components {
	is_authority_form := mode == .incoming && req.method == .connect
	is_asterisk_form := mode == .incoming && req.method == .options && req.url == '*'
	parse_target := if is_authority_form || is_asterisk_form { '/' } else { req.url }
	parsed := urllib.parse(parse_target) or {
		return MalformedMessage{
			reason: 'request url "${req.url}" is not a valid URL: ${err.msg()}'
		}
	}
	uses_absolute_form := mode == .outgoing && !isnil(req.proxy) && parsed.scheme == 'http'
	mut authority := if is_authority_form {
		req.url
	} else if parsed.host != '' {
		parsed.host
	} else {
		req.host
	}
	if mode == .outgoing {
		if parsed.host != '' {
			authority = transport_authority(parsed)
		}
		if !uses_absolute_form {
			if host := req.header.get(.host) {
				if host != '' {
					authority = host.trim_space()
				}
			}
		}
	}
	scheme := if !is_authority_form && !is_asterisk_form && parsed.scheme != '' {
		parsed.scheme
	} else {
		default_scheme
	}
	mut c := Components{
		method: req.method.str()
	}
	is_origin_form := req.url.starts_with('/')
	escaped_path := parsed.escaped_path()
	incoming_path := if is_authority_form || is_asterisk_form {
		'/'
	} else if escaped_path != '' {
		escaped_path
	} else {
		'/'
	}
	// Keep this in sync with Request.method_and_url_to_response: it removes
	// duplicate leading slashes and re-encodes query values before sending.
	transport_path := '/' + escaped_path.trim_left('/')
	transport_query := parsed.query().encode()
	origin_target := if transport_query != '' {
		'${transport_path}?${transport_query}'
	} else {
		transport_path
	}
	request_target := if mode == .outgoing {
		if uses_absolute_form {
			'${scheme}://${transport_authority(parsed)}${origin_target}'
		} else {
			origin_target
		}
	} else {
		req.url
	}
	c.target_uri = if mode == .outgoing && parsed.host != '' {
		'${scheme}://${authority}${origin_target}'
	} else if (is_authority_form || is_asterisk_form) && authority != '' && scheme != '' {
		'${scheme}://${authority}'
	} else if is_origin_form && authority != '' && scheme != '' {
		'${scheme}://${authority}${req.url}'
	} else {
		req.url
	}
	if authority != '' {
		c.authority = authority
	}
	if scheme != '' {
		c.scheme = scheme
	}
	c.path = if mode == .outgoing { transport_path } else { incoming_path }
	query := if mode == .outgoing { transport_query } else { parsed.raw_query }
	c.query = if query != '' { '?' + query } else { '?' }
	c.request_target = request_target
	for k in req.header.unique_keys() {
		values := req.header.custom_values(k)
		if values.len > 0 {
			c.fields[k.to_lower()] = values
		}
	}
	if mode == .incoming && req.version == .v2_0 {
		cookie_values := req.header.custom_values('Cookie')
		if cookie_values.len > 0 {
			c.fields['cookie'] = [cookie_values.map(it.trim_space()).join('; ')]
		}
	}
	if mode == .outgoing {
		if !req.header.contains(.host) {
			c.fields['host'] = [transport_authority(parsed)]
		}
		if !req.header.contains(.user_agent) {
			c.fields['user-agent'] = [req.user_agent]
		}
		if !req.header.contains(.content_length) {
			c.fields['content-length'] = [req.data.len.str()]
		}
		cookie_value := req.cookie_header_value()
		if cookie_value != '' {
			c.fields['cookie'] = [cookie_value]
		}
	}
	return c
}

fn transport_authority(url urllib.URL) string {
	hostname := url.hostname()
	port := url.port().int()
	if port == 0 || (url.scheme == 'http' && port == 80) || (url.scheme == 'https' && port == 443) {
		return hostname
	}
	return '${hostname}:${port}'
}

fn response_components(resp http.Response) Components {
	wire_status := if resp.status_code >= 100 && resp.status_code <= 599 {
		resp.status_code
	} else if resp.status_code == 0 {
		200
	} else {
		500
	}
	mut c := Components{
		status: wire_status
	}
	for k in resp.header.unique_keys() {
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
