// Tests for the small RFC 8941 subset implemented in
// `structured_field.v`. We don't test general SF parsing - only what
// HTTP signatures actually use - and pin the output bytes that
// matter for interop.
module signature

fn test_serialize_inner_list_quotes_each_item() {
	got := serialize_inner_list(['@method', 'date', '@path'])
	assert got == '("@method" "date" "@path")'
}

fn test_serialize_inner_list_handles_empty() {
	assert serialize_inner_list([]string{}) == '()'
}

fn test_serialize_params_keeps_input_order() {
	pairs := [
		ParamPair{
			name:  'created'
			value: i64(1)
		},
		ParamPair{
			name:  'keyid'
			value: 'k1'
		},
	]
	got := serialize_params(pairs)
	assert got == ';created=1;keyid="k1"'
}

fn test_serialize_params_escapes_quotes_in_strings() {
	pairs := [
		ParamPair{
			name:  'tag'
			value: 'has"quote'
		},
	]
	assert serialize_params(pairs) == ';tag="has\\"quote"'
}

fn test_serialize_params_emits_boolean_short_form() {
	pairs := [
		ParamPair{
			name:  'flag'
			value: true
		},
		ParamPair{
			name:  'other'
			value: false
		},
	]
	assert serialize_params(pairs) == ';flag=?1;other=?0'
}

fn test_parse_signature_input_single_entry() {
	src := 'sig1=("@method" "host");created=1618884473;keyid="my-key"'
	entries := parse_signature_input(src)!
	assert entries.len == 1
	e := entries[0]
	assert e.label == 'sig1'
	assert e.components == ['@method', 'host']
	assert e.params['created'] or { ParamValue(i64(0)) } == ParamValue(i64(1618884473))
	assert e.params['keyid'] or { ParamValue('') } == ParamValue('my-key')
}

fn test_parse_signature_input_multiple_entries() {
	src := 'sig-a=("@method");created=1, sig-b=("date" "@authority");keyid="k"'
	entries := parse_signature_input(src)!
	assert entries.len == 2
	assert entries[0].label == 'sig-a'
	assert entries[0].components == ['@method']
	assert entries[1].label == 'sig-b'
	assert entries[1].components == ['date', '@authority']
}

fn test_parse_signature_returns_decoded_bytes() {
	src := 'sig1=:cGF5bG9hZA==:'
	parsed := parse_signature(src)!
	assert parsed.len == 1
	bytes := parsed['sig1'] or { []u8{} }
	assert bytes.bytestr() == 'payload'
}

fn test_parse_signature_input_rejects_uppercase_label() {
	if _ := parse_signature_input('Sig1=()') {
		assert false, 'uppercase label must be rejected by SF parser'
	} else {
		assert err is MalformedMessage
	}
}

fn test_parse_signature_input_preserves_signature_params_value() {
	src := 'sig1=("@method");created=1618884473;keyid="my-key"'
	entries := parse_signature_input(src)!
	// The verifier replays this verbatim into the signature base so
	// it MUST equal the input substring (including the inner list).
	assert entries[0].signature_params_value == '("@method");created=1618884473;keyid="my-key"'
}

fn test_verify_accepts_non_canonical_param_order() {
	// External signers might emit `;keyid=...;created=...` (keyid
	// before created). Our re-canonicalised order is the opposite, so
	// without the verbatim replay the bases would differ. Use HMAC so
	// we can synthesise a wire signature ourselves.
	c := Components{
		method: 'POST'
	}
	key := Key.hmac_sha256('shared-secret'.bytes())
	// Hand-build the base with keyid first, sign it, then verify
	// using the same wire order.
	base := '"@method": POST\n"@signature-params": ("@method");keyid="k1";created=42'
	sig := sign_base(base.bytes(), key)!
	sig_input := 'sig1=("@method");keyid="k1";created=42'
	sig_header := signature_header_value('sig1', sig)
	verify(c, sig_input, sig_header, 'sig1', key)!
}

fn test_signature_base_string_rejects_duplicate_components() {
	c := Components{
		method: 'GET'
		fields: {
			'host': ['example.com']
		}
	}
	p := SignatureParams{
		components: ['@method', '@method']
	}
	if _ := signature_base_string(c, p) {
		assert false, 'duplicate components must error'
	} else {
		assert err is MalformedMessage
		assert err.msg().contains('duplicate')
	}
}

fn test_signature_base_string_errors_on_missing_field() {
	c := Components{
		method: 'GET'
	}
	p := SignatureParams{
		components: ['@method', 'date']
	}
	if _ := signature_base_string(c, p) {
		assert false, 'missing component must error'
	} else {
		assert err is MalformedMessage
		assert err.msg().contains('"date"')
	}
}

fn test_components_lowercases_field_names_and_adds_in_order() {
	mut c := Components{}
	c.add_field('Accept', 'text/html')
	c.add_field('accept', 'application/json')
	values := c.fields['accept']
	assert values.len == 2
	assert values[0] == 'text/html'
	assert values[1] == 'application/json'
}

fn test_field_value_joins_multi_value_with_comma_space() {
	c := Components{
		fields: {
			'accept': ['text/html', 'application/json']
		}
	}
	p := SignatureParams{
		components: ['accept']
	}
	got := signature_base_string(c, p)!
	assert got.starts_with('"accept": text/html, application/json\n')
}

fn test_field_value_trims_ows() {
	c := Components{
		fields: {
			'foo': ['  hello   ', '\tworld\t']
		}
	}
	p := SignatureParams{
		components: ['foo']
	}
	got := signature_base_string(c, p)!
	assert got.starts_with('"foo": hello, world\n')
}

fn test_query_with_leading_question_mark_is_preserved() {
	c := Components{
		query: '?a=1'
	}
	p := SignatureParams{
		components: ['@query']
	}
	got := signature_base_string(c, p)!
	assert got.starts_with('"@query": ?a=1\n')
}

fn test_empty_query_emits_single_question_mark() {
	c := Components{
		query: ''
	}
	p := SignatureParams{
		components: ['@query']
	}
	got := signature_base_string(c, p)!
	// RFC 9421 §2.2.7: empty query is the single character "?".
	assert got.starts_with('"@query": ?\n')
}

fn test_authority_is_lowercased() {
	c := Components{
		authority: 'EXAMPLE.com'
	}
	p := SignatureParams{
		components: ['@authority']
	}
	got := signature_base_string(c, p)!
	assert got.starts_with('"@authority": example.com\n')
}
