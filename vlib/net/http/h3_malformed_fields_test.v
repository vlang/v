// vtest vflags: -d http3
module http

import net.quic

// `h3_response_to_http` copies the fields of an HTTP/3 response, trailers
// included, into an `http.Header`. `Header.add_custom` refuses a field past
// `max_headers`, and a name that is not a token; the conversion used to
// ignore that and return a Response without the field. H3MuxConn already
// rejects an invalid name or value when it reads the response (RFC 9114
// section 4.2), so the field limit is what a response from the network
// can still reach here.

// h3_numbered_fields returns `n` fields `x-f0` .. `x-f<n-1>`, each with its
// number as the value.
fn h3_numbered_fields(n int) []quic.QpackFieldLine {
	return []quic.QpackFieldLine{len: n, init: quic.QpackFieldLine{
		name:  'x-f${index}'
		value: index.str()
	}}
}

fn test_h3_response_that_fills_the_header_is_delivered() {
	fields := h3_numbered_fields(max_headers)
	resp := h3_response_to_http(H3ClientResponse{
		status:  200
		headers: fields
		body:    'hi'.bytes()
	})!
	assert resp.status_code == 200
	assert resp.body == 'hi'
	assert resp.header.keys().len == max_headers
	for f in fields {
		assert resp.header.custom_values(f.name) == [f.value]
	}
}

fn test_h3_response_with_more_fields_than_a_header_holds_is_an_error() {
	if resp := h3_response_to_http(H3ClientResponse{
		status:  200
		headers: h3_numbered_fields(max_headers + 1)
	})
	{
		assert false, 'got a response with the fields ${resp.header.keys()}'
	} else {
		assert err is HeaderLimitError, err.msg()
	}
}

fn test_h3_response_with_a_field_that_the_header_refuses_is_an_error() {
	if resp := h3_response_to_http(H3ClientResponse{
		status:  200
		headers: [
			quic.QpackFieldLine{
				name:  'content-type'
				value: 'text/plain'
			},
			quic.QpackFieldLine{
				name:  'x bad'
				value: 'v'
			},
		]
	})
	{
		assert false, 'got a response with the fields ${resp.header.keys()}'
	} else {
		assert err.msg().starts_with('h3: malformed response: '), err.msg()
	}
}
