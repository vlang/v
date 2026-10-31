import crypto.pem

const colon_error = 'crypto.pem: invalid header character `:`'

fn block_with_headers(headers map[string][]string) pem.Block {
	return pem.Block{
		block_type: 'TEST'
		headers:    headers
		data:       'ABC'.bytes()
	}
}

fn encode_error(block pem.Block) string {
	encoded := block.encode() or { return err.msg() }
	return 'no error, encoded:\n${encoded}'
}

fn test_encode_rejects_a_colon_in_a_header_value() {
	assert encode_error(block_with_headers({
		'Comment': ['a:b']
	})) == colon_error
	assert encode_error(block_with_headers({
		'Comment': [':']
	})) == colon_error
	assert encode_error(block_with_headers({
		'Comment': ['trailing:']
	})) == colon_error
}

fn test_encode_rejects_a_colon_in_any_value_of_a_header() {
	assert encode_error(block_with_headers({
		'Recipient-ID-Symmetric': ['linn@zendia.enet.dec.com,ptf-kmc,3', 'pem-dev:tis.com,ptf-kmc,4']
	})) == colon_error
}

fn test_encode_rejects_a_colon_in_a_later_header() {
	assert encode_error(block_with_headers({
		'Proc-Type': ['4,ENCRYPTED']
		'DEK-Info':  ['DES-CBC:F8143EDE5960C597']
	})) == colon_error
}

fn test_encode_rejects_a_colon_in_the_proc_type_value() {
	assert encode_error(block_with_headers({
		'Proc-Type': ['4,ENCRYPTED:x']
	})) == colon_error
}

fn test_encode_rejects_a_colon_in_a_long_wrapped_value() {
	assert encode_error(block_with_headers({
		'Comment': ['x'.repeat(70) + ':tail']
	})) == colon_error
}

fn test_encode_still_rejects_a_colon_in_a_header_key() {
	assert encode_error(block_with_headers({
		'a:b': ['value']
	})) == colon_error
}

fn test_encode_accepts_header_values_without_a_colon() {
	block := block_with_headers({
		'Proc-Type':              ['4,ENCRYPTED']
		'DEK-Info':               ['DES-CBC,F8143EDE5960C597']
		'Recipient-ID-Symmetric': ['linn@zendia.enet.dec.com,ptf-kmc,3', 'pem-dev@tis.com,ptf-kmc,4']
	})
	encoded := block.encode() or {
		assert false, 'encode failed: ${err}'
		return
	}
	assert encoded == '-----BEGIN TEST-----\n' + 'Proc-Type: 4,ENCRYPTED\n' +
		'DEK-Info: DES-CBC,F8143EDE5960C597\n' +
		'Recipient-ID-Symmetric: linn@zendia.enet.dec.com,ptf-kmc,3\n' +
		'Recipient-ID-Symmetric: pem-dev@tis.com,ptf-kmc,4\n' + '\n' + 'QUJD\n' +
		'-----END TEST-----'
}
