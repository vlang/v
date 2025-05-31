// Implements PEM (Privacy Enhanced Mail) data encoding
// according to RFC 1421
module pem

const pem_begin = '-----BEGIN '
const pem_end = '\n-----END '
const pem_eol = '-----'
const colon = ':'

@[params]
pub struct EncodeConfig {
pub mut:
	// inner text wrap around
	line_length int = 64
	// line ending (alternatively '\r\n')
	line_ending string = '\n'
}

// Headers as described in RFC 1421 Section 9
pub enum Header {
	proctype
	contentdomain
	dekinfo
	origid_asymm
	origid_symm
	recipid_asymm
	recipid_symm
	cert
	issuercert
	micinfo
	keyinfo
	crl
}

// str returns the string representation of the header
pub fn (header Header) str() string {
	return match header {
		.proctype { 'Proc-Type' }
		.contentdomain { 'Content-Domain' }
		.dekinfo { 'DEK-Info' }
		.origid_asymm { 'Originator-ID-Asymmetric' }
		.origid_symm { 'Originator-ID-Symmetric' }
		.recipid_asymm { 'Recipient-ID-Asymmetric' }
		.recipid_symm { 'Recipient-ID-Symmetric' }
		.cert { 'Originator-Certificate' }
		.issuercert { 'Issuer-Certificate' }
		.micinfo { 'MIC-Info' }
		.keyinfo { 'Key-Info' }
		.crl { 'CRL' }
	}
}

pub struct Block {
pub mut:
	// from preamble
	block_type string
	// optional headers
	headers map[string][]string
	// decoded contents
	data []u8
}

// Block.new returns a new `Block` with the specified block_type
@[inline]
pub fn Block.new(block_type string) Block {
	return Block{
		block_type: block_type
	}
}

// free the resources taken by the Block `block`
@[unsafe]
pub fn (mut block Block) free() {
	$if prealloc {
		return
	}
	unsafe {
		block.block_type.free()
		block.headers.free()
		block.data.free()
	}
}

// header_by_key returns the selected key using the Header enum
//
// same as `block.headers[key.str()]`
@[inline]
pub fn (block Block) header_by_key(key Header) []string {
	return block.headers[key.str()]
}
