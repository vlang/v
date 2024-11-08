module asn1

// Unified error handling
//
enum ErrorKind {
	// vfmt off
	unexpected_tag_value 	= 0
	unexpected_length_value	= 1
	unexpected_limit_exceed	= 2
	unexpected_bytes_data	= 3
	unexpected_bytes_offset	= 4
	unsupported_rule		= 5
	unsupported_format		= 6
	unexpected_value		= 7
	unmeet_requirement		= 8
	unallowed_operation		= 9
	// vfmt on
}

fn (ek ErrorKind) str() string {
	match ek {
		.unexpected_tag_value { return 'unexpected_tag_value' }
		.unexpected_length_value { return 'unexpected_length_value' }
		.unexpected_limit_exceed { return 'unexpected_limit_exceed' }
		.unexpected_bytes_data { return 'unexpected_bytes_data' }
		.unexpected_bytes_offset { return 'unexpected_bytes_offset' }
		.unsupported_rule { return 'unsupported_rule' }
		.unsupported_format { return 'unsupported_format' }
		.unexpected_value { return 'unexpected_value' }
		.unmeet_requirement { return 'unmeet_requirement' }
		.unallowed_operation { return 'unallowed_operation' }
	}
}

struct Asn1Error {
	Error
	kind ErrorKind
	msg  string
}

fn (er Asn1Error) msg() string {
	return 'Error: ${er.kind.str()} with error ${er.msg}'
}

fn asn1_error(kind ErrorKind, msg string) !Asn1Error {
	return Asn1Error{
		kind: kind
		msg:  msg
	}
}
