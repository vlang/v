module http

// HTTP protocol version definitions and conversions.

// Version enumerates supported HTTP protocol versions.
pub enum Version {
	unknown
	v1_1
	v2_0
	v3_0
	v1_0
}

// str returns the standard HTTP version string.
pub fn (v Version) str() string {
	return match v {
		.v1_1 { 'HTTP/1.1' }
		.v2_0 { 'HTTP/2.0' }
		.v3_0 { 'HTTP/3.0' }
		.v1_0 { 'HTTP/1.0' }
		.unknown { 'unknown' }
	}
}

// version_from_str parses an HTTP version string into a Version.
pub fn version_from_str(v string) Version {
	return match v.to_lower() {
		'http/1.1' { Version.v1_1 }
		'http/2.0', 'http/2' { Version.v2_0 }
		'http/3.0', 'http/3' { Version.v3_0 }
		'http/1.0' { Version.v1_0 }
		else { Version.unknown }
	}
}

// protos returns the version major and minor numbers.
pub fn (v Version) protos() (int, int) {
	match v {
		.v1_1 { return 1, 1 }
		.v2_0 { return 2, 0 }
		.v3_0 { return 3, 0 }
		.v1_0 { return 1, 0 }
		.unknown { return 0, 0 }
	}
}

// alpn_proto returns the ALPN protocol identifier for this version.
pub fn (v Version) alpn_proto() string {
	return match v {
		.v1_1 { 'http/1.1' }
		.v2_0 { 'h2' }
		.v3_0 { 'h3' }
		.v1_0 { 'http/1.0' }
		.unknown { '' }
	}
}

// version_from_alpn converts an ALPN protocol identifier to a Version.
pub fn version_from_alpn(proto string) Version {
	return match proto {
		'h2' { Version.v2_0 }
		'h3' { Version.v3_0 }
		'http/1.1' { Version.v1_1 }
		'http/1.0' { Version.v1_0 }
		else { Version.unknown }
	}
}
