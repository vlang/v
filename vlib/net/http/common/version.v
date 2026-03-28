module common

// Version enumerates supported HTTP protocol versions.
pub enum Version {
	unknown
	v1_1
	v2_0
	v3_0
	v1_0
}

pub fn (v Version) str() string {
	return match v {
		.v1_1 { 'HTTP/1.1' }
		.v2_0 { 'HTTP/2.0' }
		.v3_0 { 'HTTP/3.0' }
		.v1_0 { 'HTTP/1.0' }
		.unknown { 'unknown' }
	}
}

pub fn version_from_str(v string) Version {
	return match v.to_lower() {
		'http/1.1' { Version.v1_1 }
		'http/2.0', 'http/2' { Version.v2_0 }
		'http/3.0', 'http/3' { Version.v3_0 }
		'http/1.0' { Version.v1_0 }
		else { Version.unknown }
	}
}

pub fn (v Version) protos() (int, int) {
	match v {
		.v1_1 { return 1, 1 }
		.v2_0 { return 2, 0 }
		.v3_0 { return 3, 0 }
		.v1_0 { return 1, 0 }
		.unknown { return 0, 0 }
	}
}

pub fn (v Version) alpn_proto() string {
	return match v {
		.v1_1 { 'http/1.1' }
		.v2_0 { 'h2' }
		.v3_0 { 'h3' }
		.v1_0 { 'http/1.0' }
		.unknown { '' }
	}
}

pub fn version_from_alpn(proto string) Version {
	return match proto {
		'h2' { Version.v2_0 }
		'h3' { Version.v3_0 }
		'http/1.1' { Version.v1_1 }
		'http/1.0' { Version.v1_0 }
		else { Version.unknown }
	}
}
