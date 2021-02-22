// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// The versions listed here are the most common ones.
pub enum Version {
	unknown
	v1_1
	v2_0
	v1_0
}

pub fn (v Version) str() string {
	return match v {
		.v1_1 { 'HTTP/1.1' }
		.v2_0 { 'HTTP/2.0' }
		.v1_0 { 'HTTP/1.0' }
		.unknown { 'unknown' }
	}
}

pub fn version_from_str(v string) Version {
	return match v.to_lower() {
		'http/1.1' { Version.v1_1 }
		'http/2.0' { Version.v2_0 }
		'http/1.0' { Version.v1_0 }
		else { Version.unknown }
	}
}
