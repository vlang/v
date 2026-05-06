// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// Re-exports from net.http.common for backward compatibility.
import net.http.common

pub type Version = common.Version

pub fn version_from_str(v string) common.Version {
	return common.version_from_str(v)
}

pub fn version_from_alpn(proto string) common.Version {
	return common.version_from_alpn(proto)
}
