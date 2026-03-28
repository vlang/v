// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// Re-exports from net.http.common for backward compatibility.
import net.http.common

pub type Method = common.Method

pub fn method_from_str(m string) common.Method {
	return common.method_from_str(m)
}

pub fn method_from_str_known(m string) ?common.Method {
	return common.method_from_str_known(m)
}
