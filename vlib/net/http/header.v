// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// Re-exports from net.http.common for backward compatibility.
// All types and functions are delegated to net.http.common.
import net.http.common

pub type Header = common.Header
pub type CommonHeader = common.CommonHeader
pub type HeaderConfig = common.HeaderConfig
pub type HeaderQueryConfig = common.HeaderQueryConfig
pub type HeaderRenderConfig = common.HeaderRenderConfig

pub const max_headers = common.max_headers

pub fn new_header(kvs ...common.HeaderConfig) common.Header {
	return common.new_header(...kvs)
}

pub fn new_header_from_map(kvs map[common.CommonHeader]string) common.Header {
	return common.new_header_from_map(kvs)
}

pub fn new_custom_header_from_map(kvs map[string]string) !common.Header {
	return common.new_custom_header_from_map(kvs)
}

// from_map creates a Header from a map[string]string.
pub fn from_map(m map[string]string) common.Header {
	return common.from_map(m)
}

fn parse_headers(s string) !common.Header {
	return common.parse_headers(s)
}

fn parse_header_fast(s string) !int {
	return common.parse_header_fast(s)
}
