module pico_http_parser

import picohttpparser

pub type Header = picohttpparser.Header

pub type Request = picohttpparser.Request

pub type Response = picohttpparser.Response

// u64toa forwards to `picohttpparser.u64toa` for backwards-compatible imports.
@[unsafe]
pub fn u64toa(buf_start &u8, value u64) !int {
	return picohttpparser.u64toa(buf_start, value)
}
