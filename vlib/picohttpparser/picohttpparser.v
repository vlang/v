// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module picohttpparser

#flag -I @VROOT/thirdparty/picohttpparser
#flag -L @VROOT/thirdparty/picohttpparser
#flag @VROOT/thirdparty/picohttpparser/picohttpparser.o

#include "picohttpparser.h"

struct C.phr_header {
pub:
	name charptr
	name_len int
	value charptr
	value_len int
}
struct C.phr_header_t {}

fn C.phr_parse_request() int
fn C.phr_parse_response() int
fn C.phr_parse_headers() int

fn C.phr_parse_request_path() int
fn C.phr_parse_request_path_pipeline() int
fn C.get_date() byteptr
fn C.u64toa() int
