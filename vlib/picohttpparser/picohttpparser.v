// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module picohttpparser

#flag -I @VEXEROOT/thirdparty/picohttpparser
#flag -L @VEXEROOT/thirdparty/picohttpparser
#flag @VEXEROOT/thirdparty/picohttpparser/picohttpparser.o

#include "picohttpparser.h"

struct C.phr_header {
pub:
	name      &char
	name_len  int
	value     &char
	value_len int
}

type PPchar = &&char

struct C.phr_header_t {}

fn C.phr_parse_request(buf &char, len size_t, method PPchar, method_len &size_t, path PPchar, path_len &size_t, minor_version &int, headers &C.phr_header, num_headers &size_t, last_len size_t) int

fn C.phr_parse_response(buf &char, len size_t, minor_version &int, status &int, msg PPchar, msg_len &size_t, headers &C.phr_header, num_headers &size_t, last_len size_t) int

fn C.phr_parse_headers(buf &char, len size_t, headers &C.phr_header, num_headers &size_t, last_len size_t) int

fn C.phr_parse_request_path(buf_start &char, len size_t, method PPchar, method_len &size_t, path PPchar, path_len &size_t) int
fn C.phr_parse_request_path_pipeline(buf_start &char, len size_t, method PPchar, method_len &size_t, path PPchar, path_len &size_t) int
fn C.get_date() &byte

// char * u64toa(uint64_t value, char *buffer)
fn C.u64toa(buffer &char, value u64) &char
