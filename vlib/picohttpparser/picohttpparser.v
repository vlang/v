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

fn C.phr_parse_request(buf &char, len usize, method PPchar, method_len &usize, path PPchar, path_len &usize, minor_version &int, headers &C.phr_header, num_headers &usize, last_len usize) int

fn C.phr_parse_response(buf &char, len usize, minor_version &int, status &int, msg PPchar, msg_len &usize, headers &C.phr_header, num_headers &usize, last_len usize) int

fn C.phr_parse_headers(buf &char, len usize, headers &C.phr_header, num_headers &usize, last_len usize) int

fn C.phr_parse_request_path(buf_start &char, len usize, method PPchar, method_len &usize, path PPchar, path_len &usize) int
fn C.phr_parse_request_path_pipeline(buf_start &char, len usize, method PPchar, method_len &usize, path PPchar, path_len &usize) int
fn C.get_date() &char

// static inline int u64toa(char* buf, uint64_t value) {
fn C.u64toa(buffer &char, value u64) int
