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
	name      charptr
	name_len  int
	value     charptr
	value_len int
}

struct C.phr_header_t {}

fn C.phr_parse_request(buf charptr, len size_t, method &charptr, method_len &size_t, path &charptr, path_len &size_t, minor_version &int, headers &C.phr_header, num_headers &size_t, last_len size_t) int

fn C.phr_parse_response(buf charptr, len size_t, minor_version &int, status &int, msg &charptr, msg_len &size_t, headers &C.phr_header, num_headers &size_t, last_len size_t) int

fn C.phr_parse_headers(buf charptr, len size_t, headers &C.phr_header, num_headers &size_t, last_len size_t) int

fn C.phr_parse_request_path(buf_start charptr, len size_t, method &charptr, method_len &size_t, path &charptr, path_len &size_t) int
fn C.phr_parse_request_path_pipeline(buf_start charptr, len size_t, method &charptr, method_len &size_t, path &charptr, path_len &size_t) int
fn C.get_date() byteptr
// char * u64toa(uint64_t value, char *buffer)
fn C.u64toa(value u64, buffer charptr) charptr
