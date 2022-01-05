// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import v.mathutil

const struct_field_align_threshold = 8

struct AlignInfo {
mut:
	line_nr      int
	max_len      int
	max_type_len int
}

[params]
struct AddInfoConfig {
	use_threshold bool
}

fn (mut infos []AlignInfo) add_new_info(len int, type_len int, line int) {
	infos << AlignInfo{
		line_nr: line
		max_len: len
		max_type_len: type_len
	}
}

[direct_array_access]
fn (mut infos []AlignInfo) add_info(len int, type_len int, line int, cfg AddInfoConfig) {
	if infos.len == 0 {
		infos.add_new_info(len, type_len, line)
		return
	}
	i := infos.len - 1
	if line - infos[i].line_nr > 1 {
		infos.add_new_info(len, type_len, line)
		return
	}
	if cfg.use_threshold {
		len_diff := mathutil.abs(infos[i].max_len - len) +
			mathutil.abs(infos[i].max_type_len - type_len)

		if len_diff >= fmt.struct_field_align_threshold {
			infos.add_new_info(len, type_len, line)
			return
		}
	}
	infos[i].line_nr = line
	if len > infos[i].max_len {
		infos[i].max_len = len
	}
	if type_len > infos[i].max_type_len {
		infos[i].max_type_len = type_len
	}
}
