// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

struct AlignInfo {
mut:
	line_nr int
	max_len int
}

@[params]
struct AlignConfig {
pub:
	ignore_newline bool // ignore newline or comment
	use_break_line bool
	use_threshold  bool
	threshold      int = 25
}

struct FieldAlign {
	cfg AlignConfig
mut:
	infos   []AlignInfo
	cur_idx int
}

fn new_field_align(cfg AlignConfig) FieldAlign {
	return FieldAlign{
		cfg: cfg
	}
}

fn (mut fa FieldAlign) add_new_info(len int, line int) {
	fa.infos << AlignInfo{
		line_nr: line
		max_len: len
	}
}

@[direct_array_access]
fn (mut fa FieldAlign) add_info(len int, line int, has_break_line bool) {
	if fa.infos.len == 0 {
		fa.add_new_info(len, line)
		return
	}
	i := fa.infos.len - 1
	if !fa.cfg.ignore_newline && ((fa.cfg.use_break_line && has_break_line)
		|| (!fa.cfg.use_break_line && line - fa.infos[i].line_nr > 1)) {
		fa.add_new_info(len, line)
		return
	}
	if fa.cfg.use_threshold {
		len_diff := if fa.infos[i].max_len >= len {
			fa.infos[i].max_len - len
		} else {
			len - fa.infos[i].max_len
		}

		if len_diff >= fa.cfg.threshold {
			fa.add_new_info(len, line)
			return
		}
	}
	fa.infos[i].line_nr = line
	if len > fa.infos[i].max_len {
		fa.infos[i].max_len = len
	}
}

fn (mut fa FieldAlign) max_len(line_nr int) int {
	if fa.cur_idx < fa.infos.len && fa.infos[fa.cur_idx].line_nr < line_nr {
		fa.cur_idx++
	}
	if fa.cur_idx < fa.infos.len {
		return fa.infos[fa.cur_idx].max_len
	}
	return 0
}
