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
struct AddInfoConfig {
pub:
	use_threshold bool
	threshold     int = 25
}

fn (mut infos []AlignInfo) add_new_info(len int, line int) {
	infos << AlignInfo{
		line_nr: line
		max_len: len
	}
}

@[direct_array_access]
fn (mut infos []AlignInfo) add_info(len int, line int, cfg AddInfoConfig) {
	if infos.len == 0 {
		infos.add_new_info(len, line)
		return
	}
	i := infos.len - 1
	if line - infos[i].line_nr > 1 {
		infos.add_new_info(len, line)
		return
	}
	if cfg.use_threshold {
		len_diff := if infos[i].max_len >= len {
			infos[i].max_len - len
		} else {
			len - infos[i].max_len
		}

		if len_diff >= cfg.threshold {
			infos.add_new_info(len, line)
			return
		}
	}
	infos[i].line_nr = line
	if len > infos[i].max_len {
		infos[i].max_len = len
	}
}
