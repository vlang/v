module transform

import os

fn (t &Transformer) vmod_root() string {
	mut dir := if t.cur_file.len > 0 { os.dir(t.cur_file) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	for {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir || parent.len == 0 {
			return if t.cur_file.len > 0 { os.dir(t.cur_file) } else { os.getwd() }
		}
		dir = parent
	}
	return dir
}
