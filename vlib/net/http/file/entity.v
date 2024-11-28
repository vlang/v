module file

import os
import time

pub struct Entity {
	os.FileInfo
	path     string
	mod_time time.Time
	url      string
	fname    string
}

fn path_to_entity(path string, uri_path string) Entity {
	pinfo := os.inode(path)
	mut uri_base := ''
	if uri_path.len > 0 {
		uri_base = '/${uri_path}'
	}
	fname := os.file_name(path)
	return Entity{
		FileInfo: pinfo
		path:     path
		mod_time: time.unix(pinfo.mtime)
		url:      '${uri_base}/${fname}'
		fname:    fname
	}
}
