module embed_file

import os

fn reload_from_file_at_runtime(mut ed EmbedFileData) {
	mut path := os.resource_abs_path(ed.path)
	if !os.is_file(path) {
		path = ed.apath
		if !os.is_file(path) {
			panic('EmbedFileData error: files "${ed.path}" and "${ed.apath}" do not exist')
		}
	}
	bytes := os.read_bytes(path) or {
		panic('EmbedFileData error: "${path}" could not be read: ${err}')
	}
	ed.uncompressed = bytes.data
	ed.free_uncompressed = true
}
