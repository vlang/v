// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import os

// download_file retrieves a document from the URL `url`,
// and saves it in the output file path `out_file_path`.
pub fn download_file(url string, out_file_path string) ? {
	$if debug_http ? {
		println('http.download_file url=$url out_file_path=$out_file_path')
	}
	s := get(url) or { return err }
	if s.status() != .ok {
		return error('received http code $s.status_code')
	}
	$if debug_http ? {
		println('http.download_file saving $s.body.len bytes')
	}
	os.write_file(out_file_path, s.body)?
}

// TODO: implement download_file_with_progress
// type DownloadChunkFn = fn (written int)
// type DownloadFinishedFn = fn ()
// pub fn download_file_with_progress(url string, out_file_path string, cb_chunk DownloadChunkFn, cb_finished DownloadFinishedFn)
