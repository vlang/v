// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import os

pub fn download_file(url, out string) bool {
	$if debug_http? {
		println('download file url=$url out=$out')
	}
	s := get(url) or {
		return false
	}
	os.write_file(out, s.text)
	return true
	// download_file_with_progress(url, out, empty, empty)
}
