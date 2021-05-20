// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

type DownloadFn = fn (written int)

/*
struct DownloadStruct {
mut:
	stream  voidptr
	written int
	cb      DownloadFn
}
*/
fn download_cb(ptr voidptr, size size_t, nmemb size_t, userp voidptr) {
	/*
	mut data := &DownloadStruct(userp)
	written := C.fwrite(ptr, size, nmemb, data.stream)
	data.written += written
	data.cb(data.written)
	//#data->cb(data->written); // TODO
	return written
*/
}

pub fn download_file_with_progress(url string, out string, cb DownloadFn, cb_finished fn()) {
	/*
	curl := C.curl_easy_init()
	if isnil(curl) {
		return
	}
	cout := out.str
	fp := C.fopen(cout, 'wb')
	C.curl_easy_setopt(curl, CURLOPT_URL, url.str)
	C.curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, download_cb)
	data := &DownloadStruct {
		stream:fp
		cb: cb
	}
	C.curl_easy_setopt(curl, CURLOPT_WRITEDATA, data)
	mut d := 0.0
	C.curl_easy_getinfo(curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &d)
	C.curl_easy_perform(curl)
	C.curl_easy_cleanup(curl)
	C.fclose(fp)
	cb_finished()
*/
}

fn empty() {
}
