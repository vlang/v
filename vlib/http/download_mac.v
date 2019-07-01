// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

import os

type downloadfn fn (written int)
type download_finished_fn fn () 

struct DownloadStruct {
mut: 
	stream  voidptr
	written int
	cb      downloadfn
}

fn download_cb(ptr voidptr, size, nmemb size_t, userp voidptr) int {
	mut data := &DownloadStruct(userp)
	written := C.fwrite(ptr, size, nmemb, data.stream) 
	data.written += written 
	#data->cb(data->written); // TODO 
	return written 
}

fn download_file_with_progress(url, out string, cb downloadfn, cb_finished download_finished_fn) {   
	curl := C.curl_easy_init()
	if isnil(curl) {
		return
	}
	cout := out.cstr() 
	fp := C.fopen(cout, 'wb') 
	C.curl_easy_setopt(curl, CURLOPT_URL, url.cstr()) 
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
	#cb_finished(); // TODO 
}

fn download_file(url, out string) {
	download_file_with_progress(url, out, empty, empty) 
}

fn empty() {
 
} 

