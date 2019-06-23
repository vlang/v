// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

import os

type downloadfn fn (written int)

struct DownloadStruct {
	stream  voidptr
	written int
	cb      downloadfn
}

fn download_cb(ptr voidptr, size, nmemb size_t, userp voidptr) int {
	// # struct http__MemoryStruct *mem = (struct http__MemoryStruct *)userp;
	data := &DownloadStruct(userp)
	# size_t written = fwrite(ptr, size, nmemb, (FILE*)(data->stream));
	// # printf("!!!%d\n", written);
	# data->written += written;
	if !isnil(data.cb) {
		# data->cb(data->written);
	}
	# return written;
	return 0
}

fn download_file_with_progress(url, out string, cb, cb_finished voidptr) {
	curl := C.curl_easy_init()
	if isnil(curl) {
		return
	}
	# FILE*        fp = fopen(out.str,"wb");
	# curl_easy_setopt(curl, CURLOPT_URL, url.str);
	C.curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, download_cb)
	// # curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, http__download_cb);
	data := &DownloadStruct {
		// stream:fp
		cb: cb
	}
	# data->stream = fp;
	# curl_easy_setopt(curl, CURLOPT_WRITEDATA, data);
	# double d = 0;
	# curl_easy_getinfo(curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &d);
	# CURLcode        res = curl_easy_perform(curl);
	println('DONE!')
	# curl_easy_cleanup(curl);
	# fclose(fp);
	# void (*finished)() =cb_finished; finished();
}

fn download_file(url, out string) {
	// println('\nDOWNLOAD FILE $out url=$url')
	// -L follow redirects
	// println('curl -L -o "$out" "$url"')
	res := os.system('curl -s -L -o "$out" "$url"')
	// res := os.system('curl -s -L -o "$out" "$url"')
	// println(res)
}

