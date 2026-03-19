module http

import os

// Downloader is the interface that you have to implement, if you need to customise
// how download_file_with_progress works, and what output it produces while a file
// is downloaded.
pub interface Downloader {
mut:
	// Called once, at the start of the streaming download. You can do setup here,
	// like opening a target file, changing request.stop_copying_limit to a different value,
	// if you need it.
	on_start(mut request Request, path string) !
	// Called many times, once a chunk of data is received
	on_chunk(request &Request, chunk []u8, already_received u64, expected u64) !
	// Called once, at the end of the streaming download. Do cleanup here,
	// like closing a file (opened in on_start), reporting stats etc.
	on_finish(request &Request, response &Response) !
}

// DownloaderParams is similar to FetchConfig, but it also allows you to pass
// a `downloader: your_downloader_instance` parameter.
// See also http.SilentStreamingDownloader, and http.TerminalStreamingDownloader .
@[params]
pub struct DownloaderParams {
	FetchConfig
pub mut:
	downloader &Downloader = &TerminalStreamingDownloader{}
}

// download_file_with_progress will save the URL `url` to the filepath `path` .
// Unlike download_file/2, it *does not* load the whole content in memory, but
// instead streams it chunk by chunk to the target `path`, as the chunks are received
// from the network. This makes it suitable for downloading big files, *without* increasing
// the memory consumption of your application.
//
// By default, it will also show a progress line, while the download happens.
// If you do not want a status line, you can call it like this:
// `http.download_file_with_progress(url, path, downloader: http.SilentStreamingDownloader{})`,
// or you can implement your own http.Downloader and pass that instead.
//
// Note: the returned response by this function, will have a truncated .body, after the first
// few KBs, because it does not accumulate all its data in memory, instead relying on the
// downloaders to save the received data chunk by chunk. You can parametrise this by
// using `stop_copying_limit:` but you need to pass a number that is big enough to fit
// at least all headers in the response, otherwise the parsing of the response at the end will
// fail, despite saving all the data in the file before that. The default is 65536 bytes.
pub fn download_file_with_progress(url string, path string, params DownloaderParams) !Response {
	mut d := unsafe { params.downloader }
	mut req := Request{
		method:   .get
		url:      url
		user_ptr: voidptr(d)
	}
	d.on_start(mut req, path)!
	response := req.do()!
	if response.status_code != 200 {
		return error_with_code(response.body, response.status_code)
	}
	if response.body.len > 0 {
		d.on_chunk(&req, response.body.bytes(), 0, u64(response.body.len))!
	}
	os.write_file(path, response.body)!
	d.on_finish(&req, &response)!
	return response
}
