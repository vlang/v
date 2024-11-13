module http

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
	mut config := params.FetchConfig
	config.url = url
	config.user_ptr = voidptr(d)
	config.on_progress_body = download_progres_cb
	if config.stop_copying_limit == -1 {
		// leave more than enough space for potential redirect headers
		config.stop_copying_limit = 65536
	}
	mut req := prepare(config)!
	d.on_start(mut req, path)!
	response := req.do()!
	$if windows && !no_vschannel ? {
		// TODO: remove this, when windows supports streaming properly through vschannel
		// For now though, just ensure that the complete body is "received" in one big chunk:
		d.on_chunk(req, response.body.bytes(), 0, u64(response.body.len))!
	}
	d.on_finish(req, response)!
	return response
}

const zz = &Downloader(unsafe { nil })

fn download_progres_cb(request &Request, chunk []u8, body_so_far u64, expected_size u64, status_code int) ! {
	// TODO: remove this hack, when `unsafe { &Downloader( request.user_ptr ) }` works reliably,
	// by just casting, without trying to promote the argument to the heap at all.
	mut d := unsafe { zz }
	pd := unsafe { &voidptr(&d) }
	unsafe {
		*pd = request.user_ptr
	}
	if status_code == 200 {
		// ignore redirects, we are interested in the chunks of the final file:
		d.on_chunk(request, chunk, body_so_far, expected_size)!
	}
}
