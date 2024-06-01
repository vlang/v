module http

// TerminalStreamingDownloader is the same as http.SilentStreamingDownloader, but produces a progress line on stdout.
pub struct TerminalStreamingDownloader {
	SilentStreamingDownloader
}

// on_chunk is called multiple times, once per chunk of received content.
pub fn (mut d TerminalStreamingDownloader) on_chunk(request &Request, chunk []u8, already_received u64, expected u64) ! {
	d.SilentStreamingDownloader.on_chunk(request, chunk, already_received, expected)!
	print('\rDownloading `${request.url}` to `${d.path}` ${f64(already_received) / expected * 100:5.2f}%')
	flush_stdout()
}

// on_finish is called once at the end of the download.
pub fn (mut d TerminalStreamingDownloader) on_finish(request &Request, response &Response) ! {
	d.SilentStreamingDownloader.on_finish(request, response)!
	println('')
	flush_stdout()
}
