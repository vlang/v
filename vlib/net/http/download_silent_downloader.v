module http

import os

// SilentStreamingDownloader just saves the downloaded file chunks to the given path.
// It does *no reporting at all*.
// Note: the folder part of the path should already exist, and has to be writable.
pub struct SilentStreamingDownloader {
pub mut:
	path string
	f    os.File
}

// on_start is called once at the start of the download.
pub fn (mut d SilentStreamingDownloader) on_start(mut request Request, path string) ! {
	d.path = path
	d.f = os.create(path)!
}

// on_chunk is called multiple times, once per chunk of received content.
pub fn (mut d SilentStreamingDownloader) on_chunk(request &Request, chunk []u8, already_received u64, expected u64) ! {
	d.f.write(chunk)!
}

// on_finish is called once at the end of the download.
pub fn (mut d SilentStreamingDownloader) on_finish(request &Request, response &Response) ! {
	d.f.close()
}
