module http

import time

// TerminalStreamingDownloader is the same as http.SilentStreamingDownloader, but produces a progress line on stdout.
pub struct TerminalStreamingDownloader {
	SilentStreamingDownloader
mut:
	start_time    time.Time
	past_time     time.Time
	past_received u64
}

// on_start is called once at the start of the download.
pub fn (mut d TerminalStreamingDownloader) on_start(mut request Request, path string) ! {
	d.SilentStreamingDownloader.on_start(mut request, path)!
	d.start_time = time.now()
	d.past_time = time.now()
}

// on_chunk is called multiple times, once per chunk of received content.
pub fn (mut d TerminalStreamingDownloader) on_chunk(request &Request, chunk []u8, already_received u64,
	expected u64) ! {
	now := time.now()
	elapsed := now - d.start_time
	// delta_elapsed := now - d.past_time
	// delta_bytes := already_received - d.past_received
	d.past_time = now
	d.past_received = already_received
	ratio := f64(already_received) / f64(expected)
	res := f64(elapsed) / ratio
	mut estimated := time.Duration(max_i64)
	if f64(min_i64) < res && res < f64(max_i64) {
		estimated = i64(res)
	}
	speed := f64(time.millisecond) * f64(already_received) / f64(elapsed)
	elapsed_s := elapsed.seconds()
	estimated_s := estimated.seconds()
	eta_s := f64_max(estimated_s - elapsed_s, 0.0)

	d.SilentStreamingDownloader.on_chunk(request, chunk, already_received, expected)!
	print('\rDownloading to `${d.path}` ${100.0 * ratio:6.2f}%, ${f64(already_received) / (1024 * 1024):7.3f}/${f64(expected) / (1024 * 1024):-7.3f}MB, ${speed:6.0f}KB/s, elapsed: ${elapsed_s:6.0f}s, eta: ${eta_s:6.0f}s')
	flush_stdout()
}

// on_finish is called once at the end of the download.
pub fn (mut d TerminalStreamingDownloader) on_finish(request &Request, response &Response) ! {
	d.SilentStreamingDownloader.on_finish(request, response)!
	println('')
	flush_stdout()
}
