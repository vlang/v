module veb

import net.http
import os

// In-process tests for unknown-extension static files: file() sniffs the
// content instead of failing. No network involved.
fn sniffed_static_fixture(name string, content []u8) string {
	path := os.join_path(os.temp_dir(), name)
	os.write_file(path, content.bytestr()) or { panic(err) }
	return path
}

fn sniffed_static_request() http.Request {
	return http.Request{
		method: .get
		url:    '/file'
		header: http.new_custom_header_from_map({
			'Host': '127.0.0.1'
		}) or { panic(err) }
	}
}

fn sniffed_static_serve(path string) &Context {
	mut ctx := &Context{
		req: sniffed_static_request()
	}
	ctx.file(path)
	return ctx
}

fn test_unknown_extension_png_is_sniffed() {
	path := sniffed_static_fixture('veb_sniff_test_image.blob', [u8(0x89), 0x50, 0x4E, 0x47, 0x0D,
		0x0A, 0x1A, 0x0A, 0x00])
	defer {
		os.rm(path) or {}
	}
	ctx := sniffed_static_serve(path)
	assert ctx.res.status_code == 200
	assert (ctx.res.header.get(.content_type) or { '' }) == 'image/png'
	assert ctx.return_type == .file
}

fn test_unknown_extension_html_is_sniffed() {
	path := sniffed_static_fixture('veb_sniff_test_page.blob', '<!DOCTYPE html><p>hi</p>'.bytes())
	defer {
		os.rm(path) or {}
	}
	ctx := sniffed_static_serve(path)
	assert ctx.res.status_code == 200
	assert (ctx.res.header.get(.content_type) or { '' }) == 'text/html; charset=utf-8'
}

fn test_unknown_extension_binary_is_octet_stream() {
	path := sniffed_static_fixture('veb_sniff_test_bin.blob', [u8(0), 1, 2, 3, 4])
	defer {
		os.rm(path) or {}
	}
	ctx := sniffed_static_serve(path)
	assert ctx.res.status_code == 200
	assert (ctx.res.header.get(.content_type) or { '' }) == 'application/octet-stream'
}

fn test_known_extension_still_wins_over_sniffing() {
	path := sniffed_static_fixture('veb_sniff_test_style.css', 'body{}'.bytes())
	defer {
		os.rm(path) or {}
	}
	ctx := sniffed_static_serve(path)
	assert ctx.res.status_code == 200
	assert (ctx.res.header.get(.content_type) or { '' }) == 'text/css'
}
