import net.http

fn test_detect_empty_is_plain_text() {
	assert http.detect_content_type([]u8{}) == 'text/plain; charset=utf-8'
}

fn test_detect_plain_text() {
	assert http.detect_content_type('hello, world'.bytes()) == 'text/plain; charset=utf-8'
	assert http.detect_content_type('line1\nline2\r\nline3'.bytes()) == 'text/plain; charset=utf-8'
}

fn test_detect_html() {
	assert http.detect_content_type('<!DOCTYPE html><html></html>'.bytes()) == 'text/html; charset=utf-8'
	assert http.detect_content_type('  \n<HTML><BODY>'.bytes()) == 'text/html; charset=utf-8'
	assert http.detect_content_type('<!-- comment -->'.bytes()) == 'text/html; charset=utf-8'
}

fn test_detect_html_requires_a_tag_boundary() {
	for body in ['<apple>example</apple>', '<plaintext>text', '<bodyguard>', '<htmlish>', '<p',
		'<html'] {
		assert http.detect_content_type(body.bytes()) == 'text/plain; charset=utf-8'
	}
	assert http.detect_content_type('\x0c<HTML lang="en">'.bytes()) == 'text/html; charset=utf-8'
}

fn test_detect_utf8_across_sniffing_boundary() {
	for text in ['a'.repeat(511) + 'é', 'a'.repeat(510) + '€', 'a'.repeat(511) + '😀'] {
		assert http.detect_content_type(text.bytes()) == 'text/plain; charset=utf-8'
	}
	assert http.detect_content_type(('a'.repeat(511) + '\xc3').bytes()) == 'application/octet-stream'
	assert http.detect_content_type(('a'.repeat(511) + '\xc3x').bytes()) == 'application/octet-stream'
}

fn test_detect_xml() {
	assert http.detect_content_type('<?xml version="1.0"?><a/>'.bytes()) == 'text/xml; charset=utf-8'
}

fn test_detect_images() {
	assert http.detect_content_type([u8(0x89), 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00]) == 'image/png'
	assert http.detect_content_type('GIF89a'.bytes()) == 'image/gif'
	assert http.detect_content_type([u8(0xFF), 0xD8, 0xFF, 0xE0]) == 'image/jpeg'
	assert http.detect_content_type('BM'.bytes()) == 'image/bmp'
	assert http.detect_content_type([u8(0), 0, 1, 0, 5]) == 'image/x-icon'
}

fn test_detect_documents_and_archives() {
	assert http.detect_content_type('%PDF-1.4'.bytes()) == 'application/pdf'
	assert http.detect_content_type('PK\x03\x04'.bytes()) == 'application/zip'
	assert http.detect_content_type([u8(0), `a`, `s`, `m`, 1, 0, 0, 0]) == 'application/wasm'
}

fn test_detect_audio_video() {
	assert http.detect_content_type('ID3\x04\x00'.bytes()) == 'audio/mpeg'
	mp4 := [u8(0), 0, 0, 24, `f`, `t`, `y`, `p`, `i`, `s`, `o`, `m`]
	assert http.detect_content_type(mp4) == 'video/mp4'
}

fn test_detect_binary_falls_back_to_octet_stream() {
	assert http.detect_content_type([u8(0), 1, 2, 3]) == 'application/octet-stream'
	// 0x1F is a control byte: neither text nor any known magic.
	assert http.detect_content_type([u8(0x1F), 0xFF, 0x00]) == 'application/octet-stream'
	// Truncated PNG magic is neither text nor a known signature.
	assert http.detect_content_type([u8(0x89), `P`, `N`]) == 'application/octet-stream'
}
