module http

// detect_content_type sniffs the MIME type of the first bytes of a body, so
// callers can serve files whose extension is unknown instead of failing.
// The algorithm follows Go's net/http sniffing: exact binary magic first,
// then text signatures for text-looking input, `text/plain; charset=utf-8`
// for other text, and `application/octet-stream` as the last resort. (The
// motivation comes from v-mime style content detection.)
// Detection examines at most 512 bytes, plus up to three available continuation
// bytes when a UTF-8 sequence crosses that boundary.
//
// Only use the result when no better type is known: a sniffed `text/html`
// served from your own origin executes scripts, so never relabel an
// explicitly configured type with this.
pub fn detect_content_type(data []u8) string {
	if data.len == 0 {
		return 'text/plain; charset=utf-8'
	}
	first := if data.len > 512 { data[..512] } else { data }
	ct := binary_signature_content_type(first)
	if ct != '' {
		return ct
	}
	if is_text_content(data, first.len) {
		text_ct := text_signature_content_type(first)
		if text_ct != '' {
			return text_ct + '; charset=utf-8'
		}
		return 'text/plain; charset=utf-8'
	}
	return 'application/octet-stream'
}

// binary_signature_content_type matches exact magic-byte prefixes.
fn binary_signature_content_type(data []u8) string {
	if starts_with_bytes(data, 'GIF87a'.bytes()) || starts_with_bytes(data, 'GIF89a'.bytes()) {
		return 'image/gif'
	}
	if starts_with_bytes(data, [u8(0x89), 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]) {
		return 'image/png'
	}
	if data.len >= 3 && data[0] == 0xFF && data[1] == 0xD8 && data[2] == 0xFF {
		return 'image/jpeg'
	}
	if starts_with_bytes(data, 'BM'.bytes()) {
		return 'image/bmp'
	}
	if starts_with_bytes(data, [u8(0x49), 0x49, 0x2A, 0x00])
		|| starts_with_bytes(data, [u8(0x4D), 0x4D, 0x00, 0x2A]) {
		return 'image/tiff'
	}
	if starts_with_bytes(data, [u8(0), 0, 1, 0]) {
		return 'image/x-icon'
	}
	if starts_with_bytes(data, 'RIFF'.bytes()) && data.len >= 12 {
		if data[8] == `W` && data[9] == `A` && data[10] == `V` && data[11] == `E` {
			return 'audio/wav'
		}
		if data[8] == `W` && data[9] == `E` && data[10] == `B` && data[11] == `P` {
			return 'image/webp'
		}
	}
	if data.len >= 12 && data[4] == `f` && data[5] == `t` && data[6] == `y` && data[7] == `p` {
		brand := data[8..12].bytestr()
		if brand == 'avif' || brand == 'avis' {
			return 'image/avif'
		}
		if brand == 'heic' || brand == 'heix' || brand == 'hevc' || brand == 'hevx'
			|| brand == 'mif1' || brand == 'msf1' {
			return 'image/heic'
		}
		if brand == 'isom' || brand == 'iso2' || brand == 'mp41' || brand == 'mp42'
			|| brand == 'avc1' || brand == 'mmp4' {
			return 'video/mp4'
		}
	}
	if starts_with_bytes(data, '%PDF-'.bytes()) {
		return 'application/pdf'
	}
	if starts_with_bytes(data, '%!PS-Adobe-'.bytes()) {
		return 'application/postscript'
	}
	if starts_with_bytes(data, [u8(0x50), 0x4B, 0x03, 0x04]) {
		return 'application/zip'
	}
	if starts_with_bytes(data, 'OggS'.bytes()) {
		return 'application/ogg'
	}
	if starts_with_bytes(data, 'ID3'.bytes()) {
		return 'audio/mpeg'
	}
	if data.len >= 2 && data[0] == 0xFF && data[1] & 0xE0 == 0xE0 && data[1] & 0x18 != 0x08
		&& data[1] & 0x06 != 0x00 {
		return 'audio/mpeg'
	}
	if starts_with_bytes(data, [u8(0), `a`, `s`, `m`]) {
		return 'application/wasm'
	}
	return ''
}

// text_signature_content_type matches text document markers. `data` must
// already be known to be text; matching is case-insensitive and leading
// whitespace is skipped, like browsers do.
fn text_signature_content_type(data []u8) string {
	mut start := 0
	for start < data.len && (data[start] == ` ` || data[start] == `\t` || data[start] == `\n`
		|| data[start] == `\r` || data[start] == `\x0c`) {
		start++
	}
	head := data[start..].bytestr().to_lower()
	if head.starts_with('<?xml') {
		return 'text/xml'
	}
	for tag in ['<!doctype html', '<html', '<head', '<script', '<iframe', '<h1', '<div', '<font',
		'<table', '<a', '<style', '<title', '<b', '<body', '<br', '<p', '<!--'] {
		if head.starts_with(tag) && (tag == '<!--' || (head.len > tag.len
			&& (head[tag.len] == ` ` || head[tag.len] == `>`))) {
			return 'text/html'
		}
	}
	return ''
}

// is_text_content reports whether every byte is printable ASCII, accepted
// whitespace, or part of a strictly valid UTF-8 sequence. NUL bytes,
// controls, DEL, and broken sequences mean binary. A sequence starting before
// limit is checked in full, even when the sniffing limit splits the sequence.
fn is_text_content(data []u8, limit int) bool {
	mut i := 0
	for i < limit {
		b := data[i]
		if b == 9 || b == 10 || b == 11 || b == 12 || b == 13 || (b >= 32 && b < 127) {
			i++
			continue
		}
		mut need := 0
		if b >= 0xC2 && b <= 0xDF {
			need = 1
		} else if b >= 0xE0 && b <= 0xEF {
			need = 2
		} else if b >= 0xF0 && b <= 0xF4 {
			need = 3
		} else {
			return false
		}
		if i + need >= data.len {
			return false
		}
		for j in 1 .. need + 1 {
			c := data[i + j]
			if c < 0x80 || c > 0xBF {
				return false
			}
		}
		if need == 2 && b == 0xE0 && data[i + 1] < 0xA0 {
			return false
		}
		if need == 2 && b == 0xED && data[i + 1] > 0x9F {
			return false
		}
		if need == 3 && b == 0xF0 && data[i + 1] < 0x90 {
			return false
		}
		if need == 3 && b == 0xF4 && data[i + 1] > 0x8F {
			return false
		}
		i += need + 1
	}
	return true
}

fn starts_with_bytes(data []u8, prefix []u8) bool {
	if data.len < prefix.len {
		return false
	}
	return data[..prefix.len] == prefix
}
