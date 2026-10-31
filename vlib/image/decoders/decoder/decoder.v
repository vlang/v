/**********************************************************************
*
* Magic-byte dispatcher routing image data to the per-format decoders
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module decoder

// decoder is the magic-byte dispatcher that routes an image buffer to the
// right per-format V decoder module. It keeps the viewer format-agnostic:
// the viewer just calls decoder.decode(data) and gets a common.Image back.

import os
import image.decoders.common
import image.decoders.bmp
import image.decoders.gif
import image.decoders.png
import image.decoders.tiff
import image.decoders.webp
import image.decoders.jpeg

// sniff returns a short format tag based on the file's magic bytes, or an
// empty string when the format is not recognised.
//
// The checks are ordered so that the most specific signatures are tested
// first (e.g. RIFF....WEBP needs bytes 0..3 AND 8..11).
pub fn sniff(data []u8) string {
	// JPEG: FF D8 FF
	if data.len >= 3 && data[0] == 0xFF && data[1] == 0xD8 && data[2] == 0xFF {
		return 'jpeg'
	}
	// PNG: 89 50 4E 47 ("\x89PNG")
	if data.len >= 4 && data[0] == 0x89 && data[1] == 0x50 && data[2] == 0x4E && data[3] == 0x47 {
		return 'png'
	}
	// BMP: 42 4D ("BM")
	if data.len >= 2 && data[0] == 0x42 && data[1] == 0x4D {
		return 'bmp'
	}
	// GIF: "GIF" (GIF87a / GIF89a)
	if data.len >= 3 && data[0] == 0x47 && data[1] == 0x49 && data[2] == 0x46 {
		return 'gif'
	}
	// WEBP: "RIFF" .... "WEBP"
	if data.len >= 12 && data[0] == 0x52 && data[1] == 0x49 && data[2] == 0x46 && data[3] == 0x46
		&& data[8] == 0x57 && data[9] == 0x45 && data[10] == 0x42 && data[11] == 0x50 {
		return 'webp'
	}
	// TIFF: "II" 2A 00 (little) or "MM" 00 2A (big)
	if data.len >= 4 && ((data[0] == 0x49 && data[1] == 0x49 && data[2] == 0x2A && data[3] == 0x00)
		|| (data[0] == 0x4D && data[1] == 0x4D && data[2] == 0x00 && data[3] == 0x2A)) {
		return 'tiff'
	}
	return ''
}

// decode sniffs the buffer and dispatches to the matching decoder.
pub fn decode(data []u8) !common.Image {
	match sniff(data) {
		'jpeg' {
			return jpeg.decode(data)
		}
		'png' {
			return png.decode(data)
		}
		'bmp' {
			return bmp.decode(data)
		}
		'gif' {
			return gif.decode(data)
		}
		'webp' {
			return webp.decode(data)
		}
		'tiff' {
			return tiff.decode(data)
		}
		else {
			return error('decoder: unknown image format (${data.len} bytes)')
		}
	}
}

// decode_animation sniffs the buffer and dispatches to a format decoder
// that can produce a multi-frame animation. The viewer uses this for
// animated GIFs; other formats fall back to the static decode() path.
pub fn decode_animation(data []u8) !common.Animation {
	match sniff(data) {
		'gif' {
			return gif.decode_animation(data)
		}
		else {
			return error('decoder: no animation support for this format')
		}
	}
}

// decode_file reads a file from disk and decodes it.
pub fn decode_file(path string) !common.Image {
	data := os.read_bytes(path) or {
		return error('decoder: cannot read file `${path}`')
	}
	return decode(data)
}
