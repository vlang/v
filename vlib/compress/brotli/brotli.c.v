// Brotli compression/decompression helpers using the system libbrotli runtime.
module brotli

import os

$if linux {
	#flag -ldl
}
#insert "@VEXEROOT/vlib/compress/brotli/brotli_dl.h"

fn C.v_brotli_open(const_name &char) voidptr
fn C.v_brotli_sym(handle voidptr, const_name &char) voidptr
fn C.v_brotli_close(handle voidptr) int
fn C.v_brotli_msan_unpoison(ptr voidptr, len usize)

const min_quality = 0
const max_quality = 11
const min_lgwin = 10
const max_lgwin = 24
const decode_chunk_size = 32 * 1024

const decoder_result_error = 0
const decoder_result_success = 1
const decoder_result_needs_more_input = 2
const decoder_result_needs_more_output = 3

type FNEncoderMaxCompressedSize = fn (usize) usize

type FNEncoderCompress = fn (int, int, int, usize, voidptr, &usize, voidptr) int

type FNDecoderCreateInstance = fn (voidptr, voidptr, voidptr) voidptr

type FNDecoderDestroyInstance = fn (voidptr)

type FNDecoderDecompressStream = fn (voidptr, &usize, &&u8, &usize, &&u8, &usize) int

type FNDecoderGetErrorCode = fn (voidptr) int

type FNDecoderErrorString = fn (int) charptr

// Mode tunes the Brotli encoder for generic, text, or font data.
pub enum Mode {
	generic = 0
	text    = 1
	font    = 2
}

// CompressParams controls Brotli compression.
@[params]
pub struct CompressParams {
pub:
	quality int  = 11
	lgwin   int  = 22
	mode    Mode = .generic
}

// DecompressParams controls Brotli decompression.
@[params]
pub struct DecompressParams {}

fn open_library_name(name string) voidptr {
	return C.v_brotli_open(&char(name.str))
}

fn open_env_library(env_name string) voidptr {
	path := os.getenv_opt(env_name) or { return 0 }
	if path == '' {
		return 0
	}
	return open_library_name(path)
}

fn open_encoder_library() !voidptr {
	env_handle := open_env_library('V_BROTLI_ENC_LIB')
	if env_handle != 0 {
		return env_handle
	}
	$if windows {
		for name in ['brotlienc.dll', 'libbrotlienc.dll'] {
			handle := open_library_name(name)
			if handle != 0 {
				return handle
			}
		}
	} $else $if macos {
		for name in ['libbrotlienc.dylib', '/opt/homebrew/lib/libbrotlienc.dylib',
			'/usr/local/lib/libbrotlienc.dylib'] {
			handle := open_library_name(name)
			if handle != 0 {
				return handle
			}
		}
	} $else {
		for name in ['libbrotlienc.so', 'libbrotlienc.so.1'] {
			handle := open_library_name(name)
			if handle != 0 {
				return handle
			}
		}
	}
	return error('brotli: could not load encoder library')
}

fn open_decoder_library() !voidptr {
	env_handle := open_env_library('V_BROTLI_DEC_LIB')
	if env_handle != 0 {
		return env_handle
	}
	$if windows {
		for name in ['brotlidec.dll', 'libbrotlidec.dll'] {
			handle := open_library_name(name)
			if handle != 0 {
				return handle
			}
		}
	} $else $if macos {
		for name in ['libbrotlidec.dylib', '/opt/homebrew/lib/libbrotlidec.dylib',
			'/usr/local/lib/libbrotlidec.dylib'] {
			handle := open_library_name(name)
			if handle != 0 {
				return handle
			}
		}
	} $else {
		for name in ['libbrotlidec.so', 'libbrotlidec.so.1'] {
			handle := open_library_name(name)
			if handle != 0 {
				return handle
			}
		}
	}
	return error('brotli: could not load decoder library')
}

fn close_library(handle voidptr) {
	C.v_brotli_close(handle)
}

fn library_symbol(handle voidptr, name string, kind string) !voidptr {
	sym := C.v_brotli_sym(handle, &char(name.str))
	if sym == 0 {
		return error('brotli: could not load ${kind} symbol ${name}')
	}
	return sym
}

// is_available reports whether both Brotli encoder and decoder libraries can be loaded.
pub fn is_available() bool {
	enc := open_encoder_library() or { return false }
	defer {
		close_library(enc)
	}
	dec := open_decoder_library() or { return false }
	defer {
		close_library(dec)
	}
	library_symbol(enc, 'BrotliEncoderCompress', 'encoder') or { return false }
	library_symbol(dec, 'BrotliDecoderDecompressStream', 'decoder') or { return false }
	return true
}

fn validate_compress_params(params CompressParams) ! {
	if params.quality < min_quality || params.quality > max_quality {
		return error('brotli: quality must be between ${min_quality} and ${max_quality}')
	}
	if params.lgwin < min_lgwin || params.lgwin > max_lgwin {
		return error('brotli: lgwin must be between ${min_lgwin} and ${max_lgwin}')
	}
}

// compress compresses data as a Brotli stream and returns the compressed bytes.
pub fn compress(data []u8, params CompressParams) ![]u8 {
	validate_compress_params(params)!
	enc := open_encoder_library()!
	defer {
		close_library(enc)
	}
	max_compressed_size := FNEncoderMaxCompressedSize(library_symbol(enc,
		'BrotliEncoderMaxCompressedSize', 'encoder')!)
	encoder_compress := FNEncoderCompress(library_symbol(enc, 'BrotliEncoderCompress', 'encoder')!)
	dst_capacity := max_compressed_size(usize(data.len))
	if dst_capacity == 0 {
		return error('brotli: input is too large')
	}
	mut dst := []u8{len: int(dst_capacity)}
	mut keep_dst := false
	defer {
		if !keep_dst {
			unsafe { dst.free() }
		}
	}
	mut encoded_size := dst_capacity
	ok := encoder_compress(params.quality, params.lgwin, int(params.mode), usize(data.len),
		data.data, &encoded_size, dst.data)
	if ok == 0 {
		return error('brotli: compression failed')
	}
	C.v_brotli_msan_unpoison(dst.data, encoded_size)
	keep_dst = true
	return dst[..int(encoded_size)]
}

// decompress decompresses a Brotli stream and returns the decompressed bytes.
pub fn decompress(data []u8, _ DecompressParams) ![]u8 {
	dec := open_decoder_library()!
	defer {
		close_library(dec)
	}
	decoder_create := FNDecoderCreateInstance(library_symbol(dec, 'BrotliDecoderCreateInstance',
		'decoder')!)
	decoder_destroy := FNDecoderDestroyInstance(library_symbol(dec, 'BrotliDecoderDestroyInstance',
		'decoder')!)
	decoder_stream := FNDecoderDecompressStream(library_symbol(dec,
		'BrotliDecoderDecompressStream', 'decoder')!)
	decoder_get_error := FNDecoderGetErrorCode(library_symbol(dec, 'BrotliDecoderGetErrorCode',
		'decoder')!)
	decoder_error_string := FNDecoderErrorString(library_symbol(dec, 'BrotliDecoderErrorString',
		'decoder')!)
	state := decoder_create(unsafe { nil }, unsafe { nil }, unsafe { nil })
	if state == 0 {
		return error('brotli: could not create decoder state')
	}
	defer {
		decoder_destroy(state)
	}
	mut out := []u8{}
	mut keep_out := false
	defer {
		if !keep_out {
			unsafe { out.free() }
		}
	}
	mut chunk := []u8{len: decode_chunk_size}
	defer {
		unsafe { chunk.free() }
	}
	mut available_in := usize(data.len)
	mut next_in := unsafe { &u8(data.data) }
	for {
		mut available_out := usize(chunk.len)
		mut next_out := unsafe { &u8(chunk.data) }
		mut total_out := usize(0)
		result := decoder_stream(state, &available_in, &next_in, &available_out, &next_out,
			&total_out)
		produced := chunk.len - int(available_out)
		if produced > 0 {
			C.v_brotli_msan_unpoison(chunk.data, usize(produced))
			out << chunk[..produced]
		}
		match result {
			decoder_result_success {
				if available_in != 0 {
					return error('brotli: trailing data after stream')
				}
				keep_out = true
				return out
			}
			decoder_result_needs_more_output {
				if produced == 0 {
					return error('brotli: decoder needs more output without producing data')
				}
			}
			decoder_result_needs_more_input {
				return error('brotli: unexpected end of input')
			}
			decoder_result_error {
				code := decoder_get_error(state)
				msg := unsafe { tos_clone(&u8(decoder_error_string(code))) }
				return error('brotli: ${msg}')
			}
			else {
				return error('brotli: unexpected decoder result ${result}')
			}
		}
	}
	return error('brotli: decoder stopped unexpectedly')
}
