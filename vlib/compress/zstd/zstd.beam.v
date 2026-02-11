// BEAM backend stub for compress.zstd module
// Provides type-compatible placeholders so V code compiles on BEAM.
module zstd

const frame_header_size_max = 18
const content_size_unknown = u64(-1)
const content_size_error = u64(-2)

const buf_in_size = 1024 * 1024
const buf_out_size = 1024 * 1024

// note : new strategies _might_ be added in the future. Only the order (from fast to strong) is guaranteed
pub enum Strategy {
	default  = 0
	fast     = 1
	dfast    = 2
	greedy   = 3
	lazy     = 4
	lazy2    = 5
	btlazy2  = 6
	btopt    = 7
	btultra  = 8
	btultra2 = 9
}

pub enum CParameter {
	compression_level                 = 100
	window_log                        = 101
	hash_log                          = 102
	chain_log                         = 103
	search_log                        = 104
	min_match                         = 105
	target_length                     = 106
	strategy                          = 107
	target_c_block_size               = 130
	enable_long_distance_matching     = 160
	ldm_hash_log                      = 161
	ldm_min_match                     = 162
	ldm_bucket_size_log               = 163
	ldm_hash_rate_log                 = 164
	content_size_flag                 = 200
	checksum_flag                     = 201
	dict_id_flag                      = 202
	nb_workers                        = 400
	job_size                          = 401
	overlap_log                       = 402
	experimental_param1               = 500
	experimental_param2               = 10
	experimental_param3               = 1000
	experimental_param4               = 1001
	experimental_param5               = 1002
	experimental_param7               = 1004
	experimental_param8               = 1005
	experimental_param9               = 1006
	experimental_param10              = 1007
	experimental_param11              = 1008
	experimental_param12              = 1009
	experimental_param13              = 1010
	experimental_param14              = 1011
	experimental_param15              = 1012
	experimental_param16              = 1013
	experimental_param17              = 1014
	experimental_param18              = 1015
	experimental_param19              = 1016
	experimental_param20              = 1017
}

pub struct Bounds {
pub:
	error       usize
	lower_bound int
	upper_bound int
}

pub enum ResetDirective {
	session_only           = 1
	parameters             = 2
	session_and_parameters = 3
}

pub enum DParameter {
	window_log_max      = 100
	experimental_param1 = 1000
	experimental_param2 = 1001
	experimental_param3 = 1002
	experimental_param4 = 1003
	experimental_param5 = 1004
	experimental_param6 = 1005
}

// streaming compression
pub struct InBuffer {
pub mut:
	src  voidptr
	size usize
	pos  usize
}

pub struct OutBuffer {
pub mut:
	dst  voidptr
	size usize
	pos  usize
}

pub enum EndDirective {
	@continue = 0
	flush     = 1
	end       = 2
}

// version_number returns runtime library version (stub returns 0 on BEAM).
pub fn version_number() u32 {
	return 0
}

// version_string returns runtime library version string (stub on BEAM).
pub fn version_string() string {
	return '0.0.0-beam-stub'
}

// is_error tells if a `usize` function result is an error code.
// In zstd, error codes have the high bit set (values > ZSTD_CONTENTSIZE_ERROR).
pub fn is_error(code usize) bool {
	// zstd convention: error codes are large unsigned values (high bit set)
	// On 64-bit: errors start at ~(0) - 127 and go up
	return code > usize(0xFFFFFFFFFFFFFF00)
}

// get_error_name provides readable string from an error code.
pub fn get_error_name(code usize) string {
	// Common zstd error codes mapped to names
	// These are the standard error names from the zstd library
	if !is_error(code) {
		return 'No error detected'
	}
	// The error code is ~(errorCode), so the actual code is ~code
	// Without the C library, we return a generic message
	return 'zstd error (code ${code})'
}

// check_error checks the zstd error code.
pub fn check_error(code usize) ! {
	if is_error(code) {
		return error(get_error_name(code))
	}
}

// min_c_level returns minimum negative compression level allowed.
pub fn min_c_level() int {
	return -131072
}

// max_c_level returns maximum compression level available.
pub fn max_c_level() int {
	return 22
}

// default_c_level returns default compression level.
pub fn default_c_level() int {
	return 3
}

@[params]
pub struct CompressParams {
pub:
	compression_level int      = 3
	nb_threads        int      = 1
	checksum_flag     bool     = true
	strategy          Strategy = .default
}

// compress compresses an array of bytes using zstd.
// BEAM note: Zstandard is not part of Erlang/OTP stdlib.
// Options: Use a NIF wrapper around libzstd, or a port-based solution.
// For now, returns an error since no pure V/Erlang zstd implementation exists.
pub fn compress(data []u8, params CompressParams) ![]u8 {
	if data.len == 0 {
		return []
	}
	if params.compression_level < min_c_level() || params.compression_level > max_c_level() {
		return error('zstd: compression level ${params.compression_level} out of range [${min_c_level()}, ${max_c_level()}]')
	}
	return error('zstd compress not available on BEAM (requires NIF or port-based libzstd binding)')
}

@[params]
pub struct DecompressParams {
pub:
	window_log_max int
}

// decompress decompresses an array of bytes using zstd.
// BEAM note: Zstandard is not part of Erlang/OTP stdlib.
// Validates the zstd magic number (0xFD2FB528) before attempting decompression.
pub fn decompress(data []u8, params DecompressParams) ![]u8 {
	if data.len == 0 {
		return []
	}
	// Check zstd magic number (first 4 bytes: 0x28, 0xB5, 0x2F, 0xFD in little-endian)
	if data.len >= 4 {
		if data[0] != 0x28 || data[1] != 0xB5 || data[2] != 0x2F || data[3] != 0xFD {
			return error('zstd: invalid magic number, not zstd compressed data')
		}
	} else {
		return error('zstd: data too short (minimum 4 bytes for magic number)')
	}
	return error('zstd decompress not available on BEAM (requires NIF or port-based libzstd binding)')
}

pub struct CCtx {
mut:
	ctx voidptr
}

// new_cctx creates a compression context.
pub fn new_cctx(params CompressParams) !&CCtx {
	return error('zstd CCtx not available on BEAM')
}

// set sets a compression parameter.
pub fn (mut c CCtx) set(c_param CParameter, val int) ! {
	return error('zstd CCtx.set not available on BEAM')
}

// compress_stream2 does stream compression.
pub fn (mut c CCtx) compress_stream2(output &OutBuffer, input &InBuffer, mode EndDirective) !usize {
	return error('zstd compress_stream2 not available on BEAM')
}

// free_cctx frees a compression context.
pub fn (mut c CCtx) free_cctx() usize {
	return 0
}

pub struct DCtx {
mut:
	ctx voidptr
}

// new_dctx creates a decompression context.
pub fn new_dctx(params DecompressParams) !&DCtx {
	return error('zstd DCtx not available on BEAM')
}

// set sets a decompression parameter.
pub fn (mut d DCtx) set(d_param DParameter, val int) ! {
	return error('zstd DCtx.set not available on BEAM')
}

// decompress_stream does stream decompression.
pub fn (mut d DCtx) decompress_stream(output &OutBuffer, input &InBuffer) !usize {
	return error('zstd decompress_stream not available on BEAM')
}

// free_dctx frees a decompression context.
pub fn (mut d DCtx) free_dctx() usize {
	return 0
}

// store_array compresses an array's data and stores it to a file.
// BEAM note: Requires both zstd compression and file I/O.
pub fn store_array[T](fname string, array []T, params CompressParams) ! {
	if fname.len == 0 {
		return error('zstd store_array: empty filename')
	}
	if array.len == 0 {
		return error('zstd store_array: empty array')
	}
	return error('zstd store_array not available on BEAM (requires NIF or port-based libzstd binding)')
}

// load_array returns an array with data decompressed from a file.
// BEAM note: Requires both zstd decompression and file I/O.
pub fn load_array[T](fname string, params DecompressParams) ![]T {
	if fname.len == 0 {
		return error('zstd load_array: empty filename')
	}
	return error('zstd load_array not available on BEAM (requires NIF or port-based libzstd binding)')
}
