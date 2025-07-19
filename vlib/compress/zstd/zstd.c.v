// zstd(https://github.com/facebook/zstd) is a fast real-time compression algorithm developed by Facebook.
// zstd compression/decompression
module zstd

import os
import encoding.binary

#flag -I @VMODROOT/thirdparty/zstd
#include "zstd.c"	// msvc can't compile multiple source files, so included

const frame_header_size_max = 18
const content_size_unknown = u64(-1)
const content_size_error = u64(-2)

const buf_in_size = 1024 * 1024
const buf_out_size = 1024 * 1024

fn C.ZSTD_versionNumber() u32
fn C.ZSTD_versionString() charptr

fn C.ZSTD_compress(voidptr, usize, voidptr, usize, int) usize
fn C.ZSTD_decompress(voidptr, usize, voidptr, usize) usize
fn C.ZSTD_getFrameContentSize(voidptr, usize) u64
fn C.ZSTD_findFrameCompressedSize(voidptr, usize) usize
fn C.ZSTD_compressBound(usize) usize
fn C.ZSTD_isError(usize) u32
fn C.ZSTD_getErrorName(usize) charptr
fn C.ZSTD_minCLevel() int
fn C.ZSTD_maxCLevel() int
fn C.ZSTD_defaultCLevel() int
fn C.ZSTD_createCCtx() &C.ZSTD_CCtx
fn C.ZSTD_freeCCtx(voidptr) usize
fn C.ZSTD_compressCCtx(voidptr, voidptr, usize, voidptr, usize, int) usize
fn C.ZSTD_createDCtx() &C.ZSTD_DCtx
fn C.ZSTD_freeDCtx(voidptr) usize
fn C.ZSTD_decompressDCtx(voidptr, voidptr, usize, voidptr, usize) usize

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
	// compression parameters
	// Note: When compressing with a ZSTD_CDict these parameters are superseded
	// by the parameters used to construct the ZSTD_CDict.
	// See ZSTD_CCtx_refCDict() for more info (superseded-by-cdict).
	//
	// Set compression parameters according to pre-defined cLevel table.
	// Note that exact compression parameters are dynamically determined,
	// depending on both compression level and srcSize (when known).
	// Default level is ZSTD_CLEVEL_DEFAULT==3.
	// Special: value 0 means default, which is controlled by ZSTD_CLEVEL_DEFAULT.
	// Note 1 : it's possible to pass a negative compression level.
	// Note 2 : setting a level does not automatically set all other compression parameters
	// to default. Setting this will however eventually dynamically impact the compression
	// parameters which have not been manually set. The manually set
	// ones will 'stick'.
	compression_level = 100
	// Advanced compression parameters :
	// It's possible to pin down compression parameters to some specific values.
	// In which case, these values are no longer dynamically selected by the compressor
	//
	// Maximum allowed back-reference distance, expressed as power of 2.
	// This will set a memory budget for streaming decompression,
	// with larger values requiring more memory
	// and typically compressing more.
	// Must be clamped between ZSTD_WINDOWLOG_MIN and ZSTD_WINDOWLOG_MAX.
	// Special: value 0 means "use default windowLog".
	// Note: Using a windowLog greater than ZSTD_WINDOWLOG_LIMIT_DEFAULT
	// requires explicitly allowing such size at streaming decompression stage.
	window_log = 101
	// Size of the initial probe table, as a power of 2.
	// Resulting memory usage is (1 << (hashLog+2)).
	// Must be clamped between ZSTD_HASHLOG_MIN and ZSTD_HASHLOG_MAX.
	// Larger tables improve compression ratio of strategies <= dFast,
	// and improve speed of strategies > dFast.
	// Special: value 0 means "use default hashLog".
	hash_log = 102
	// Size of the multi-probe search table, as a power of 2.
	// Resulting memory usage is (1 << (chainLog+2)).
	// Must be clamped between ZSTD_CHAINLOG_MIN and ZSTD_CHAINLOG_MAX.
	// Larger tables result in better and slower compression.
	// This parameter is useless for "fast" strategy.
	// It's still useful when using "dfast" strategy,
	// in which case it defines a secondary probe table.
	// Special: value 0 means "use default chainLog".
	chain_log = 103
	// Number of search attempts, as a power of 2.
	// More attempts result in better and slower compression.
	// This parameter is useless for "fast" and "dFast" strategies.
	// Special: value 0 means "use default searchLog".
	search_log = 104
	// Minimum size of searched matches.
	// Note that Zstandard can still find matches of smaller size,
	// it just tweaks its search algorithm to look for this size and larger.
	// Larger values increase compression and decompression speed, but decrease ratio.
	// Must be clamped between ZSTD_MINMATCH_MIN and ZSTD_MINMATCH_MAX.
	// Note that currently, for all strategies < btopt, effective minimum is 4.
	// , for all strategies > fast, effective maximum is 6.
	// Special: value 0 means "use default minMatchLength".
	min_match = 105
	// Impact of this field depends on strategy.
	// For strategies btopt, btultra & btultra2:
	// Length of Match considered "good enough" to stop search.
	// Larger values make compression stronger, and slower.
	// For strategy fast:
	// Distance between match sampling.
	// Larger values make compression faster, and weaker.
	// Special: value 0 means "use default targetLength".
	target_length = 106
	// See ZSTD_strategy enum definition.
	// The higher the value of selected strategy, the more complex it is,
	// resulting in stronger and slower compression.
	// Special: value 0 means "use default strategy".
	strategy = 107
	// v1.5.6+
	// Attempts to fit compressed block size into approximately targetCBlockSize.
	// Bound by ZSTD_TARGETCBLOCKSIZE_MIN and ZSTD_TARGETCBLOCKSIZE_MAX.
	// Note that it's not a guarantee, just a convergence target (default:0).
	// No target when targetCBlockSize == 0.
	// This is helpful in low bandwidth streaming environments to improve end-to-end latency,
	// when a client can make use of partial documents (a prominent example being Chrome).
	// Note: this parameter is stable since v1.5.6.
	// It was present as an experimental parameter in earlier versions,
	// but it's not recommended using it with earlier library versions
	// due to massive performance regressions.
	target_c_block_size = 130
	// LDM mode parameters
	// Enable long distance matching.
	// This parameter is designed to improve compression ratio
	// for large inputs, by finding large matches at long distance.
	// It increases memory usage and window size.
	// Note: enabling this parameter increases default ZSTD_c_windowLog to 128 MB
	// except when expressly set to a different value.
	// Note: will be enabled by default if ZSTD_c_windowLog >= 128 MB and
	// compression strategy >= ZSTD_btopt (== compression level 16+)
	enable_long_distance_matching = 160
	// Size of the table for long distance matching, as a power of 2.
	// Larger values increase memory usage and compression ratio,
	// but decrease compression speed.
	// Must be clamped between ZSTD_HASHLOG_MIN and ZSTD_HASHLOG_MAX
	// default: windowlog - 7.
	// Special: value 0 means "automatically determine hashlog".
	ldm_hash_log = 161
	// Minimum match size for long distance matcher.
	// Larger/too small values usually decrease compression ratio.
	// Must be clamped between ZSTD_LDM_MINMATCH_MIN and ZSTD_LDM_MINMATCH_MAX.
	// Special: value 0 means "use default value" (default: 64).
	ldm_min_match = 162
	// log size of each bucket in the ldm hash table for collision resolution.
	// Larger values improve collision resolution but decrease compression speed.
	// The maximum value is ZSTD_LDM_BUCKETSIZELOG_MAX.
	// Special: value 0 means "use default value" (default: 3).
	ldm_bucket_size_log = 163
	// Frequency of inserting/looking up entries into the LDM hash table.
	// Must be clamped between 0 and (ZSTD_WINDOWLOG_MAX - ZSTD_HASHLOG_MIN).
	// Default is MAX(0, (windowLog - ldmHashLog)), optimizing hash table usage.
	// Larger values improve compression speed.
	// Deviating far from default value will likely result in a compression ratio decrease.
	// Special: value 0 means "automatically determine hashRateLog".
	ldm_hash_rate_log = 164
	// frame parameters
	// Content size will be written into frame header _whenever known_ (default:1)
	// Content size must be known at the beginning of compression.
	// This is automatically the case when using ZSTD_compress2(),
	// For streaming scenarios, content size must be provided with ZSTD_CCtx_setPledgedSrcSize()
	content_size_flag = 200
	// A 32-bits checksum of content is written at end of frame (default:0)
	checksum_flag = 201
	// When applicable, dictionary's ID is written into frame header (default:1)
	dict_id_flag = 202
	// multi-threading parameters
	// These parameters are only active if multi-threading is enabled (compiled with build macro ZSTD_MULTITHREAD).
	// Otherwise, trying to set any other value than default (0) will be a no-op and return an error.
	// In a situation where it's unknown if the linked library supports multi-threading or not,
	// setting ZSTD_c_nbWorkers to any value >= 1 and consulting the return value provides a quick way to check this property.
	//
	// Select how many threads will be spawned to compress in parallel.
	// When nbWorkers >= 1, triggers asynchronous mode when invoking ZSTD_compressStream*() :
	// ZSTD_compressStream*() consumes input and flush output if possible, but immediately gives back control to caller,
	// while compression is performed in parallel, within worker thread(s).
	// (note : a strong exception to this rule is when first invocation of ZSTD_compressStream2() sets ZSTD_e_end :
	// in which case, ZSTD_compressStream2() delegates to ZSTD_compress2(), which is always a blocking call).
	// More workers improve speed, but also increase memory usage.
	// Default value is `0`, aka "single-threaded mode" : no worker is spawned,
	// compression is performed inside Caller's thread, and all invocations are blocking
	nb_workers = 400
	// Size of a compression job. This value is enforced only when nbWorkers >= 1.
	// Each compression job is completed in parallel, so this value can indirectly impact the nb of active threads.
	// 0 means default, which is dynamically determined based on compression parameters.
	// Job size must be a minimum of overlap size, or ZSTDMT_JOBSIZE_MIN (= 512 KB), whichever is largest.
	// The minimum size is automatically and transparently enforced.
	job_size = 401
	// Control the overlap size, as a fraction of window size.
	// The overlap size is an amount of data reloaded from previous job at the beginning of a new job.
	// It helps preserve compression ratio, while each job is compressed in parallel.
	// This value is enforced only when nbWorkers >= 1.
	// Larger values increase compression ratio, but decrease speed.
	// Possible values range from 0 to 9 :
	// - 0 means "default" : value will be determined by the library, depending on strategy
	// - 1 means "no overlap"
	// - 9 means "full overlap", using a full window size.
	// Each intermediate rank increases/decreases load size by a factor 2 :
	// 9: full window;  8: w/2;  7: w/4;  6: w/8;  5:w/16;  4: w/32;  3:w/64;  2:w/128;  1:no overlap;  0:default
	// default value varies between 6 and 9, depending on strategy
	overlap_log = 402
	// note : additional experimental parameters are also available
	// within the experimental section of the API.
	// At the time of this writing, they include :
	// zstd_c_rsyncable
	// zstd_c_format
	// zstd_c_force_max_window
	// zstd_c_force_attach_dict
	// zstd_c_literal_compression_mode
	// zstd_c_target_c_block_size
	// zstd_c_src_size_hint
	// zstd_c_enable_dedicated_dict_search
	// zstd_c_stable_in_buffer
	// zstd_c_stable_out_buffer
	// zstd_c_block_delimiters
	// zstd_c_validate_sequences
	// zstd_c_use_block_splitter
	// zstd_c_use_row_match_finder
	// zstd_c_prefetch_c_dict_tables
	// zstd_c_enable_seq_producer_fallback
	// zstd_c_max_block_size
	// Because they are not stable, it's necessary to define ZSTD_STATIC_LINKING_ONLY to access them.
	// note : never ever use experimentalParam? names directly;
	//        also, the enums values themselves are unstable and can still change.
	//
	experimental_param1 = 500
	experimental_param2 = 10
	experimental_param3 = 1000
	experimental_param4 = 1001
	experimental_param5 = 1002
	// experimental_param6  = 1003 is now ZSTD_c_targetCBlockSize
	experimental_param7  = 1004
	experimental_param8  = 1005
	experimental_param9  = 1006
	experimental_param10 = 1007
	experimental_param11 = 1008
	experimental_param12 = 1009
	experimental_param13 = 1010
	experimental_param14 = 1011
	experimental_param15 = 1012
	experimental_param16 = 1013
	experimental_param17 = 1014
	experimental_param18 = 1015
	experimental_param19 = 1016
	experimental_param20 = 1017
}

pub struct Bounds {
pub:
	error       usize
	lower_bound int
	upper_bound int
}

fn C.ZSTD_cParam_getBounds(CParameter) Bounds
fn C.ZSTD_CCtx_setParameter(voidptr, CParameter, int) usize
fn C.ZSTD_CCtx_setPledgedSrcSize(voidptr, u64) usize

pub enum ResetDirective {
	session_only           = 1
	parameters             = 2
	session_and_parameters = 3
}

fn C.ZSTD_CCtx_reset(voidptr, ResetDirective) usize
fn C.ZSTD_compress2(voidptr, voidptr, usize, voidptr, usize) usize

pub enum DParameter {
	// Select a size limit (in power of 2) beyond which
	// the streaming API will refuse to allocate memory buffer
	// in order to protect the host from unreasonable memory requirements.
	// This parameter is only useful in streaming mode, since no internal buffer is allocated in single-pass mode.
	// By default, a decompression context accepts window sizes <= (1 << ZSTD_WINDOWLOG_LIMIT_DEFAULT).
	// Special: value 0 means "use default maximum windowLog".
	window_log_max = 100
	// note : additional experimental parameters are also available
	// within the experimental section of the API.
	// At the time of this writing, they include :
	// ZSTD_d_format
	// zstd_d_stable_out_buffer
	// zstd_d_force_ignore_checksum
	// zstd_d_ref_multipled_dicts
	// zstd_d_disable_huffman_assembly
	// Because they are not stable, it's necessary to define ZSTD_STATIC_LINKING_ONLY to access them.
	// note : never ever use experimentalParam? names directly
	experimental_param1 = 1000
	experimental_param2 = 1001
	experimental_param3 = 1002
	experimental_param4 = 1003
	experimental_param5 = 1004
	experimental_param6 = 1005
}

fn C.ZSTD_dParam_getBounds(DParameter) Bounds
fn C.ZSTD_DCtx_setParameter(voidptr, DParameter, int) usize
fn C.ZSTD_DCtx_reset(voidptr, ResetDirective) usize

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

fn C.ZSTD_createCStream() voidptr
fn C.ZSTD_freeCStream(voidptr) usize

pub enum EndDirective {
	// collect more data, encoder decides when to output compressed result, for optimal compression ratio
	continue = 0
	// flush any data provided so far,
	// it creates (at least) one new block, that can be decoded immediately on reception;
	// frame will continue: any future data can still reference previously compressed data, improving compression.
	// note : multithreaded compression will block to flush as much output as possible.
	flush = 1
	// flush any remaining data _and_ close current frame.
	// note that frame is only closed after compressed data is fully flushed (return value == 0).
	// After that point, any additional data starts a new frame.
	// note : each frame is independent (does not reference any content from previous frame).
	// note : multithreaded compression will block to flush as much output as possible.
	end = 2
}

fn C.ZSTD_compressStream2(voidptr, &OutBuffer, &InBuffer, EndDirective) usize
fn C.ZSTD_CStreamInSize() usize
fn C.ZSTD_CStreamOutSize() usize
fn C.ZSTD_initCStream(voidptr, int) usize
fn C.ZSTD_compressStream(voidptr, &OutBuffer, &InBuffer) usize
fn C.ZSTD_flushStream(voidptr, &OutBuffer) usize
fn C.ZSTD_endStream(voidptr, &OutBuffer) usize

// streaming decompression
fn C.ZSTD_createDStream() voidptr
fn C.ZSTD_freeDStream(voidptr) usize
fn C.ZSTD_initDStream(voidptr) usize
fn C.ZSTD_decompressStream(voidptr, &OutBuffer, &InBuffer) usize
fn C.ZSTD_DStreamInSize() usize
fn C.ZSTD_DStreamOutSize() usize

// version_number returns runtime library version, the value is (MAJOR*100*100 + MINOR*100 + RELEASE).
pub fn version_number() u32 {
	return C.ZSTD_versionNumber()
}

// version_string returns runtime library version, like "1.5.7".
pub fn version_string() string {
	return unsafe { tos_clone(C.ZSTD_versionString()) }
}

// is_error tells if a `usize` function result is an error code.
pub fn is_error(code usize) bool {
	return C.ZSTD_isError(code) == 1
}

// get_error_name provides readable string from an error code.
pub fn get_error_name(code usize) string {
	return unsafe { tos_clone(&u8(C.ZSTD_getErrorName(code))) }
}

// check_zstd checks the zstd error code, and return a error string.
pub fn check_error(code usize) ! {
	if is_error(code) {
		return error(get_error_name(code))
	}
}

// min_c_level returns minimum negative compression level allowed.
pub fn min_c_level() int {
	return C.ZSTD_minCLevel()
}

// max_c_level returns maximum compression level available.
pub fn max_c_level() int {
	return C.ZSTD_maxCLevel()
}

// default_c_level returns default compression level.
pub fn default_c_level() int {
	return C.ZSTD_defaultCLevel()
}

@[params]
pub struct CompressParams {
pub:
	// 1~22
	compression_level int = default_c_level()
	// how many threads will be spawned to compress in parallel
	nb_threads    int      = 1
	checksum_flag bool     = true
	strategy      Strategy = .default
}

// compresses an array of bytes using zstd and returns the compressed bytes in a new array
// extra compression parameters can be set by `params`
// Example: compressed := zstd.compress(b)!
pub fn compress(data []u8, params CompressParams) ![]u8 {
	dst_capacity := C.ZSTD_compressBound(data.len)
	check_error(dst_capacity)!
	mut dst := []u8{len: int(dst_capacity)}
	mut cctx := new_cctx(params)!
	defer {
		cctx.free_cctx()
	}
	size := C.ZSTD_compress2(cctx.ctx, dst.data, dst.len, data.data, data.len)
	check_error(size)!
	return dst[..size]
}

@[params]
pub struct DecompressParams {
pub:
	window_log_max int
}

// decompresses an array of bytes using zstd and returns the decompressed bytes in a new array
// extra decompression parameters can be set by `params`
// Example: decompressed := zstd.decompress(b)!
pub fn decompress(data []u8, params DecompressParams) ![]u8 {
	dst_capacity := C.ZSTD_getFrameContentSize(data.data, frame_header_size_max)
	if dst_capacity == content_size_unknown {
		return error('The size cannot be determined, try use streaming mode to decompress data?')
	} else if dst_capacity == content_size_error {
		return error('An error occurred (e.g. invalid magic number, srcSize too small)')
	} else if dst_capacity == 0 {
		return error('The frame is valid but empty')
	}
	mut dst := []u8{len: int(dst_capacity)}
	decompressed_size := C.ZSTD_decompress(dst.data, dst.len, data.data, data.len)
	check_error(decompressed_size)!
	return dst[..decompressed_size]
}

pub struct CCtx {
mut:
	ctx &C.ZSTD_CCtx
}

// new_cctx create a compression context.
// extra compression parameters can be set by `params`
pub fn new_cctx(params CompressParams) !&CCtx {
	mut ctx := C.ZSTD_createCCtx()
	if isnil(ctx) {
		return error('new_cctx() failed!')
	}
	mut cctx := &CCtx{ctx}
	cctx.set(.compression_level, params.compression_level)!
	$if !(tinyc && windows) {
		// TODO: tinyc on windows doesn't support multiple thread
		cctx.set(.nb_workers, params.nb_threads)!
	}
	cctx.set(.checksum_flag, if params.checksum_flag { 1 } else { 0 })!
	cctx.set(.strategy, int(params.strategy))!
	return cctx
}

// set_parameter set compression parameter `c_param` to value `val`
pub fn (mut c CCtx) set(c_param CParameter, val int) ! {
	check_error(C.ZSTD_CCtx_setParameter(c.ctx, c_param, val))!
}

// compress_stream2 do stream compress on `input`, and store compressed data in `output`.
// `mode`:
// 	.zstd_e_continue => continue stream compression.
// 	.zstd_e_flush => flush data
// 	.zstd_e_end => it is the last frame
pub fn (mut c CCtx) compress_stream2(output &OutBuffer, input &InBuffer, mode EndDirective) !usize {
	res := C.ZSTD_compressStream2(c.ctx, output, input, mode)
	check_error(res)!
	return res
}

// free_cctx free a compression context.
pub fn (mut c CCtx) free_cctx() usize {
	return C.ZSTD_freeCCtx(c.ctx)
}

struct C.ZSTD_CCtx {}

struct C.ZSTD_DCtx {}

pub struct DCtx {
mut:
	ctx &C.ZSTD_DCtx
}

// new_dctx creates a decompression context.
// extra decompression parameters can be set by `params`
pub fn new_dctx(params DecompressParams) !&DCtx {
	mut ctx := C.ZSTD_createDCtx()
	if isnil(ctx) {
		return error('new_dctx() failed!')
	}
	mut dctx := &DCtx{ctx}
	dctx.set(.window_log_max, params.window_log_max)!
	return dctx
}

// set_parameter sets decompression parameter `d_param` to value `val`
pub fn (mut d DCtx) set(d_param DParameter, val int) ! {
	check_error(C.ZSTD_DCtx_setParameter(d.ctx, d_param, val))!
}

// decompress_stream do stream decompress on `input`, and store decompressed data in `output`.
// return remaining bytes in `input` stream
pub fn (mut d DCtx) decompress_stream(output &OutBuffer, input &InBuffer) !usize {
	res := C.ZSTD_decompressStream(d.ctx, output, input)
	check_error(res)!
	return res
}

// free_cctx free a compression context
pub fn (mut d DCtx) free_dctx() usize {
	return C.ZSTD_freeDCtx(d.ctx)
}

// store_array compress an `array`'s data, and store it to file `fname`.
// extra compression parameters can be set by `params`
// WARNING: Because struct padding, some data in struct may be marked unused.
// So, when `store_array`, it will cause memory fsanitize fail with 'use-of-uninitialized-value'.
// It can be safely ignored.
// For example, following struct may cause memory fsanitize fail:
// struct MemoryTrace {
// 	operation u8
// 	address   u64
// 	size      u8
// }
// By changing it into following , you can pass the memory fsanitize check :
// struct MemoryTrace {
// 	operation u64
// 	address   u64
// 	size      u64
// }
pub fn store_array[T](fname string, array []T, params CompressParams) ! {
	mut fout := os.open_file(fname, 'wb')!
	mut cctx := new_cctx(params)!
	defer {
		cctx.free_cctx()
		fout.close()
	}

	mut buf_out := []u8{len: buf_out_size}
	mut input := &InBuffer{}
	mut output := &OutBuffer{}
	// first, write the array.len to file
	mut len_buf := []u8{len: 8}
	binary.little_endian_put_u64(mut len_buf, u64(array.len))
	input.src = len_buf.data
	input.size = 8
	input.pos = 0
	output.dst = buf_out.data
	output.size = buf_out_size
	output.pos = 0
	mut remaining := cctx.compress_stream2(output, input, .flush)!
	fout.write(buf_out[..output.pos])!
	// then, write the array.data to file
	input.src = array.data
	input.size = usize(array.len * int(sizeof(T)))
	input.pos = 0
	output.dst = buf_out.data
	output.size = buf_out_size
	output.pos = 0
	for {
		output.dst = buf_out.data
		output.size = buf_out_size
		output.pos = 0
		remaining = cctx.compress_stream2(output, input, .end)!
		fout.write(buf_out[..output.pos])!
		if remaining == 0 {
			break
		}
	}
}

// load_array return an array which data is decompressed from a file `fname`.
// extra decompression parameters can be set by `params`
pub fn load_array[T](fname string, params DecompressParams) ![]T {
	mut fin := os.open_file(fname, 'rb')!
	mut dctx := new_dctx(params)!
	defer {
		dctx.free_dctx()
		fin.close()
	}

	mut buf_in := []u8{len: buf_in_size}
	mut len_buf := []u8{len: 8}
	mut input := &InBuffer{}
	mut output := &OutBuffer{}
	mut last_ret := usize(0)
	mut last_chunk := false
	// first, read the array.len from file
	mut read_len := fin.read(mut buf_in)!
	last_chunk = read_len < buf_in.len
	input.src = buf_in.data
	input.size = usize(read_len)
	input.pos = 0
	output.dst = len_buf.data
	output.size = usize(len_buf.len)
	output.pos = 0
	last_ret = dctx.decompress_stream(output, input)!
	len := binary.little_endian_u64(len_buf)
	// then, read the array.data from file
	mut result := []T{len: int(len)}
	output.dst = result.data
	output.size = usize(result.len) * sizeof(T)
	output.pos = 0
	last_ret = dctx.decompress_stream(output, input)!
	for !last_chunk {
		read_len = fin.read(mut buf_in)!
		last_chunk = read_len < buf_in.len
		input.size = usize(read_len)
		input.pos = 0
		for input.pos < input.size {
			last_ret = dctx.decompress_stream(output, input)!
		}
		if read_len < buf_in.len {
			break
		}
	}
	if last_ret != 0 {
		// The last return value from ZSTD_decompressStream did not end on a
		// frame, but we reached the end of the file! We assume this is an
		// error, and the input was truncated.
		return error('EOF before end of stream: ${last_ret}')
	}
	return result
}
