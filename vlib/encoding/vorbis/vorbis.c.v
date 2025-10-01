// Copyright (c) 2025 Delyan Angelov. All rights reserved. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.
module vorbis

#flag -I @VEXEROOT/thirdparty/stb_vorbis
#flag @VEXEROOT/thirdparty/stb_vorbis/stb_vorbis.o
#include "stb_vorbis.h"

@[typedef]
pub struct C.stb_vorbis_alloc {
pub mut:
	alloc_buffer                 &char
	alloc_buffer_length_in_bytes i32
}

@[typedef]
pub struct C.stb_vorbis {}

@[typedef]
pub struct C.stb_vorbis_info {
pub mut:
	sample_rate                u32
	channels                   i32
	setup_memory_required      u32
	setup_temp_memory_required u32
	max_frame_size             i32
}

@[typedef]
struct C.stb_vorbis_comment {
pub mut:
	vendor              &char
	comment_list_length i32
	comment_list        &&char
}

pub enum VorbisErrorCode {
	no_error
	need_more_data              = 1                   // not a real error
	invalid_api_mixing    // can not mix API modes
	out_of_memory         // not enough memory
	not_supported         // uses floor 0
	too_many_channels     // STB_VORBIS_MAX_CHANNELS is too small
	file_open_failure     // fopen() failed
	seek_without_length   // can't seek in unknown-length file
	unexpected_eof              = 10                  // file is truncated?
	seek_invalid          // seek past EOF
	vorbis_invalid_setup        = 20                  // vorbis decoding error (corrupt/invalid stream)
	vorbis_invalid_stream // vorbis decoding error
	ogg_missing_capture_pattern = 30
	ogg_invalid_stream_structure_version
	ogg_continued_packet_flag_invalid
	ogg_incorrect_stream_serial_number
	ogg_invalid_first_page
	ogg_bad_packet_type
	ogg_cant_find_last_page
	ogg_seek_failed
	ogg_skeleton_not_supported
}

// stb_vorbis_get_info retrieve general information about the file
pub fn C.stb_vorbis_get_info(f &C.stb_vorbis) C.stb_vorbis_info

// stb_vorbis_get_comment retrieve ogg comments
pub fn C.stb_vorbis_get_comment(f &C.stb_vorbis) C.stb_vorbis_comment

// stb_vorbis_get_error return the last detected error (and clears it too)
pub fn C.stb_vorbis_get_error(f &C.stb_vorbis) VorbisErrorCode

// stb_vorbis_close closes an ogg vorbis file and frees all the memory used for it
pub fn C.stb_vorbis_close(f &C.stb_vorbis)

// stb_vorbis_get_sample_offset returns the offset in samples, from the start of the file,
// that will be returned by the next decode, if it is known or -1 otherwise.
// NOTE: NOT WORKING YET after a seek with PULLDATA API
pub fn C.stb_vorbis_get_sample_offset(f &C.stb_vorbis) i32

// stb_vorbis_get_file_offset returns the current seek point within the file, or offset
// from the beginning of the memory buffer. In pushdata mode it returns 0.
pub fn C.stb_vorbis_get_file_offset(f &C.stb_vorbis) u32

// stb_vorbis_open_pushdata create a vorbis decoder by passing in the initial data block
// containing the ogg&vorbis headers (you don't need to do parse them, just provide
// the first N bytes of the file--you're told if it's not enough, see below)
// on success, returns an stb_vorbis *, does not set error, returns the amount of
// data parsed/consumed on this call in *datablock_memory_consumed_in_bytes;
// on failure, returns NULL on error and sets *error, does not change *datablock_memory_consumed
// if returns NULL and *error is VORBIS_need_more_data, then the input block was
// incomplete and you need to pass in a larger block from the start of the file

pub fn C.stb_vorbis_open_pushdata(const_datablock &u8, datablock_length_in_bytes i32, datablock_memory_consumed_in_bytes &i32, xerror &VorbisErrorCode, const_alloc_buffer &C.stb_vorbis_alloc) &C.stb_vorbis

// stb_vorbis_decode_frame_pushdata decode a frame of audio sample data if possible from the passed-in data block
// Returns the number of bytes we used from `const_datablock`.
// possible cases:
//     0 bytes used, 0 samples output (need more data)
//     N bytes used, 0 samples output (resynching the stream, keep going)
//     N bytes used, M samples output (one frame of data)
// note that after opening a file, you will ALWAYS get one N-bytes,0-sample
// frame, because Vorbis always "discards" the first frame.
//
// Note that on resynch, stb_vorbis will rarely consume all of the buffer,
// instead only datablock_length_in_bytes-3 or less. This is because it wants
// to avoid missing parts of a page header if they cross a datablock boundary,
// without writing state-machiney code to record a partial detection.
//
// The number of channels returned are stored in *channels (which can be
// NULL -- it is always the same as the number of channels reported by
// get_info). *output will contain an array of float* buffers, one per
// channel. In other words, (*output)[0][0] contains the first sample from
// the first channel, and (*output)[1][0] contains the first sample from
// the second channel.
//
// *output points into stb_vorbis's internal output buffer storage; these
// buffers are owned by stb_vorbis and application code should not free
// them or modify their contents. They are transient and will be overwritten
// once you ask for more data to get decoded, so be sure to grab any data
// you need before then.
// `channels` is a place to write number of float * buffers
// `output` is a place to write float ** array of float * buffers
// `samples` is a place to write number of output samples
pub fn C.stb_vorbis_decode_frame_pushdata(f &C.stb_vorbis, const_datablock &u8, datablock_length_in_bytes i32, channels &i32, output &&&f32, samples &i32) i32

// stb_vorbis_flush_pushdata inform stb_vorbis that your next datablock will not be contiguous with
// previous ones (e.g. you've seeked in the data); future attempts to decode
// frames will cause stb_vorbis to resynchronize (as noted above), and
// once it sees a valid Ogg page (typically 4-8KB, as large as 64KB), it
// will begin decoding the _next_ frame.
// If you want to seek using pushdata, you need to seek in your file, then
// call stb_vorbis_flush_pushdata(), then start calling decoding, then once
// decoding is returning you data, call stb_vorbis_get_sample_offset, and
// if you don't like the result, seek your file again and repeat.
pub fn C.stb_vorbis_flush_pushdata(f &C.stb_vorbis)

// stb_vorbis_decode_filename assumes stb_vorbis is allowed to pull data
// from a source: either a block of memory containing the _entire_ vorbis stream,
// or a &C.FILE that you or it create, or possibly some other reading mechanism
// if you go modify the source to replace the &C.FILE case with some kind
// of callback to your code. (But if you don't support seeking, you may
// just want to go ahead and use pushdata.)
// The return value is the number of samples decoded, or -1 if the file could not be opened or was not an ogg vorbis file.
// When you're done with it, just C.free() the pointer returned in *output.
pub fn C.stb_vorbis_decode_filename(const_filename &char, channels &i32, sample_rate &i32, output &&i16) i32

// stb_vorbis_decode_memory decodes an entire file and output the data interleaved
// into a malloc()ed buffer stored in *output.
// The return value is the number of samples decoded, or -1 if the file could not be opened or was not an ogg vorbis file.
// When you're done with it, just C.free() the pointer returned in *output.
pub fn C.stb_vorbis_decode_memory(const_mem &u8, len i32, channels &i32, sample_rate &i32, output &&i16) i32

// stb_vorbis_open_memory creates an ogg vorbis decoder from an ogg vorbis stream
// in memory (note this must be the entire stream!).
// On failure, returns NULL and sets *error
pub fn C.stb_vorbis_open_memory(const_data &u8, len i32, xerror &VorbisErrorCode, const_alloc_buffer &C.stb_vorbis_alloc) &C.stb_vorbis

// stb_vorbis_open_filename creates an ogg vorbis decoder from a filename via fopen().
// On failure, returns NULL and sets *error (possibly to VORBIS_file_open_failure).
pub fn C.stb_vorbis_open_filename(const_filename &char, xerror &VorbisErrorCode, const_alloc_buffer &C.stb_vorbis_alloc) &C.stb_vorbis

// stb_vorbis_open_file creates an ogg vorbis decoder from an open &C.FILE,
// looking for a stream at the _current_ seek point (ftell).
// On failure, returns NULL and sets *error.
// Note: stb_vorbis must "own" this stream; if you seek it in between
// calls to stb_vorbis, it will become confused. Moreover, if you attempt to
// perform stb_vorbis_seek_*() operations on this file, it will assume it
// owns the _entire_ rest of the file after the start point. Use the next
// function, stb_vorbis_open_file_section(), to limit it.
pub fn C.stb_vorbis_open_file(f &C.FILE, close_handle_on_close i32, xerror &VorbisErrorCode, const_alloc_buffer &C.stb_vorbis_alloc) &C.stb_vorbis

// stb_vorbis_open_file_section creates an ogg vorbis decoder from an open &C.FILE,
// looking for a stream at the _current_ seek point (ftell).
// The stream will be of length 'len' bytes.
// On failure, returns NULL and sets *error.
// Note: stb_vorbis must "own" this stream; if you seek it in between calls to stb_vorbis,
// it will become confused.
pub fn C.stb_vorbis_open_file_section(f &C.FILE, close_handle_on_close i32, xerror &VorbisErrorCode, const_alloc_buffer &C.stb_vorbis_alloc, len u32) &C.stb_vorbis

// stb_vorbis_seek_frame seeks in the Vorbis file to (approximately) 'sample_number'.
// After calling seek_frame(), the next call to get_frame_*() will include
// the specified sample.
pub fn C.stb_vorbis_seek_frame(f &C.stb_vorbis, sample_number u32) i32

// stb_vorbis_seek seeks in the Vorbis file to (approximately) 'sample_number'.
// After calling stb_vorbis_seek(), the next call to stb_vorbis_get_samples_*
// will start with the specified sample. If you do not need to seek to EXACTLY
// the target sample when using get_samples_*, you can also use seek_frame().
pub fn C.stb_vorbis_seek(f &C.stb_vorbis, sample_number u32) i32

// stb_vorbis_seek_start is equivalent to stb_vorbis_seek(f,0)
pub fn C.stb_vorbis_seek_start(f &C.stb_vorbis) i32

// stb_vorbis_stream_length_in_samples returns the total length of the vorbis stream in samples
pub fn C.stb_vorbis_stream_length_in_samples(f &C.stb_vorbis) u32

// stb_vorbis_stream_length_in_seconds returns the total length of the vorbis stream in seconds
pub fn C.stb_vorbis_stream_length_in_seconds(f &C.stb_vorbis) f32

// stb_vorbis_get_frame_float decodes the next frame and returns the number of samples.
// The number of channels returned are stored in *channels (which can be NULL;
// it is always the same as the number of channels reported by get_info).
// `*output` will contain an array of float* buffers, one per channel.
// These outputs will be overwritten on the next call to stb_vorbis_get_frame_*.
// You generally should not intermix calls to stb_vorbis_get_frame_*()
// and stb_vorbis_get_samples_*(), since the latter calls the former.
pub fn C.stb_vorbis_get_frame_float(f &C.stb_vorbis, channels &i32, output &&&f32) i32

// stb_vorbis_get_frame_short_interleaved decodes the next frame and returns the number
// of *samples* per channel.
// Note that for interleaved data, you pass in the number of shorts (the
// size of your array), but the return value is the number of samples per
// channel, not the total number of samples.//
// The data is coerced to the number of channels you request according to the
// channel coercion rules (see below). You must pass in the size of your
// buffer(s) so that stb_vorbis will not overwrite the end of the buffer.
// The maximum buffer size needed can be gotten from get_info(); however,
// the Vorbis I specification implies an absolute maximum of 4096 samples
// per channel.
// Channel coercion rules:
//    Let M be the number of channels requested, and N the number of channels present,
//    and Cn be the nth channel; let stereo L be the sum of all L and center channels,
//    and stereo R be the sum of all R and center channels (channel assignment from the
//    vorbis spec).
//        M    N       output
//        1    k      sum(Ck) for all k
//        2    *      stereo L, stereo R
//        k    l      k > l, the first l channels, then 0s
//        k    l      k <= l, the first k channels
//    Note that this is not _good_ surround etc. mixing at all! It's just so
//    you get something useful.
pub fn C.stb_vorbis_get_frame_short_interleaved(f &C.stb_vorbis, num_c i32, buffer &i16, num_shorts i32) i32

// stb_vorbis_get_frame_short similar to stb_vorbis_get_frame_short_interleaved, but without interleaving.
pub fn C.stb_vorbis_get_frame_short(f &C.stb_vorbis, num_c i32, buffer &&i16, num_samples i32) i32

// stb_vorbis_get_samples_float_interleaved gets num_floats, not necessarily on a frame boundary.
// This requires buffering so you have to supply the buffers. DOES NOT APPLY THE COERCION RULES.
// Returns the number of samples stored per channel; it may be less than requested
// at the end of the file. If there are no more samples in the file, returns 0.
pub fn C.stb_vorbis_get_samples_float_interleaved(f &C.stb_vorbis, channels i32, buffer &f32, num_floats i32) i32

// stb_vorbis_get_samples_float gets num_samples samples, not necessarily on a frame boundary.
// See also stb_vorbis_get_samples_float_interleaved.
pub fn C.stb_vorbis_get_samples_float(f &C.stb_vorbis, channels i32, buffer &&f32, num_samples i32) i32

// stb_vorbis_get_samples_short_interleaved gets num_samples samples, not necessarily on a frame boundary.
// This requires buffering so you have to supply the buffers. Applies the coercion rules above
// to produce 'channels' channels. Returns the number of samples stored per channel;
// it may be less than requested at the end of the file. If there are no more
// samples in the file, returns 0.
pub fn C.stb_vorbis_get_samples_short_interleaved(f &C.stb_vorbis, channels i32, buffer &i16, num_shorts i32) i32

// stb_vorbis_get_samples_short is similar to stb_vorbis_get_samples_short_interleaved .
pub fn C.stb_vorbis_get_samples_short(f &C.stb_vorbis, channels i32, buffer &&i16, num_samples i32) i32
