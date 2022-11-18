module audio

$if linux {
	// provide a nicer error for the user that does not have ALSA installed
	#include <alsa/asoundlib.h> # Please install the `libasound2-dev` package
}

#flag -I @VEXEROOT/thirdparty/sokol
#define SOKOL_IMPL
#include "sokol_audio.h"
#flag linux -lasound
#flag darwin -framework AudioToolbox
#flag windows -lole32

// callback function for `stream_cb` in [[C.saudio_desc](#C.saudio_desc)] when calling [audio.setup()](#setup)
//
// sokol callback functions run in a separate thread
//
// This function will be called with a reference to the C buffer and the maximum number of frames and channels
// the audio backend is expecting in its buffer.
//
// Terms:
// - *sample* - a 32-bit floating point number from `-1.0` to `+1.0` representing the waveform amplitude at that instant
// - *frame* - one sample for each channel at that instant
//
// To determine the number of samples expected, do `num_frames * num_channels`.
// Then, write up to that many `f32` samples into `buffer` using unsafe operations.
//
// Do not write more data to the buffer than it is requesting, but you may write less. The buffer is initialized with
// zeroes, so unwritten data will result in audio silence.
// Example: unsafe { C.memcpy(buffer, &samples, samples.len * int(sizeof(f32))) }
// Example: unsafe { mut b := buffer; for i, sample in samples { b[i] = sample } }
pub type FNStreamingCB = fn (buffer &f32, num_frames int, num_channels int)

// callback function for `stream_userdata_cb` to use in `C.saudio_desc` when calling [audio.setup()](#setup)
//
// sokol callback functions run in a separate thread
//
// This function operates the same way as [[FNStreamingCB](#FNStreamingCB)] but it passes customizable `user_data` to the
// callback. This is the method to use if your audio data is stored in a struct or array. Identify the
// `user_data` when you call `audio.setup()` and that object will be passed to the callback as the last arg.
// Example: mut soundbuffer := []f32
// Example: soundbuffer << previously_parsed_wavfile_bytes
// Example: audio.setup(stream_userdata_cb: mycallback, user_data: soundbuffer)
// Example: fn mycallback(buffer &f32, num_frames int, num_channels int, mut sb []f32) { ... }
pub type FnStreamingCBWithUserData = fn (buffer &f32, num_frames int, num_channels int, user_data voidptr)

pub fn (x FNStreamingCB) str() string {
	return '&FNStreamingCB{ ${ptr_str(x)} }'
}

pub fn (x FnStreamingCBWithUserData) str() string {
	return '&FnStreamingCBWithUserData{ ${ptr_str(x)} }'
}

// only one of `stream_cb` or `stream_userdata_cb` should be used
//
// default values (internal to sokol C library):
//
// | variable      | default  | note |
// | :-----------  | -------: | :--------- |
// | sample_rate   | 44100    | higher sample rates take more memory but are higher quality |
// | num_channels  | 1        | for stereo sound, this should be 2 |
// | buffer_frames | 2048     | buffer size in frames, larger is more latency, smaller means higher CPU |
// | packet_frames | 128      | push model only, number of frames that will be pushed in each packet |
// | num_packets   | 64       | for push model only, number of packets in the backend ringbuffer |
pub struct C.saudio_desc {
	sample_rate        int
	num_channels       int
	buffer_frames      int
	packet_frames      int
	num_packets        int
	stream_cb          FNStreamingCB
	stream_userdata_cb FnStreamingCBWithUserData
	user_data          voidptr
}

fn C.saudio_setup(desc &C.saudio_desc)

fn C.saudio_shutdown()

fn C.saudio_isvalid() bool

fn C.saudio_userdata() voidptr

fn C.saudio_query_desc() C.saudio_desc

fn C.saudio_sample_rate() int

fn C.saudio_buffer_frames() int

fn C.saudio_channels() int

fn C.saudio_suspended() bool

fn C.saudio_expect() int

fn C.saudio_push(frames &f32, num_frames int) int

// setup - setup sokol-audio
pub fn setup(desc &C.saudio_desc) {
	C.saudio_setup(desc)
}

// shutdown - shutdown sokol-audio
pub fn shutdown() {
	C.saudio_shutdown()
}

// is_valid - true after setup if audio backend was successfully initialized
pub fn is_valid() bool {
	return C.saudio_isvalid()
}

// userdata - return the saudio_desc.user_data pointer
pub fn user_data() voidptr {
	return C.saudio_userdata()
}

// query - return a copy of the original saudio_desc struct
pub fn query() C.saudio_desc {
	return C.saudio_query_desc()
}

// sample_rate - return the actual sample rate
pub fn sample_rate() int {
	return C.saudio_sample_rate()
}

// buffer_frames - return the actual backend buffer size in number of frames
pub fn buffer_frames() int {
	return C.saudio_buffer_frames()
}

// channels - return the actual number of channels
pub fn channels() int {
	return C.saudio_channels()
}

// suspended returns true if audio context is currently suspended
// (only in WebAudio backend, all other backends return false)
pub fn suspended() bool {
	return C.saudio_suspended()
}

// expect - get current number of frames to fill packet queue; use in combination with audio.push
pub fn expect() int {
	return C.saudio_expect()
}

// push - push sample frames from main thread, returns number of frames actually pushed
pub fn push(frames &f32, num_frames int) int {
	return C.saudio_push(frames, num_frames)
}

// fclamp - helper function to 'clamp' a number to a certain range
// Example: realsample := audio.fclamp(sample, -1.0, 1.0)
[inline]
pub fn fclamp(x f32, flo f32, fhi f32) f32 {
	if x > fhi {
		return fhi
	}
	if x < flo {
		return flo
	}
	return x
}

// min - helper function to return the smaller of two numbers
//
// NOTE: math.min returns `f32` values, this returns `int` values
// Example: smaller := audio.min(1, 5) // smaller == 1
pub fn min(x int, y int) int {
	if x < y {
		return x
	}
	return y
}

// max - helper function to return the larger of two numbers
//
// NOTE: math.max returns `f32` values, this returns `int` values
// Example: larger := audio.max(1, 5) // larger == 5
pub fn max(x int, y int) int {
	if x < y {
		return y
	}
	return x
}
