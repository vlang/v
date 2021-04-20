module audio

#flag -I @VEXEROOT/thirdparty/sokol
#define SOKOL_IMPL
#include "sokol_audio.h"
#flag linux -lasound
#flag darwin -framework AudioToolbox
#flag windows -lole32
//
pub type FNStreamingCB = fn (buffer &f32, num_frames int, num_channels int)

pub type FnStreamingCBWithUserData = fn (buffer &f32, num_frames int, num_channels int, user_data voidptr)

pub fn (x FNStreamingCB) str() string {
	return '&FNStreamingCB{ ${ptr_str(x)} }'
}

pub fn (x FnStreamingCBWithUserData) str() string {
	return '&FnStreamingCBWithUserData{ ${ptr_str(x)} }'
}

//
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

fn C.saudio_expect() int

fn C.saudio_push(frames &f32, num_frames int) int

// audio.setup - setup sokol-audio
pub fn setup(desc C.saudio_desc) {
	C.saudio_setup(&desc)
}

// audio.shutdown - shutdown sokol-audio
pub fn shutdown() {
	C.saudio_shutdown()
}

// audio.is_valid - true after setup if audio backend was successfully initialized
pub fn is_valid() bool {
	return C.saudio_isvalid()
}

// audio.userdata - return the saudio_desc.user_data pointer
pub fn user_data() voidptr {
	return C.saudio_userdata()
}

// audio.query - return a copy of the original saudio_desc struct
pub fn query() C.saudio_desc {
	return C.saudio_query_desc()
}

// audio.sample_rate - actual sample rate
pub fn sample_rate() int {
	return C.saudio_sample_rate()
}

// audio.buffer_frames - return actual backend buffer size in number of frames
pub fn buffer_frames() int {
	return C.saudio_buffer_frames()
}

// audio.channels - actual number of channels
pub fn channels() int {
	return C.saudio_channels()
}

// audio.expect - get current number of frames to fill packet queue; use in combination with audio.push/2
pub fn expect() int {
	return C.saudio_expect()
}

// audio.push - push sample frames from main thread, returns number of frames actually pushed
pub fn push(frames &f32, num_frames int) int {
	return C.saudio_push(frames, num_frames)
}

//
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

pub fn min(x int, y int) int {
	if x < y {
		return x
	}
	return y
}

pub fn max(x int, y int) int {
	if x < y {
		return y
	}
	return x
}
