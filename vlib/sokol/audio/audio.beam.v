// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
module audio

import sokol.memory

pub type FNStreamingCB = fn (buffer &f32, num_frames int, num_channels int)

pub type FnStreamingCBWithUserData = fn (buffer &f32, num_frames int, num_channels int, user_data voidptr)

pub fn (x FNStreamingCB) str() string {
	return '&FNStreamingCB{ ${ptr_str(x)} }'
}

pub fn (x FnStreamingCBWithUserData) str() string {
	return '&FnStreamingCBWithUserData{ ${ptr_str(x)} }'
}

pub struct AudioAllocator {
pub mut:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

pub struct AudioLogger {
pub mut:
	func      memory.FnLogCb = unsafe { nil }
	user_data voidptr
}

@[params]
pub struct AudioDesc {
pub:
	sample_rate        int
	num_channels       int
	buffer_frames      int
	packet_frames      int
	num_packets        int
	stream_cb          FNStreamingCB              = unsafe { nil }
	stream_userdata_cb FnStreamingCBWithUserData  = unsafe { nil }
pub mut:
	user_data voidptr
	allocator AudioAllocator
	logger    AudioLogger
}

pub fn setup(desc AudioDesc) {
}

pub fn shutdown() {
}

pub fn is_valid() bool {
	return false
}

pub fn user_data() voidptr {
	return unsafe { nil }
}

pub fn query() AudioDesc {
	return AudioDesc{}
}

pub fn sample_rate() int {
	return 44100
}

pub fn buffer_frames() int {
	return 2048
}

pub fn channels() int {
	return 1
}

pub fn suspended() bool {
	return false
}

pub fn expect() int {
	return 0
}

pub fn push(frames &f32, num_frames int) int {
	return 0
}

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
