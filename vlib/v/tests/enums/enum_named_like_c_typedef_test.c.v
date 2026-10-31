#include "@VMODROOT/vlib/v/tests/testdata/enum_named_like_c_typedef.h"

// Each enum below shares its name with a type that the header declares. The C type
// emitted for a main module enum must not redeclare the header's typedef.
enum MediaState {
	invalid = -1
	stopped
	paused
	playing
}

enum MediaConfigFlag {
	io_buffer
	video_queue
	audio_queue
}

@[flag]
enum MediaLoadFlag {
	no_audio
	no_video
}

enum MediaChannel as u8 {
	left
	right
}

struct Media {
mut:
	state    MediaState = .stopped
	channel  MediaChannel
	load     MediaLoadFlag
	channels []MediaChannel
}

fn C.c_media_state(playing int) int
fn C.c_media_flag_value(flag MediaConfigFlag) int
fn C.c_media_load_flags() int
fn C.c_media_channel() int

fn media_state(playing bool) MediaState {
	return unsafe { MediaState(C.c_media_state(int(playing))) }
}

fn (c MediaChannel) other() MediaChannel {
	return if c == .left { MediaChannel.right } else { MediaChannel.left }
}

fn test_plain_enums_named_like_c_typedefs() {
	assert media_state(true) == .playing
	assert media_state(false) == .invalid
	assert media_state(true).str() == 'playing'
	assert int(MediaState.invalid) == -1
	assert C.c_media_flag_value(.audio_queue) == 20
	assert C.c_media_flag_value(MediaConfigFlag.video_queue) == 10
	assert MediaConfigFlag.from('video_queue')! == .video_queue
	assert '${MediaConfigFlag.io_buffer}' == 'io_buffer'
}

fn test_flag_enum_named_like_c_typedef() {
	mut load := unsafe { MediaLoadFlag(C.c_media_load_flags()) }
	assert load == .no_video
	load.set(.no_audio)
	assert load.has(.no_audio | .no_video)
	assert load.str() == 'MediaLoadFlag{.no_audio | .no_video}'
}

fn test_backed_enum_named_like_c_typedef() {
	channel := unsafe { MediaChannel(u8(C.c_media_channel())) }
	assert channel == .right
	assert channel.other() == .left
	assert sizeof(MediaChannel) == 1
	mut media := Media{
		channel:  channel
		channels: [MediaChannel.left, .right]
	}
	media.load.set(.no_audio)
	assert media.state == .stopped
	assert media.channels.map(it.other()) == [MediaChannel.right, .left]
	assert media.str().contains('channel: right')
}
