// Every typedef name here is also the name of a V enum declared by
// vlib/v/tests/enums/enum_named_like_c_typedef_test.c.v.
#ifndef V_ENUM_NAMED_LIKE_C_TYPEDEF_H
#define V_ENUM_NAMED_LIKE_C_TYPEDEF_H

typedef enum
{
	MEDIA_STATE_INVALID = -1,
	MEDIA_STATE_STOPPED,
	MEDIA_STATE_PAUSED,
	MEDIA_STATE_PLAYING
} MediaState;

typedef enum {
	MEDIA_IO_BUFFER,
	MEDIA_VIDEO_QUEUE,
	MEDIA_AUDIO_QUEUE
} MediaConfigFlag;

typedef enum MediaLoadFlag {
	MEDIA_LOAD_NO_AUDIO = 1 << 0,
	MEDIA_LOAD_NO_VIDEO = 1 << 1
} MediaLoadFlag;

typedef unsigned char MediaChannel;

static int c_media_state(int playing) {
	MediaState state = playing ? MEDIA_STATE_PLAYING : MEDIA_STATE_INVALID;
	return (int)state;
}

static int c_media_flag_value(int flag) {
	MediaConfigFlag config_flag = (MediaConfigFlag)flag;
	return (int)config_flag * 10;
}

static int c_media_load_flags(void) {
	MediaLoadFlag flags = MEDIA_LOAD_NO_VIDEO;
	return (int)flags;
}

static int c_media_channel(void) {
	MediaChannel channel = 1;
	return (int)channel;
}

#endif
