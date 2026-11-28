module audio

fn test_audio_push_frames() {
	assert audio_push_frames(8, 1) == 8
	assert audio_push_frames(8, 2) == 4
	assert audio_push_frames(8, 4) == 2
	assert audio_push_frames(9, 2) == 4
	assert audio_push_frames(8, 0) == 0
	assert audio_push_frames(8, -1) == 0
	assert audio_push_frames(0, 2) == 0
}
