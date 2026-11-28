module fx

import gg

fn test_voice_play_and_stop() {
	sound := &gg.Sound{ samples: [i16(1000), 2000, 3000, 4000] }
	v := new_voice(sound)
	assert !v.playing
	v.play()
	assert v.playing
	v.set_volume(2)
	assert v.volume == 1
	v.set_pan(-5)
	assert v.pan == -1
	v.stop()
	assert !v.playing
}

fn test_voice_mix() {
	sound := &gg.Sound{ samples: [i16(32767), 32767, 32767, 32767] }
	v := new_voice(sound)
	v.set_volume(0.5)
	v.play()
	mut buffer := [f32(0), 0, 0, 0, 0, 0, 0, 0]
	v.mix(mut buffer, 4, 2)
	assert buffer[0] > 0 && buffer[1] > 0
	assert buffer[0] == buffer[1]
	assert v.pos == 4
	assert !v.playing
}

fn test_voice_loop_and_pan() {
	sound := &gg.Sound{ samples: [i16(1000), 1000] }
	mut v := new_voice(sound)
	v.looping = true
	v.set_pan(-1)
	v.play()
	mut buffer := [f32(0), 0, 0, 0]
	v.mix(mut buffer, 2, 2)
	assert buffer[0] > 0
	assert v.playing
	assert v.pos == 0
}

fn test_voice_loop_wraps_partial_frame() {
	sound := &gg.Sound{ samples: [i16(1000), 2000, 3000, 4000] }
	mut v := new_voice(sound)
	v.looping = true
	v.play()
	mut buffer := [f32(0), 0, 0, 0, 0, 0]
	v.mix(mut buffer, 3, 2)
	// The third frame wraps to the start instead of going silent.
	assert buffer[0] > 0 && buffer[1] > 0
	assert buffer[2] > 0 && buffer[3] > 0
	assert buffer[4] > 0 && buffer[5] > 0
	assert v.playing
}

fn test_voice_zero_channels_is_safe() {
	sound := &gg.Sound{ samples: [i16(1000), 1000] }
	mut v := new_voice(sound)
	v.sound.channels = 0
	v.play()
	mut buffer := [f32(0), 0]
	v.mix(mut buffer, 1, 2)
	assert buffer[0] == 0
}

fn test_mix_voices() {
	sound := &gg.Sound{ samples: [i16(1000), 1000, 1000, 1000] }
	a := new_voice(sound)
	b := new_voice(sound)
	a.play()
	b.play()
	mut buffer := [f32(0), 0]
	mix_voices(mut buffer, 1, 2, [a, b])
	assert buffer[0] > 0
}
