module spatial

import gg
import math

fn test_bus_processing() {
	mut b := new_bus('music')
	b.gain = 0.5
	b.pan = 0
	mut samples := [f32(1), 1, 1, 1]
	b.process(mut samples)
	assert samples[0] > 0 && samples[0] < 1
	assert math.abs(samples[0] - samples[1]) < 0.01
	assert b.left_gain() > 0
}

fn test_positional_voice() {
	sound := &gg.Sound{ samples: [i16(10000), 10000, 10000, 10000], channels: 1 }
	mut v := new_positional_voice(sound, gg.Vec3{5, 0, 0})
	v.playing = true
	listener := Listener{}
	mut buffer := []f32{len: 8, init: 0}
	v.mix(mut buffer, 4, &listener)
	assert buffer[0] > 0 || buffer[1] > 0
	assert !v.playing
}

fn test_stream_loop() {
	mut s := new_stream([i16(1), 2, 3], true)
	s.playing = true
	mut out := []i16{len: 5, init: 0}
	assert s.next(mut out, 5) == 5
	assert out == [i16(1), 2, 3, 1, 2]
}

fn test_bus_keeps_independent_channel_states() {
	mut b := new_bus('stereo')
	b.gain = 1
	b.pan = 0
	b.cutoff = 1
	mut frames := [f32(1), 0, 0, 1]
	b.process(mut frames)
	assert math.abs(frames[0] - 1) < 0.001
	assert math.abs(frames[1]) < 0.001
	assert math.abs(frames[2]) < 0.001
	assert math.abs(frames[3] - 1) < 0.001
	// With a passthrough filter each state holds its own last channel value.
	assert math.abs(b.state_l) < 0.001
	assert math.abs(b.state_r - 1) < 0.001
}

fn test_positional_voice_writes_final_buffer_slot() {
	sound := &gg.Sound{ samples: [i16(10000), 10000], channels: 1 }
	mut v := new_positional_voice(sound, gg.Vec3{5, 0, 0})
	v.playing = true
	mut buffer := []f32{len: 4, init: 0}
	v.mix(mut buffer, 2, &Listener{})
	assert buffer[2] > 0
	assert buffer[3] > 0
}
