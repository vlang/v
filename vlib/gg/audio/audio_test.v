module audio

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
