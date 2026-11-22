module audio

import math

fn test_decode_generate_resample() {
	samples := generate_sine(440, 100, 8000)
	assert samples.len == 100
	buf := new_buffer(samples, 1, 8000)!
	assert buf.sample_rate == 8000
	mut wav := []u8{}
	wav << 'RIFF'.bytes()
	wav << [u8(0), 0, 0, 0]
	wav << 'WAVE'.bytes()
	wav << 'fmt '.bytes()
	wav << [u8(16), 0, 0, 0]
	wav << [u8(1), 0, 1, 0]
	wav << [u8(0x40), 0x1f, 0, 0]
	wav << [u8(0x80), 0x3e, 0, 0]
	wav << [u8(2), 0, 16, 0]
	wav << 'data'.bytes()
	wav << [u8(8), 0, 0, 0]
	for s in samples[..4] {
		wav << u8(s & 0xff)
		wav << u8(s >> 8)
	}
	decoded := decode_wav(wav)!
	assert decoded.channels == 1 && decoded.sample_rate == 8000
	assert decoded.samples.len == 4
	up := resample(&decoded, 16000)!
	assert up.sample_rate == 16000
	assert up.samples.len >= 8
}

fn test_mix_bus_and_device() {
	buf := new_buffer([i16(10000), 10000], 1, 44100)!
	mut out := []f32{len: 2, init: 0}
	mix_into(mut out, &buf, 0.5)
	assert out[0] > 0
	assert gain([i16(1000)], 2)[0] == 2000

	mut b := new_bus('test')
	b.gain = 0.5
	mut stereo := [f32(1), 1]
	b.process(mut stereo)
	assert stereo[0] > 0 && stereo[0] < 1

	d := null_device()
	assert !d.available
	assert (d.setup() or { false }) == false
	assert (d.push([]f32{}) or { 0 }) == 0
}

fn test_positional_and_stream() {
	buf := new_buffer([i16(5000), 5000, 5000, 5000], 1, 44100)!
	mut voice := new_positional_voice(&buf, Vec3{5, 0, 0})
	voice.playing = true
	listener := Listener{}
	mut dst := []f32{len: 8, init: 0}
	voice.mix(mut dst, 4, &listener)
	assert dst[0] > 0 || dst[1] > 0
	assert !voice.playing

	mut stream := new_stream([i16(1), 2, 3], true)
	stream.playing = true
	mut out := []i16{len: 5, init: 0}
	assert stream.next(mut out, 5) == 5
	assert out == [i16(1), 2, 3, 1, 2]
	assert math.abs(1.0 - 1.0) < 0.001
}
