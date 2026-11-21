module gg

fn test_dsp_gain_and_clamp() {
	s := [i16(100), -100, 32767]
	g := dsp_gain(s, 0.5)
	assert g[0] == 50 && g[1] == -50
	assert dsp_gain(s, 2)[2] == 32767
}

fn test_dsp_fade() {
	s := [i16(1000), 1000, 1000, 1000]
	f := dsp_fade(s, 2, 2)
	assert f[0] == 0
	assert f[1] == 500
	assert f[2] == 500
	assert f[3] == 0
}

fn test_dsp_normalize() {
	n := dsp_normalize([i16(100), -50], 1)
	assert n[0] == 32767
	assert (n[1] + 16384) < 2 && (n[1] + 16384) > -2
	assert dsp_normalize([i16(0), 0], 1) == [i16(0), 0]
}

fn test_dsp_sine_and_filter() {
	wave := dsp_generate_sine(440, 100, 44100)
	assert wave.len == 100
	assert dsp_generate_sine(440, 10, 0).len == 10
	noisy := [f32(1), -1, 1, -1]
	smooth := dsp_lowpass(noisy, 0.5)
	assert smooth.len == 4
	assert smooth[1] > noisy[1]
}

fn test_dsp_mix_and_pan() {
	mut dst := [f32(0.5), 0.5]
	dsp_mix_into(mut dst, [i16(32767)], 0.5)
	assert dst[0] > 0.5 && dst[0] <= 1
	stereo := dsp_to_stereo([i16(1000)], 0)
	assert stereo.len == 2
	assert stereo[0] == stereo[1]
	left := dsp_to_stereo([i16(1000)], -1)
	assert left[0] > left[1]
}
