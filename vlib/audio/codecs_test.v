module audio

import math

fn test_adpcm_roundtrip() {
	pcm := generate_sine(440, 256, 8000)
	compressed := adpcm_encode(pcm)
	assert compressed.len < pcm.len * 2 / 3
	decoded := adpcm_decode(compressed, pcm.len)
	assert decoded.len == pcm.len
	mut error_sum := f64(0)
	for i in 0 .. pcm.len {
		error_sum += math.abs(f64(decoded[i]) - f64(pcm[i]))
	}
	avg := error_sum / f64(pcm.len)
	assert avg < 2000
}

fn test_wav_encode() {
	buf := new_buffer([i16(1), -1, 2, -2], 1, 8000)!
	data := encode_wav(&buf)
	assert data.len == 52
	assert data[0] == `R` && data[1] == `I` && data[2] == `F` && data[3] == `F`
	again := decode_wav(data)!
	assert again.samples == buf.samples
	assert again.sample_rate == 8000
}
