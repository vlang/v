module audio

import math

// IMA ADPCM is a simple 4-bit compressed format: 16-bit PCM is reduced to
// four bits per sample (4:1) with a predictor and an adaptive step index.
// It is implemented in pure V so it works on every backend.

const ima_steps = [
	u16(7),
	8,
	9,
	10,
	11,
	12,
	13,
	14,
	16,
	17,
	19,
	21,
	23,
	25,
	28,
	31,
	34,
	37,
	41,
	45,
	50,
	55,
	60,
	66,
	73,
	80,
	88,
	97,
	107,
	118,
	130,
	143,
	157,
	173,
	190,
	209,
	230,
	253,
	279,
	307,
	337,
	371,
	408,
	449,
	494,
	544,
	598,
	658,
	724,
	796,
	876,
	963,
	1060,
	1166,
	1282,
	1411,
	1552,
	1707,
	1878,
	2066,
	2272,
	2500,
	2750,
	3025,
	3327,
	3660,
	4026,
	4429,
	4872,
	5359,
	5895,
	6484,
	7132,
	7845,
	8630,
	9493,
	10442,
	11487,
	12635,
	13899,
	15289,
	16818,
	18500,
	20350,
	22385,
	24623,
	27086,
	29794,
	32767,
]

const ima_index_table = [int(-1), -1, -1, -1, 2, 4, 6, 8, -1, -1, -1, -1, 2, 4, 6, 8]

fn clamp_i16_adpcm(v int) i16 {
	return i16(math.clip(v, -32768, 32767))
}

// adpcm_encode compresses interleaved PCM into a 4-bit stream. The first four
// bytes hold the predictor (little-endian i16), step index and a reserved byte.
pub fn adpcm_encode(pcm []i16) []u8 {
	mut out := []u8{len: 4 + (pcm.len + 1) / 2, init: 0}
	if pcm.len == 0 {
		return out
	}
	mut predictor := int(pcm[0])
	mut index := 0
	out[0] = u8(predictor)
	out[1] = u8(predictor >> 8)
	out[2] = u8(index)
	for i, sample in pcm {
		step := int(ima_steps[index])
		diff := int(sample) - predictor
		mut code := 0
		if diff < 0 {
			code = (-diff * 4) / step
			if code > 7 {
				code = 7
			}
			code |= 8
		} else {
			code = (diff * 4) / step
			if code > 7 {
				code = 7
			}
		}
		delta := (code & 7) * step / 4 + step / 8
		if code & 8 != 0 {
			predictor -= delta
		} else {
			predictor += delta
		}
		predictor = int(clamp_i16(predictor))
		index += ima_index_table[code]
		index = int(math.clip(index, 0, 88))
		at := 4 + i / 2
		if i % 2 == 0 {
			out[at] = u8(code)
		} else {
			out[at] |= u8(code << 4)
		}
	}
	return out
}

// adpcm_decode expands a stream produced by adpcm_encode. sample_count is the
// number of 16-bit samples to produce.
pub fn adpcm_decode(data []u8, sample_count int) []i16 {
	if sample_count <= 0 {
		return []i16{}
	}
	mut out := []i16{len: sample_count, init: 0}
	if data.len < 4 {
		return out
	}
	mut predictor := int(u16(data[0]) | u16(data[1]) << 8)
	if predictor >= 32768 {
		predictor -= 65536
	}
	mut index := int(data[2])
	index = int(math.clip(index, 0, 88))
	for i in 0 .. sample_count {
		at := 4 + i / 2
		if at >= data.len {
			break
		}
		code := if i % 2 == 0 { int(data[at]) & 0xf } else { int(data[at]) >> 4 }
		step := int(ima_steps[index])
		delta := (code & 7) * step / 4 + step / 8
		if code & 8 != 0 {
			predictor -= delta
		} else {
			predictor += delta
		}
		predictor = int(clamp_i16(predictor))
		index += ima_index_table[code]
		index = int(math.clip(index, 0, 88))
		out[i] = i16(predictor)
	}
	return out
}

// encode_wav writes a 16-bit PCM WAV container from a Buffer.
pub fn encode_wav(buf &Buffer) []u8 {
	data_len := buf.samples.len * 2
	mut out := []u8{len: 44 + data_len, init: 0}
	copy(mut out[0..4], 'RIFF'.bytes())
	put_u32(mut out, 4, u32(36 + data_len))
	copy(mut out[8..12], 'WAVE'.bytes())
	copy(mut out[12..16], 'fmt '.bytes())
	put_u32(mut out, 16, 16)
	put_u16(mut out, 20, 1)
	put_u16(mut out, 22, u16(buf.channels))
	put_u32(mut out, 24, u32(buf.sample_rate))
	put_u32(mut out, 28, u32(buf.sample_rate * buf.channels * 2))
	put_u16(mut out, 32, u16(buf.channels * 2))
	put_u16(mut out, 34, 16)
	copy(mut out[36..40], 'data'.bytes())
	put_u32(mut out, 40, u32(data_len))
	for i, s in buf.samples {
		put_u16(mut out, 44 + i * 2, u16(s))
	}
	return out
}

fn put_u16(mut out []u8, at int, v u16) {
	out[at] = u8(v)
	out[at + 1] = u8(v >> 8)
}

fn put_u32(mut out []u8, at int, v u32) {
	for i in 0 .. 4 {
		out[at + i] = u8(v >> u32(i * 8))
	}
}
