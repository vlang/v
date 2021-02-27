import math

const tau = 2 * math.pi

struct Note {
mut:
	freq   f32
	vol    f32
	step   int
	paused bool
}

enum WaveKind {
	sine
	sawtooth
}

struct Config {
	wave_kind WaveKind
}

fn sawtooth(note &Note, time f32, amp f32) f32 {
	t := time * note.freq
	f := t - int(t)
	return f32(2 * (f - 0.5)) * (amp / 2)
}

fn sine(note &Note, time f32, amp f32) f32 {
	return math.sinf(tau * time * note.freq) * amp
}

pub fn new_context(cfg Config) fn (&Note, f32, f32) f32 {
	// Note, that here `sine` and `sawtooth`,
	// are different functions, but they do have
	// a compatible signature, so `next_fn` can 
	// become either one of them:
	next_fn := match cfg.wave_kind {
		.sine { sine }
		.sawtooth { sawtooth }
	}
	return next_fn
}

fn test_match_expression_returning_fns() {
	note := &Note{
		freq: 432
		vol: 80
		step: 3
		paused: false
	}

	x := new_context(wave_kind: .sawtooth)
	assert '$x' == 'fn (Note, f32, f32) f32'
	assert math.abs(x(note, 0, 0.5) + 0.25) < 0.001
	assert math.abs(x(note, 0.4, 0.5) - 0.15) < 0.001
	assert math.abs(x(note, 0.7, 0.5) + 0.05) < 0.001

	y := new_context(wave_kind: .sine)
	assert '$y' == 'fn (Note, f32, f32) f32'
	assert math.abs(y(note, 0, 0.5)) - 0.475 < 0.0001
	assert math.abs(y(note, 0.4, 0.5)) - 0.4755 < 0.0001
	assert math.abs(y(note, 0.7, 0.5)) - 0.2939 < 0.0001
}
