// vtest build: !sanitize-memory-clang // Fails compilation with: `ld: /lib/x86_64-linux-gnu/libpthread.so.0: error adding symbols: DSO missing from command line`
// import log
import math
import time
import sokol.audio

fn main() {
	args := arguments()
	freq := args[1] or { '417' }.int()
	amplitude := args[2] or { '0.5' }.f32()
	dump(freq)
	dump(amplitude)

	audio.setup(num_channels: 1)
	sample_rate := dump(audio.sample_rate())
	dump(audio.buffer_frames())
	dump(audio.expect())

	// create an array of frames, filled with the desired pure sine tone:
	mut frames := []f32{len: sample_rate * 2} // 2 seconds
	for i in 0 .. frames.len {
		t := f32(i) / f32(sample_rate)
		frames[i] = amplitude * math.sinf(f32(freq) * t * (2 * math.pi))
	}

	// play the sound by continuosly pushing samples from the generated
	// array of sound frames, when there is need for more:
	mut fpos := 0
	for {
		expected_frames := audio.expect()
		if expected_frames > 0 {
			written_frames := audio.push(unsafe { &frames[fpos] }, expected_frames)
			fpos += written_frames
			// log.info('> pushing done ... fpos: ${fpos:6} | expected_frames: ${expected_frames:6} | written: ${written:6}')
			if fpos > frames.len - 2 * written_frames {
				// log.info('> fpos too large: ${fpos}')
				fpos = find_loop_position(fpos, frames)
				// log.info('> fpos looped back to start: ${fpos}')
			}
		}
		time.sleep(50 * time.millisecond)
	}
	audio.shutdown()
}

fn find_loop_position(fpos int, frames []f32) int {
	return find_matching_position(frames, fpos, 0.01, 2) or {
		find_matching_position(frames, fpos, 0.05, 2) or {
			find_matching_position(frames, fpos, 0.1, 2) or { 0 }
		}
	}
}

@[direct_array_access]
fn find_matching_position(frames []f32, fpos int, tol f32, d int) ?int {
	if fpos - d < 0 || fpos + d >= frames.len {
		return none
	}
	p1, p2, p3 := frames[fpos - d], frames[fpos], frames[fpos + d]
	for i in 2 .. fpos / 2 {
		np1, np2, np3 := frames[i - d], frames[i], frames[i + d]
		if math.tolerance(np1, p1, tol) && math.tolerance(np2, p2, tol)
			&& math.tolerance(np3, p3, tol) {
			return i
		}
	}
	return none
}
