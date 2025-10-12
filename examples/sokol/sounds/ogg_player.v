import os
import time
import sokol.audio
import encoding.vorbis

fn main() {
	unbuffer_stdout()
	if os.args.len < 2 {
		eprintln('Usage: ogg_player file.ogg ...')
		play_sounds([os.resource_abs_path('pickup.ogg')])!
		exit(1)
	}
	play_sounds(os.args[1..])!
}

fn play_sounds(files []string) ! {
	mut player := Player{
		decoder: unsafe { nil }
	}
	player.init()
	for f in files {
		if !os.exists(f) || os.is_dir(f) {
			eprintln('skipping "${f}" (does not exist)')
			continue
		}
		fext := os.file_ext(f).to_lower()
		if fext != '.ogg' {
			eprintln('skipping "${f}" (not an .ogg file)')
			continue
		}
		player.play_ogg_file(f)!
	}
	player.stop()
}

struct Player {
mut:
	channels           int
	sample_rate        int
	pos                int
	finished           bool
	push_slack_ms      int = 5
	stream_rate        u32
	stream_channels    int
	stream_len_samples u32
	stream_len_seconds f32
	xerror             vorbis.VorbisErrorCode
	allocator          C.stb_vorbis_alloc = C.stb_vorbis_alloc{
		alloc_buffer:                 0
		alloc_buffer_length_in_bytes: 0
	}
	decoder            &C.stb_vorbis // TODO: cgen error with -cstrict -gcc, when this is = unsafe { nil } here
}

fn (mut p Player) init() {
	audio.setup()
	p.sample_rate = audio.sample_rate()
	p.channels = audio.channels()
	alloc_size := 200 * 1024
	p.allocator = C.stb_vorbis_alloc{
		alloc_buffer:                 unsafe { &char(vcalloc(alloc_size)) }
		alloc_buffer_length_in_bytes: alloc_size
	}
}

fn (mut p Player) stop() {
	p.free()
	audio.shutdown()
}

fn (mut p Player) free() {
	p.finished = false
	p.pos = 0
	p.close_decoder()
	unsafe { free(p.allocator.alloc_buffer) }
	unsafe {
		p.allocator.alloc_buffer = nil
	}
}

fn (mut p Player) close_decoder() {
	if !isnil(p.decoder) {
		C.stb_vorbis_close(p.decoder)
	}
}

fn (mut p Player) play_ogg_file(fpath string) ! {
	p.close_decoder()
	p.pos = 0
	p.xerror = .no_error
	p.decoder = C.stb_vorbis_open_filename(&char(fpath.str), voidptr(&p.xerror), &p.allocator)
	if isnil(p.decoder) || p.xerror != .no_error {
		return error('could not open ogg file: ${fpath}, xerror: ${p.xerror}')
	}
	info := C.stb_vorbis_get_info(p.decoder)
	p.stream_rate = info.sample_rate
	p.stream_channels = info.channels
	p.stream_len_samples = C.stb_vorbis_stream_length_in_samples(p.decoder)
	p.stream_len_seconds = C.stb_vorbis_stream_length_in_seconds(p.decoder)
	p.finished = false

	if !(p.channels == p.stream_channels && p.sample_rate == p.stream_rate) {
		audio.shutdown()
		audio.setup(
			num_channels: p.stream_channels
			sample_rate:  int(p.stream_rate)
		)
		p.sample_rate = audio.sample_rate()
		p.channels = audio.channels()
	}
	println('> play_ogg_file: rate: ${p.sample_rate:5}, channels: ${p.channels:1} | stream rate: ${p.stream_rate:5}, channels: ${p.stream_channels:1}, samples: ${p.stream_len_samples:8} | seconds: ${p.stream_len_seconds:7.3f} | ${fpath}')

	frames := [16384]f32{}
	pframes := unsafe { &frames[0] }
	for !p.finished {
		mut delay := p.push_slack_ms
		expected_frames := audio.expect()
		if expected_frames > 0 {
			mut decoded_frames := 0
			for decoded_frames < expected_frames {
				samples := C.stb_vorbis_get_samples_float_interleaved(p.decoder, p.channels,
					pframes, 1024)
				if samples == 0 {
					p.finished = true
					break
				}
				written_frames := audio.push(pframes, samples)
				decoded_frames += written_frames
				p.pos += samples
			}
			delay = (1_000 * decoded_frames) / p.sample_rate
		}
		print('\r  position: ${p.pos:9} / ${p.stream_len_samples:-9} samples | ${p.pos * p.stream_len_seconds / p.stream_len_samples:7.3f} / ${p.stream_len_seconds:-7.3f} seconds')
		time.sleep(int_max(p.push_slack_ms, delay - p.push_slack_ms) * time.millisecond)
	}
	println('')
}
