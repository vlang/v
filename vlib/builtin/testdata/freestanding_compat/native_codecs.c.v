module main

#flag -lm

import os
import time

#include "@DIR/native_codecs_probe.h"
#flag @DIR/native_codecs_probe.c
#pkgconfig libavformat libavcodec libavfilter libavdevice libswscale libswresample libavutil
#pkgconfig libass harfbuzz fribidi freetype2 alsa

fn C.v_native_codecs_probe(&char, &char) i32

fn main() {
	if os.args.len != 3 {
		eprintln('expected video and font paths')
		exit(1)
	}
	gate := os.getenv('V_NATIVE_CODECS_GATE')
	println('native codec fixture ready')
	flush_stdout()
	if gate.len > 0 {
		for !os.exists(gate) { time.sleep(10 * time.millisecond) }
	}
	result := C.v_native_codecs_probe(&char(os.args[1].str), &char(os.args[2].str))
	if result != 0 {
		eprintln('native subtitle/audio/codec regression failed at C line ${result}')
		exit(1)
	}
	println('native shaping, subtitles, PCM, filtering, decoding and seeking passed')
	flush_stdout()
	if gate.len > 0 {
		for os.exists(gate) { time.sleep(10 * time.millisecond) }
	}
}
