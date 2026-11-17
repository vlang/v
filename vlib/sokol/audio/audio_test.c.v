// vtest build: !openbsd

module main

import sokol.audio

// Select the dummy backend from the C compiler's actual instrumentation flags.
// The test runner's self_sanitize_memory define only controls test selection.
#preinclude "@VMODROOT/vlib/sokol/audio/audio_test_backend.h"

fn test_sokol_audio_setup_and_shutdown() {
	audio.setup()
	// In environments with an audio device, is_valid() will be true.
	// In headless CI without audio endpoints, setup fails gracefully and is_valid() is false.
	_ = audio.is_valid()
	_ = audio.sample_rate()
	audio.shutdown()
}
