module gg

fn test_sound_lifetime_and_cleanup() {
	mut buffer := []f32{len: 4, init: 0}
	mut saved := &Sound{}
	{
		// The Sound is a short-lived local, as returned by load_wav callers.
		mut local := Sound{
			samples:     []i16{len: 8, init: 1000}
			channels:    1
			sample_rate: 44100
		}
		play_sound(&local)
		saved = &local
		assert is_sound_playing(&local)
	}
	// The original Sound is out of scope; the callback must mix from the
	// owned deep copy without panicking on a dangling pointer.
	mix_sound_frames(unsafe { &f32(buffer.data) }, 2, 1)
	assert buffer[0] != 0
	// Stopping through the original address clears both tracking maps.
	stop_sound(saved)
	assert !is_sound_playing(saved)
	assert sound_playing.len == 0
	assert sound_copies.len == 0
}
