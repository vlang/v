module gg

fn test_backend_caps() {
	c := backend_caps()
	assert c.backend == 'native' || c.backend == 'js'
	if c.backend == 'js' {
		assert !c.can_draw && !c.can_audio
	} else {
		assert c.can_draw && c.can_audio
	}
}

fn test_info_counters() {
	mut info := Info{}
	info.record_draw(12)
	info.record_draw(4)
	assert info.draw_calls == 2
	assert info.triangles == 16
	info.end_frame()
	assert info.frames == 1
	assert info.draw_calls == 0 && info.triangles == 0
}
