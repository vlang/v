module game

fn game_test_update(dt f32) {
	_ = dt
}

fn test_fixed_step() {
	mut f := new_fixed_step(0.1)
	assert f.advance(0.05) == 0
	assert f.advance(0.05) == 1
	assert f.advance(0.35) == 3
	assert f.accumulator < 0.1
	mut zero := new_fixed_step(0)
	assert zero.advance(1) == 0
}

fn test_frame_stats() {
	mut s := FrameStats{}
	for _ in 0 .. 30 {
		s.update(1.0 / 60.0)
	}
	assert s.fps > 55 && s.fps < 65
	assert s.elapsed > 0.49
}

fn test_state_machine() {
	mut sm := new_state_machine()
	sm.add('idle', game_test_update)
	sm.set('idle')
	assert sm.current == 'idle'
	sm.update(1.0 / 60.0)
	assert sm.updates == 1
	sm.set('missing')
	assert sm.current == 'idle'
	sm.update(1.0 / 60.0)
	assert sm.updates == 2
}
