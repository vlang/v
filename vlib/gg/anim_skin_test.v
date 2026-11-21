module gg

fn make_clip() &AnimClip {
	return &AnimClip{
		name:     'walk'
		duration: 2
		tracks:   [AnimTrack{
			joint: 0
			keys:  [AnimKey{0, Vec3{}, Vec3{}, Vec3{1, 1, 1}},
				AnimKey{2, Vec3{10, 0, 0}, Vec3{0, 90, 0}, Vec3{1, 1, 1}}]
		}]
	}
}

fn test_track_sampling() {
	clip := make_clip()
	mut p := new_player(clip)
	pose := p.sample(1)
	assert pose.len == 1
	assert pose[0].translation.x == 0
	p.time = 1
	mid := p.sample(1)
	assert mid[0].translation.x == 5
	assert mid[0].rotation.y == 45
	p.time = 9
	end := p.sample(1)
	assert end[0].translation.x == 10
}

fn test_step_interpolation() {
	mut clip := make_clip()
	clip.tracks[0].interp = .step
	mut p := new_player(clip)
	p.time = 1
	pose := p.sample(1)
	assert pose[0].translation.x == 0
}

fn test_player_update() {
	clip := make_clip()
	mut p := new_player(clip)
	p.play()
	assert !p.update(0.5)
	assert p.time == 0.5
	assert !p.update(1)
	assert p.time == 1.5
	assert !p.update(1)
	assert p.time == 0.5
	p.looping = false
	p.rewind()
	p.play()
	assert !p.update(1)
	assert p.update(1)
	assert !p.playing
	assert p.time == 2
	p.play()
	assert p.time == 0
}

fn test_missing_joints() {
	clip := make_clip()
	mut p := new_player(clip)
	poses := p.sample(3)
	assert poses.len == 3
	assert poses[1].scale.x == 1
	assert poses[2].translation.x == 0
}
