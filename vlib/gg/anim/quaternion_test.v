module anim

import gg

fn test_quat_identity_and_axis() {
	q := quat_identity()
	assert q.w == 1 && q.x == 0
	half := quat_from_axis_angle(gg.Vec3{0, 1, 0}, 90)
	assert (half.w - 0.7071) < 0.001
	assert (half.y - 0.7071) < 0.001
}

fn test_quat_slerp() {
	a := quat_from_euler(gg.Vec3{0, 0, 0})
	b := quat_from_euler(gg.Vec3{0, 90, 0})
	mid := quat_slerp(a, b, 0.5)
	assert b.y > 0.3
	assert mid.y > 0.1
	e := quat_to_euler(mid)
	mag := if e.y < 0 { -e.y } else { e.y }
	assert (mag - 45) < 1 && (mag - 45) > -1
}

fn test_quat_normalize_and_mul() {
	q := quat_normalize(Quat{0, 0, 0, 0})
	assert q.w == 1
	id := quat_mul(quat_identity(), quat_identity())
	assert id.w == 1
}

fn test_mixer() {
	clip := &AnimClip{
		duration: 1
		tracks:   [AnimTrack{
			joint: 0
			keys:  [AnimKey{0, gg.Vec3{}, gg.Vec3{}, gg.Vec3{1, 1, 1}},
				AnimKey{1, gg.Vec3{10, 0, 0}, gg.Vec3{}, gg.Vec3{1, 1, 1}}]
		}]
	}
	mut mixer := new_mixer()
	mixer.add(new_action(clip))
	mixer.play()
	mixer.update(0.5)
	poses := mixer.sample(1)
	assert poses.len == 1
	assert (poses[0].translation.x - 5) < 0.001
}
