module anim

// AnimAction is one clip playing inside a mixer, with a blend weight.
pub struct AnimAction {
pub mut:
	clip   &AnimClip
	player AnimPlayer
	weight f32  = 1
	active bool = true
}

// new_action returns a playable action for clip.
pub fn new_action(clip &AnimClip) AnimAction {
	return AnimAction{ clip: clip, player: new_player(clip) }
}

// AnimMixBlend selects how actions are combined.
pub enum AnimMixBlend {
	override
	additive
}

// AnimMixer runs several actions at once and blends their poses. This is the
// small-scale equivalent of three.js AnimationMixer.
pub struct AnimMixer {
pub mut:
	actions []AnimAction
	blend   AnimMixBlend = .override
}

// new_mixer returns an empty mixer.
pub fn new_mixer() AnimMixer {
	return AnimMixer{}
}

// add registers an action and returns its index.
pub fn (mut m AnimMixer) add(action AnimAction) int {
	m.actions << action
	return m.actions.len - 1
}

// play starts every active action.
pub fn (mut m AnimMixer) play() {
	for i in 0 .. m.actions.len {
		m.actions[i].player.play()
	}
}

// update advances all active actions by dt.
pub fn (mut m AnimMixer) update(dt f32) {
	for i in 0 .. m.actions.len {
		if m.actions[i].active {
			m.actions[i].player.update(dt)
		}
	}
}

// sample returns a blended pose for each joint. In override mode a weight of
// 1 replaces the previous pose; partial weights blend. Additive mode blends
// proportionally. Rotations use quaternion slerp.
pub fn (m &AnimMixer) sample(joint_count int) []JointPose {
	mut out := []JointPose{len: joint_count}
	for action in m.actions {
		if !action.active || action.weight <= 0 {
			continue
		}
		poses := action.player.sample(joint_count)
		factor := if action.weight > 1 { 1 } else { action.weight }
		for j in 0 .. joint_count {
			if m.blend == .override && factor >= 1 {
				out[j] = poses[j]
			} else {
				out[j] = blend_pose(out[j], poses[j], factor)
			}
		}
	}
	return out
}

fn blend_pose(a JointPose, b JointPose, factor f32) JointPose {
	qa := quat_from_euler(a.rotation)
	qb := quat_from_euler(b.rotation)
	return JointPose{
		translation: lerp_vec3(a.translation, b.translation, factor)
		rotation:    quat_to_euler(quat_slerp(qa, qb, factor))
		scale:       lerp_vec3(a.scale, b.scale, factor)
	}
}
