// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module anim

import gg
import math

const max_wrap_cycles = 1_000_000_000.0

// AnimInterp selects how a track interpolates between its keyframes.
pub enum AnimInterp {
	step
	linear
}

// Joint describes one node of a skeleton. parent is -1 for a root joint.
pub struct Joint {
pub mut:
	name   string
	parent int = -1
}

// AnimKey is a transform keyframe at a time in seconds. rotation is an Euler
// angle triplet in degrees; scale defaults to one.
pub struct AnimKey {
pub mut:
	time        f32
	translation gg.Vec3
	rotation    gg.Vec3
	scale       gg.Vec3 = gg.Vec3{1, 1, 1}
}

// AnimTrack animates one joint by index.
pub struct AnimTrack {
pub mut:
	joint  int
	keys   []AnimKey
	interp AnimInterp = .linear
}

// AnimClip is a named collection of tracks with an explicit duration.
@[heap]
pub struct AnimClip {
pub mut:
	name     string
	duration f32
	tracks   []AnimTrack
}

// JointPose is a sampled transform for one joint.
pub struct JointPose {
pub mut:
	translation gg.Vec3
	rotation    gg.Vec3
	scale       gg.Vec3 = gg.Vec3{1, 1, 1}
}

// AnimPlayer advances an AnimClip and samples poses for any number of joints.
// It is pure V and independent of any file format; feed it tracks from your
// asset pipeline or from a loader you control.
pub struct AnimPlayer {
pub mut:
	clip    &AnimClip
	time    f32
	speed   f32  = 1
	looping bool = true
	playing bool
}

// new_player returns a stopped player for clip.
pub fn new_player(clip &AnimClip) AnimPlayer {
	return AnimPlayer{ clip: clip }
}

// play starts playback. If the player had reached the end it rewinds.
pub fn (mut p AnimPlayer) play() {
	if p.time >= p.clip.duration {
		p.time = 0
	}
	p.playing = true
}

// stop pauses playback without rewinding.
pub fn (mut p AnimPlayer) stop() {
	p.playing = false
}

// rewind resets playback to the first frame.
pub fn (mut p AnimPlayer) rewind() {
	p.time = 0
}

// update advances playback by dt seconds and returns true when a non-looping
// clip finished on this call.
pub fn (mut p AnimPlayer) update(dt f32) bool {
	if !p.playing || dt <= 0 {
		return false
	}
	p.time += dt * p.speed
	if p.time < p.clip.duration {
		return false
	}
	if p.looping {
		duration := p.clip.duration
		if duration <= 0 {
			// No meaningful wrap period; keep the time instead of dividing by zero.
			return false
		}
		// Compute in f64 so a large time cannot overflow the int conversion.
		mut cycles_f := math.floor(f64(p.time) / f64(duration))
		if cycles_f < 0 {
			cycles_f = 0
		}
		if cycles_f > max_wrap_cycles {
			cycles_f = max_wrap_cycles
		}
		cycles := f32(int(cycles_f))
		p.time -= cycles * duration
		return false
	}
	p.time = p.clip.duration
	p.playing = false
	return true
}

fn sample_track(track AnimTrack, time f32) JointPose {
	if track.keys.len == 0 {
		return JointPose{}
	}
	if track.keys.len == 1 || time <= track.keys[0].time {
		k := track.keys[0]
		return JointPose{k.translation, k.rotation, k.scale}
	}
	last := track.keys[track.keys.len - 1]
	if time >= last.time {
		return JointPose{last.translation, last.rotation, last.scale}
	}
	mut i := 0
	for i < track.keys.len - 1 && time > track.keys[i + 1].time {
		i++
	}
	a := track.keys[i]
	b := track.keys[i + 1]
	if track.interp == .step {
		return JointPose{a.translation, a.rotation, a.scale}
	}
	span := b.time - a.time
	t := if span <= 0 { f32(0) } else { (time - a.time) / span }
	return JointPose{
		translation: lerp_vec3(a.translation, b.translation, t)
		rotation:    lerp_vec3(a.rotation, b.rotation, t)
		scale:       lerp_vec3(a.scale, b.scale, t)
	}
}

// sample returns one pose per joint for the current time. Joints without a
// track receive an identity pose.
pub fn (p &AnimPlayer) sample(joint_count int) []JointPose {
	mut out := []JointPose{len: joint_count}
	for track in p.clip.tracks {
		if track.joint < 0 || track.joint >= joint_count {
			continue
		}
		out[track.joint] = sample_track(track, p.time)
	}
	return out
}
