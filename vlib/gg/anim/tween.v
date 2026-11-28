// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module anim

import gg
import math

// lerp_f32 interpolates two f32 values and is the default interpolation
// function for numeric tweens.
pub fn lerp_f32(a f32, b f32, t f32) f32 {
	return math.mix(a, b, t)
}

// lerp_f64 interpolates two f64 values.
pub fn lerp_f64(a f64, b f64, t f64) f64 {
	return math.mix(a, b, t)
}

// lerp_color interpolates two colors channel by channel. The parameter is
// clamped to [0, 1] so a tween never produces out-of-range channels.
pub fn lerp_color(a gg.Color, b gg.Color, t f32) gg.Color {
	k := math.clip(t, 0.0, 1.0)
	return gg.Color{
		r: u8(f32(a.r) + (f32(b.r) - f32(a.r)) * k)
		g: u8(f32(a.g) + (f32(b.g) - f32(a.g)) * k)
		b: u8(f32(a.b) + (f32(b.b) - f32(a.b)) * k)
		a: u8(f32(a.a) + (f32(b.a) - f32(a.a)) * k)
	}
}

// lerp_vec2 interpolates two 2D vectors component wise.
pub fn lerp_vec2(a gg.Vec2, b gg.Vec2, t f32) gg.Vec2 {
	return a + (b - a).scale(math.clip(t, 0.0, 1.0))
}

// lerp_vec3 interpolates two 3D vectors component wise.
pub fn lerp_vec3(a gg.Vec3, b gg.Vec3, t f32) gg.Vec3 {
	return a + (b - a).scale(math.clip(t, 0.0, 1.0))
}

// Tween interpolates a value from from to to over duration seconds using an
// Easing curve. The lerp_fn selects how values of T are blended; use one of
// lerp_f32, lerp_color, lerp_vec2 or lerp_vec3, or supply your own.
pub struct Tween[T] {
pub mut:
	from     T
	to       T
	duration f32    = 1
	elapsed  f32    = 0
	easing   Easing = .linear
	looping  bool
	lerp_fn  fn (a T, b T, t f32) T @[required]
}

// new_tween builds a tween that blends values of T with lerp_fn.
pub fn new_tween[T](from T, to T, duration f32, lerp_fn fn (a T, b T, t f32) T) Tween[T] {
	return Tween[T]{
		from:     from
		to:       to
		duration: duration
		lerp_fn:  lerp_fn
	}
}

// progress returns the normalized, clamped elapsed time in [0, 1].
pub fn (t &Tween[T]) progress() f32 {
	if t.duration <= 0 {
		return 1
	}
	return math.clip(t.elapsed / t.duration, 0.0, 1.0)
}

// value returns the interpolated value at the current progress.
pub fn (t &Tween[T]) value() T {
	return t.lerp_fn(t.from, t.to, ease(t.progress(), t.easing))
}

// update advances the tween by dt seconds and returns true once it finishes.
// A looping tween wraps its elapsed time and never reports completion.
pub fn (mut t Tween[T]) update(dt f32) bool {
	t.elapsed += dt
	if t.duration <= 0 {
		return true
	}
	if t.elapsed < t.duration {
		return false
	}
	if t.looping {
		cycles := f32(math.floor(f64(t.elapsed / t.duration)))
		t.elapsed -= cycles * t.duration
		return false
	}
	t.elapsed = t.duration
	return true
}

// reset restarts the tween from its from value.
pub fn (mut t Tween[T]) reset() {
	t.elapsed = 0
}

// TimelineEvent is a named cue that fires once when the timeline reaches at.
pub struct TimelineEvent {
pub:
	at   f32
	name string
pub mut:
	fired bool
}

// Timeline fires named cues in order as time advances, for sequencing
// animations or scripted events without a callback graph.
pub struct Timeline {
pub mut:
	time   f32 = 0
	events []TimelineEvent
}

// new_timeline returns an empty timeline.
pub fn new_timeline() Timeline {
	return Timeline{}
}

// add schedules a named cue at the given time in seconds.
pub fn (mut tl Timeline) add(at f32, name string) {
	tl.events << TimelineEvent{ at: at, name: name }
}

// update advances the timeline and returns the names of cues fired this step.
pub fn (mut tl Timeline) update(dt f32) []string {
	tl.time += dt
	mut fired := []string{}
	for i in 0 .. tl.events.len {
		if !tl.events[i].fired && tl.time >= tl.events[i].at {
			tl.events[i].fired = true
			fired << tl.events[i].name
		}
	}
	return fired
}

// reset rewinds the timeline and clears all fired flags.
pub fn (mut tl Timeline) reset() {
	tl.time = 0
	for i in 0 .. tl.events.len {
		tl.events[i].fired = false
	}
}

// Spring is a damped harmonic spring driven towards target. It is useful for
// camera follow, smoothing and UI motion that should settle naturally.
pub struct Spring {
pub mut:
	value     f32
	velocity  f32 = 0
	target    f32
	stiffness f32 = 120
	damping   f32 = 14
}

// new_spring returns a spring at value with the given target.
pub fn new_spring(value f32, target f32) Spring {
	return Spring{ value: value, target: target }
}

// update advances the spring by dt seconds using semi-implicit Euler.
// Large frame gaps are clamped to keep the simulation stable.
pub fn (mut s Spring) update(dt f32) {
	step := math.clip(dt, 0.0, 1.0 / 30.0)
	force := -s.stiffness * (s.value - s.target) - s.damping * s.velocity
	s.velocity += force * step
	s.value += s.velocity * step
}
