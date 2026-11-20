// Gesture recognition for gg.
//
// GestureTracker consumes the normal stream of gg `Event`s and turns
// mouse/touch input into higher level gestures such as taps, holds, drags,
// swipes and two finger pinch/twist. It is deliberately independent of any
// windowing or rendering state, so an application can own one tracker (or one
// per pointer) and feed it from its `event_fn` callback.

module gg

import math
import time

// Gesture enumerates the gestures that GestureTracker is able to recognise.
// `none` is only used as a zero value; GestureTracker.update never returns it.
pub enum Gesture {
	none
	tap
	double_tap
	hold
	drag
	swipe_left
	swipe_right
	swipe_up
	swipe_down
	pinch
	twist
}

// GestureEvent is a single recognised gesture.
//
// `position` is the last known pointer position (the centroid of the touches
// for multi touch gestures). `delta` carries the meaningful magnitude of the
// gesture: travelled pixels for drag/swipe, pixels of separation change for
// pinch, radians for twist, and milliseconds for hold. It is 0 for taps.
// `timestamp` is a unix time in milliseconds taken when the gesture was
// recognised.
pub struct GestureEvent {
pub:
	kind      Gesture
	position  Vec2
	delta     f32
	timestamp i64
}

// GestureTracker holds the state needed to turn a stream of events into
// gestures. Create one with `GestureTracker{}` and call `update` for every
// event delivered to the application.
pub struct GestureTracker {
pub mut:
	active       bool // true while a press/touch is in progress
	start_ms     i64  // unix time in ms when the current press began
	last_tap_ms  i64  // unix time in ms of the most recent tap (for double tap)
	start_pos    Vec2 // position where the current press began
	last_pos     Vec2 // most recent position of the pointer
	max_touches  int  // largest number of simultaneous touches in this press
	hold_emitted bool // a hold has already been reported for this press
	start_dist   f32  // initial distance between the first two touches
	last_dist    f32  // latest distance between the first two touches
	start_angle  f32  // initial angle between the first two touches (radians)
	last_angle   f32  // latest angle between the first two touches (radians)
}

const tap_max_ms = 250 // a tap must be released within this time
const hold_min_ms = 500 // a press held at least this long is a hold
const double_tap_ms = 300 // gap within which two taps become a double tap

// Update feeds one gg event into the tracker. It returns a GestureEvent once
// the gesture is complete (normally on release), otherwise `none`.
//
// Mouse input (`mouse_down`/`mouse_move`/`mouse_up`) and touch input
// (`touches_began`/`touches_moved`/`touches_ended`/`touches_cancelled`) are
// handled with the same thresholds. Two or more touches enable pinch and
// twist recognition.
pub fn (mut t GestureTracker) update(e &Event) ?GestureEvent {
	now := time.now().unix_milli()
	match e.typ {
		.mouse_down {
			t.begin(e.mouse_x, e.mouse_y, 1, now)
			return none
		}
		.touches_began {
			p := touch_center(e)
			n := touch_count(e)
			t.begin(p.x, p.y, n, now)
			$if !js {
				if n >= 2 {
					t.start_dist, t.start_angle = touch_pair(e)
					t.last_dist = t.start_dist
					t.last_angle = t.start_angle
				}
			}
			return none
		}
		.mouse_move {
			if !t.active {
				return none
			}
			t.last_pos = Vec2{e.mouse_x, e.mouse_y}
			return t.maybe_hold(now)
		}
		.touches_moved {
			if !t.active {
				return none
			}
			t.last_pos = touch_center(e)
			$if !js {
				if touch_count(e) >= 2 {
					t.last_dist, t.last_angle = touch_pair(e)
				}
			}
			return t.maybe_hold(now)
		}
		.mouse_up, .touches_ended, .touches_cancelled {
			if !t.active {
				return none
			}
			if e.typ == .touches_ended || e.typ == .touches_cancelled {
				if touch_count(e) > 0 {
					t.last_pos = touch_center(e)
				}
			}
			return t.finish(now)
		}
		else {
			return none
		}
	}
}

// reset clears all state, including the remembered tap used for double tap
// detection. Call it when the input context changes (for example on focus
// loss) so that a stale tap cannot pair with a new one.
pub fn (mut t GestureTracker) reset() {
	t.active = false
	t.last_tap_ms = 0
	t.hold_emitted = false
}

fn (mut t GestureTracker) begin(x f32, y f32, touches int, now i64) {
	t.active = true
	t.start_ms = now
	t.start_pos = Vec2{x, y}
	t.last_pos = t.start_pos
	t.max_touches = touches
	t.hold_emitted = false
	t.start_dist = 0
	t.last_dist = 0
	t.start_angle = 0
	t.last_angle = 0
}

// maybe_hold reports a hold early, as soon as a stationary press has lasted
// longer than hold_min_ms. Reporting it here (rather than only on release)
// lets applications react while the finger is still down.
fn (mut t GestureTracker) maybe_hold(now i64) ?GestureEvent {
	if t.hold_emitted || !t.active {
		return none
	}
	if now - t.start_ms < hold_min_ms {
		return none
	}
	// A pointer that has travelled far is a drag, not a hold.
	if gesture_distance(t.start_pos, t.last_pos) > 10.0 {
		return none
	}
	t.hold_emitted = true
	t.active = false
	return GestureEvent{
		kind:      .hold
		position:  t.last_pos
		delta:     f32(now - t.start_ms)
		timestamp: now
	}
}

// finish classifies a completed press and resets the per press state.
fn (mut t GestureTracker) finish(now i64) ?GestureEvent {
	duration := now - t.start_ms
	pos := t.last_pos
	t.active = false
	if t.hold_emitted {
		return none
	}

	// Two finger gestures are decided from the change in separation and angle.
	if t.max_touches >= 2 {
		dist_delta := t.last_dist - t.start_dist
		angle_delta := t.last_angle - t.start_angle
		if gesture_fabs(dist_delta) > 10.0 {
			return GestureEvent{
				kind:      .pinch
				position:  pos
				delta:     dist_delta
				timestamp: now
			}
		}
		if gesture_fabs(angle_delta) > 0.2 {
			return GestureEvent{
				kind:      .twist
				position:  pos
				delta:     angle_delta
				timestamp: now
			}
		}
	}

	dist := gesture_distance(t.start_pos, t.last_pos)
	if duration >= hold_min_ms {
		return GestureEvent{
			kind:      .hold
			position:  pos
			delta:     f32(duration)
			timestamp: now
		}
	}
	// A swipe is a large, fast movement. A larger movement over a longer time
	// is intentionally still classified as a drag.
	if dist > 50.0 && duration <= tap_max_ms * 2 {
		mut kind := Gesture.drag
		dx := pos.x - t.start_pos.x
		dy := pos.y - t.start_pos.y
		if gesture_fabs(dx) >= gesture_fabs(dy) {
			if dx > 0 {
				kind = .swipe_right
			} else {
				kind = .swipe_left
			}
		} else {
			if dy > 0 {
				kind = .swipe_down
			} else {
				kind = .swipe_up
			}
		}
		return GestureEvent{ kind: kind, position: pos, delta: dist, timestamp: now }
	}
	if dist > 10.0 {
		return GestureEvent{
			kind:      .drag
			position:  pos
			delta:     dist
			timestamp: now
		}
	}
	if duration <= tap_max_ms {
		if t.last_tap_ms > 0 && now - t.last_tap_ms <= double_tap_ms {
			t.last_tap_ms = 0
			return GestureEvent{ kind: .double_tap, position: pos, timestamp: now }
		}
		t.last_tap_ms = now
		return GestureEvent{ kind: .tap, position: pos, timestamp: now }
	}
	return none
}

fn gesture_fabs(v f32) f32 {
	return if v < 0 { -v } else { v }
}

fn gesture_distance(a Vec2, b Vec2) f32 {
	dx := a.x - b.x
	dy := a.y - b.y
	return f32(math.sqrt(dx * dx + dy * dy))
}

// touch_count returns the number of touches carried by `e`. The JS backend
// does not expose touch data yet, so it always reports zero there.
fn touch_count(e &Event) int {
	$if js {
		return 0
	} $else {
		return e.num_touches
	}
}

// touch_center returns the centroid of the touches in `e`, or the mouse
// position when the event has no touch data.
fn touch_center(e &Event) Vec2 {
	$if js {
		return Vec2{e.mouse_x, e.mouse_y}
	} $else {
		mut n := touch_count(e)
		if n > e.touches.len {
			n = e.touches.len
		}
		if n <= 0 {
			return Vec2{e.mouse_x, e.mouse_y}
		}
		mut sx := f32(0)
		mut sy := f32(0)
		for i in 0 .. n {
			sx += e.touches[i].pos_x
			sy += e.touches[i].pos_y
		}
		return Vec2{sx / f32(n), sy / f32(n)}
	}
}

// touch_pair returns the distance and angle (in radians) between the first
// two touches in `e`, or (0, 0) when there are fewer than two touches.
fn touch_pair(e &Event) (f32, f32) {
	$if js {
		return 0, 0
	} $else {
		if e.num_touches < 2 || e.touches.len < 2 {
			return 0, 0
		}
		a := e.touches[0]
		b := e.touches[1]
		dx := b.pos_x - a.pos_x
		dy := b.pos_y - a.pos_y
		return f32(math.sqrt(dx * dx + dy * dy)), f32(math.atan2(dy, dx))
	}
}
