// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module debug

import gg

// DebugLog is a bounded ring of text lines for an on-screen console.
pub struct DebugLog {
pub mut:
	lines   []string
	max     int  = 100
	visible bool = true
}

// new_debug_log returns an empty log.
pub fn new_debug_log() DebugLog {
	return DebugLog{}
}

// add appends a line, dropping the oldest entries past the limit.
pub fn (mut l DebugLog) add(line string) {
	l.lines << line
	for l.lines.len > l.max && l.lines.len > 0 {
		l.lines = l.lines[1..]
	}
}

// clear removes all lines.
pub fn (mut l DebugLog) clear() {
	l.lines = []
}

// recent returns up to n newest lines in oldest-to-newest order.
pub fn (l &DebugLog) recent(n int) []string {
	if n <= 0 || l.lines.len == 0 {
		return []string{}
	}
	count := if n < l.lines.len { n } else { l.lines.len }
	return l.lines[l.lines.len - count..]
}

// FrameGraph keeps a bounded history of frame times for profiling overlays.
pub struct FrameGraph {
pub mut:
	samples []f32
	max     int = 120
}

// push records one delta time in seconds.
pub fn (mut g FrameGraph) push(dt f32) {
	g.samples << dt
	for g.samples.len > g.max && g.samples.len > 0 {
		g.samples = g.samples[1..]
	}
}

// average returns the mean of recorded frame times, or zero when empty.
pub fn (g &FrameGraph) average() f32 {
	if g.samples.len == 0 {
		return 0
	}
	mut sum := f32(0)
	for s in g.samples {
		sum += s
	}
	return sum / f32(g.samples.len)
}

// worst returns the largest recorded frame time.
pub fn (g &FrameGraph) worst() f32 {
	mut best := f32(0)
	for s in g.samples {
		if s > best {
			best = s
		}
	}
	return best
}

// GizmoKind selects the shape drawn for a queued debug gizmo.
pub enum GizmoKind {
	line
	circle
	rect
	cross
}

// Gizmo is one queued debug shape. b is the second point for lines, the
// bottom-right corner for rects, and unused for circles and crosses.
pub struct Gizmo {
pub mut:
	kind  GizmoKind
	a     gg.Vec2
	b     gg.Vec2
	color gg.Color
}

// Gizmos is a per-frame buffer of debug shapes, cleared after drawing.
pub struct Gizmos {
pub mut:
	items []Gizmo
}

// new_gizmos returns an empty buffer.
pub fn new_gizmos() Gizmos {
	return Gizmos{}
}

// line queues a line from a to b.
pub fn (mut g Gizmos) line(a gg.Vec2, b gg.Vec2, color gg.Color) {
	g.items << Gizmo{ kind: .line, a: a, b: b, color: color }
}

// circle queues a circle outline centered at center.
pub fn (mut g Gizmos) circle(center gg.Vec2, radius f32, color gg.Color) {
	g.items << Gizmo{ kind: .circle, a: center, b: gg.Vec2{radius, radius}, color: color }
}

// rect queues a rectangle outline from a to b.
pub fn (mut g Gizmos) rect(a gg.Vec2, b gg.Vec2, color gg.Color) {
	g.items << Gizmo{ kind: .rect, a: a, b: b, color: color }
}

// cross queues a crosshair centered at p with the given arm length.
pub fn (mut g Gizmos) cross(p gg.Vec2, size f32, color gg.Color) {
	g.items << Gizmo{ kind: .cross, a: p, b: gg.Vec2{size, size}, color: color }
}

// clear removes all queued gizmos.
pub fn (mut g Gizmos) clear() {
	g.items = []
}
