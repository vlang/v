// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// Capabilities describes what the active gg backend can do. Addons should
// query this before using drawing or audio helpers so they can degrade
// gracefully instead of failing at compile time.
pub struct BackendCaps {
pub:
	backend     string
	can_draw    bool
	can_audio   bool
	depth       bool
	multiwindow bool
}

// capabilities returns the compile-time capabilities of the current backend.
pub fn backend_caps() BackendCaps {
	$if js {
		return BackendCaps{
			backend:     'js'
			can_draw:    false
			can_audio:   false
			depth:       false
			multiwindow: false
		}
	} $else {
		return BackendCaps{
			backend:     'native'
			can_draw:    true
			can_audio:   true
			depth:       true
			multiwindow: false
		}
	}
}

// Info holds lightweight draw statistics. gg does not auto-count every
// primitive; addons and apps can call record_draw from their own wrappers.
pub struct Info {
pub mut:
	draw_calls int
	triangles  int
	frames     int
}

// record_draw adds one draw call and its triangle count.
pub fn (mut i Info) record_draw(triangles int) {
	i.draw_calls++
	i.triangles += if triangles < 0 { 0 } else { triangles }
}

// end_frame records a completed frame and resets per-frame counters.
pub fn (mut i Info) end_frame() {
	i.frames++
	i.draw_calls = 0
	i.triangles = 0
}
