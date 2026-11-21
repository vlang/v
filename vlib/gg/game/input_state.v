// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module game

import gg

const input_key_count = 512

const input_mouse_count = 3

// InputState tracks held keys and buttons plus the one-frame press/release
// edges games need. Feed it from your own event callback, then call
// end_frame() once per frame after reading edges.
pub struct InputState {
mut:
	keys           []bool
	pressed        []bool
	released       []bool
	mouse_buttons  []bool
	mouse_pressed  []bool
	mouse_released []bool
	mouse_x        f32
	mouse_y        f32
	bindings       map[string][]gg.KeyCode
}

// new_input_state returns an empty input state with storage for all key codes.
pub fn new_input_state() InputState {
	return InputState{
		keys:           []bool{len: input_key_count}
		pressed:        []bool{len: input_key_count}
		released:       []bool{len: input_key_count}
		mouse_buttons:  []bool{len: input_mouse_count}
		mouse_pressed:  []bool{len: input_mouse_count}
		mouse_released: []bool{len: input_mouse_count}
		bindings:       map[string][]gg.KeyCode{}
	}
}

fn input_key_index(k gg.KeyCode) int {
	i := int(k)
	if i < 0 || i >= input_key_count {
		return -1
	}
	return i
}

fn input_mouse_index(b gg.MouseButton) int {
	i := int(b)
	if i < 0 || i >= input_mouse_count {
		return -1
	}
	return i
}

// on_key_down records a key press. Repeated down events do not re-fire the
// just_pressed edge.
pub fn (mut s InputState) on_key_down(k gg.KeyCode) {
	i := input_key_index(k)
	if i < 0 {
		return
	}
	if !s.keys[i] {
		s.pressed[i] = true
	}
	s.keys[i] = true
}

// on_key_up records a key release.
pub fn (mut s InputState) on_key_up(k gg.KeyCode) {
	i := input_key_index(k)
	if i < 0 {
		return
	}
	if s.keys[i] {
		s.released[i] = true
	}
	s.keys[i] = false
}

// on_mouse_button records a mouse button press or release at a position.
pub fn (mut s InputState) on_mouse_button(b gg.MouseButton, down bool, x f32, y f32) {
	i := input_mouse_index(b)
	if i < 0 {
		return
	}
	s.mouse_x = x
	s.mouse_y = y
	if down {
		if !s.mouse_buttons[i] {
			s.mouse_pressed[i] = true
		}
		s.mouse_buttons[i] = true
	} else {
		if s.mouse_buttons[i] {
			s.mouse_released[i] = true
		}
		s.mouse_buttons[i] = false
	}
}

// on_mouse_move updates the tracked cursor position.
pub fn (mut s InputState) on_mouse_move(x f32, y f32) {
	s.mouse_x = x
	s.mouse_y = y
}

// is_down reports whether a key is currently held.
pub fn (s &InputState) is_down(k gg.KeyCode) bool {
	i := input_key_index(k)
	return i >= 0 && s.keys[i]
}

// just_pressed reports whether a key went down since the last end_frame.
pub fn (s &InputState) just_pressed(k gg.KeyCode) bool {
	i := input_key_index(k)
	return i >= 0 && s.pressed[i]
}

// just_released reports whether a key went up since the last end_frame.
pub fn (s &InputState) just_released(k gg.KeyCode) bool {
	i := input_key_index(k)
	return i >= 0 && s.released[i]
}

// mouse_down reports whether a mouse button is currently held.
pub fn (s &InputState) mouse_down(b gg.MouseButton) bool {
	i := input_mouse_index(b)
	return i >= 0 && s.mouse_buttons[i]
}

// mouse_just_pressed reports a mouse button going down this frame.
pub fn (s &InputState) mouse_just_pressed(b gg.MouseButton) bool {
	i := input_mouse_index(b)
	return i >= 0 && s.mouse_pressed[i]
}

// mouse_just_released reports a mouse button going up this frame.
pub fn (s &InputState) mouse_just_released(b gg.MouseButton) bool {
	i := input_mouse_index(b)
	return i >= 0 && s.mouse_released[i]
}

// mouse_pos returns the last reported cursor position.
pub fn (s &InputState) mouse_pos() gg.Vec2 {
	return gg.Vec2{s.mouse_x, s.mouse_y}
}

// bind maps a named action to one or more keys.
pub fn (mut s InputState) bind(name string, keys []gg.KeyCode) {
	s.bindings[name] = keys
}

// action_down reports whether any key bound to name is held.
pub fn (s &InputState) action_down(name string) bool {
	keys := s.bindings[name] or { return false }
	for k in keys {
		if s.is_down(k) {
			return true
		}
	}
	return false
}

// action_just_pressed reports whether any key bound to name went down this
// frame.
pub fn (s &InputState) action_just_pressed(name string) bool {
	keys := s.bindings[name] or { return false }
	for k in keys {
		if s.just_pressed(k) {
			return true
		}
	}
	return false
}

// end_frame clears one-frame press and release edges. Call it after all
// gameplay reads for the frame.
pub fn (mut s InputState) end_frame() {
	for i in 0 .. s.keys.len {
		s.pressed[i] = false
		s.released[i] = false
	}
	for i in 0 .. s.mouse_buttons.len {
		s.mouse_pressed[i] = false
		s.mouse_released[i] = false
	}
}
