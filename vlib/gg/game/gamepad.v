// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module game

// max_gamepads is the number of controller slots tracked by default.
pub const max_gamepads = 4

// gamepad_button_count is the number of buttons exposed per controller.
pub const gamepad_button_count = 16

// gamepad_axis_count is the number of analog axes exposed per controller.
pub const gamepad_axis_count = 4

const gamepad_dead_zone = f32(0.15)

// Gamepad is a backend-neutral controller state container. It provides
// button held/pressed edges and a dead-zone filtered axis value. Hardware
// polling is deliberately left to your platform or web layer: call
// set_button, set_axis and end_frame from that layer. Keeping this pure V
// makes the same gameplay code work on every gg backend.
pub struct Gamepad {
pub mut:
	connected    bool
	id           string
	buttons      [16]bool
	prev_buttons [16]bool
	axes         [4]f32
}

// new_gamepad returns an empty, disconnected controller slot.
pub fn new_gamepad() Gamepad {
	return Gamepad{}
}

// set_connected marks the slot connected or disconnected and stores a label.
pub fn (mut g Gamepad) set_connected(value bool, id string) {
	g.connected = value
	g.id = id
	if !value {
		for i in 0 .. g.buttons.len {
			g.buttons[i] = false
			g.prev_buttons[i] = false
			g.axes[i % g.axes.len] = 0
		}
	}
}

// set_button updates one button state.
pub fn (mut g Gamepad) set_button(index int, down bool) {
	if index < 0 || index >= g.buttons.len {
		return
	}
	g.buttons[index] = down
}

// set_axis updates one analog axis, normally in [-1, 1].
pub fn (mut g Gamepad) set_axis(index int, value f32) {
	if index < 0 || index >= g.axes.len {
		return
	}
	g.axes[index] = value
}

// button_down reports whether a button is currently held.
pub fn (g &Gamepad) button_down(index int) bool {
	if index < 0 || index >= g.buttons.len {
		return false
	}
	return g.buttons[index]
}

// button_just_pressed reports whether a button went down since end_frame.
pub fn (g &Gamepad) button_just_pressed(index int) bool {
	if index < 0 || index >= g.buttons.len {
		return false
	}
	return g.buttons[index] && !g.prev_buttons[index]
}

// axis returns an axis value with small noise clamped to zero.
pub fn (g &Gamepad) axis(index int) f32 {
	if index < 0 || index >= g.axes.len {
		return 0
	}
	v := g.axes[index]
	if v > -gamepad_dead_zone && v < gamepad_dead_zone {
		return 0
	}
	return v
}

// end_frame copies current buttons to the previous state. Call it once per
// frame after gameplay reads button_just_pressed.
pub fn (mut g Gamepad) end_frame() {
	g.prev_buttons = g.buttons
}
