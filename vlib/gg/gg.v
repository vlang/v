// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module gg

pub type FNCb = fn (data voidptr)

// Define the type of function that can be given to a Config and that will be called on every events
pub type FNEvent = fn (e &Event, data voidptr)

pub type FNEvent2 = fn (data voidptr, e &Event)

pub type FNFail = fn (msg string, data voidptr)

// Define the type of function that can be given to a Config and that will be called every time a key is pressed down
pub type FNKeyDown = fn (c KeyCode, m Modifier, data voidptr)

// Define the type of function that can be given to a Config and that will be called every time a key come back tp the up position
pub type FNKeyUp = fn (c KeyCode, m Modifier, data voidptr)

// Define the type of function that can be given to a Config and that will be called every time the mouse move on the screen
pub type FNMove = fn (x f32, y f32, data voidptr)

// Define the type of function that can be given to a Config and that will be called every time the mouse is pressed
pub type FNClick = fn (x f32, y f32, button MouseButton, data voidptr)

// Define the type of function that can be given to a Config and that will be called every time the mouse is released
pub type FNUnClick = fn (x f32, y f32, button MouseButton, data voidptr)

pub type FNChar = fn (c u32, data voidptr)

pub struct PenConfig {
pub:
	color     Color
	line_type PenLineType = .solid
	thickness f32         = 1
}

@[markused]
pub struct Size {
pub mut:
	width  int
	height int
}
