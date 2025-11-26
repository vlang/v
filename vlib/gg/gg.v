// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module gg

pub type FNCb = fn (data voidptr)

// FNEvent defines the type of a function that will be called for every event
pub type FNEvent = fn (e &Event, data voidptr)

// FNEvent2 same as FNEvent with inverted arguments TODO: deprecate this, in favor of event_fn
pub type FNEvent2 = fn (data voidptr, e &Event)

// FNFail defines the type of a function that will be called when there is a fail
pub type FNFail = fn (msg string, data voidptr)

// FNKeyDown defines the type of a function that will be called for every key pressed down
pub type FNKeyDown = fn (c KeyCode, m Modifier, data voidptr)

// FNKeyUp defines the type of a function that will be called for every release of a pressed key
pub type FNKeyUp = fn (c KeyCode, m Modifier, data voidptr)

// FNMove defines the type of a function that will be called for every mouse move on the screen
pub type FNMove = fn (x f32, y f32, data voidptr)

// FNClick defines the type of a function that will be called for every mouse click
pub type FNClick = fn (x f32, y f32, button MouseButton, data voidptr)

// FNUnClick defines the type of a function that will be called every time a mouse button is released
pub type FNUnClick = fn (x f32, y f32, button MouseButton, data voidptr)

// FNChar defines the type of a function that will be called once per character
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
