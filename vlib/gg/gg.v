// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module gg

pub type FNCb = fn (data voidptr)

pub type FNEvent = fn (e &Event, data voidptr)

pub type FNFail = fn (msg string, data voidptr)

pub type FNKeyDown = fn (c KeyCode, m Modifier, data voidptr)

pub type FNKeyUp = fn (c KeyCode, m Modifier, data voidptr)

pub type FNMove = fn (x f32, y f32, data voidptr)

pub type FNClick = fn (x f32, y f32, button MouseButton, data voidptr)

pub type FNUnClick = fn (x f32, y f32, button MouseButton, data voidptr)

pub type FNChar = fn (c u32, data voidptr)

pub struct PenConfig {
	color     gx.Color
	line_type PenLineType = .solid
	thickness int = 1
}

pub struct Size {
pub mut:
	width  int
	height int
}
