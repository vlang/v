// Copyright (c) 2020 Raúl Hernández. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ui

pub enum KeyCode {
	null                 = 0
	tab                  = 9
	enter                = 10
	escape               = 27
	space                = 32
	backspace            = 127

	exclamation          = 33
	double_quote         = 34
	hashtag              = 35
	dollar               = 36
	percent              = 37
	ampersand            = 38
	single_quote         = 39
	left_paren           = 40
	right_paren          = 41
	asterisk             = 42
	plus                 = 43
	comma                = 44
	minus                = 45
	period               = 46
	slash                = 47

	_0                   = 48
	_1                   = 49
	_2                   = 50
	_3                   = 51
	_4                   = 52
	_5                   = 53
	_6                   = 54
	_7                   = 55
	_8                   = 56
	_9                   = 57

	colon                = 58
	semicolon            = 59
	less_than            = 60
	equal                = 61
	greater_than         = 62
	question_mark        = 63
	at                   = 64

	a                    = 97
	b                    = 98
	c                    = 99
	d                    = 100
	e                    = 101
	f                    = 102
	g                    = 103
	h                    = 104
	i                    = 105
	j                    = 106
	k                    = 107
	l                    = 108
	m                    = 109
	n                    = 110
	o                    = 111
	p                    = 112
	q                    = 113
	r                    = 114
	s                    = 115
	t                    = 116
	u                    = 117
	v                    = 118
	w                    = 119
	x                    = 120
	y                    = 121
	z                    = 122

	left_square_bracket  = 91
	backslash            = 92
	right_square_bracket = 93
	caret                = 94
	underscore           = 95
	backtick             = 96

	left_curly_bracket   = 123
	vertical_bar         = 124
	right_curly_bracket  = 125
	tilde                = 126

    insert               = 260
    delete               = 261
    up                   = 262
    down                 = 263
    right                = 264
    left                 = 265
    page_up              = 266
    page_down            = 267
    home                 = 268
    end                  = 269

	f1                   = 290
	f2                   = 291
	f3                   = 292
	f4                   = 293
	f5                   = 294
	f6                   = 295
	f7                   = 296
	f8                   = 297
	f9                   = 298
	f10                  = 299
	f11                  = 300
	f12                  = 301
}

pub const (
	shift = u32(1 << 0)
	ctrl  = u32(1 << 1)
	alt   = u32(1 << 2)
)

pub enum Direction {
	unknown
	up
	down
	left
	right
}

pub enum MouseButton {
	unknown
	left
	middle
	right
}

pub enum EventType {
	unknown
	mouse_down
	mouse_up
	mouse_move
	mouse_drag
	mouse_scroll
	key_down
	resized
}

pub struct Event {
pub:
	typ       EventType

	// Mouse event info
	x         int
	y         int
	button    MouseButton
	direction Direction

	// Keyboard event info
	code      KeyCode
	modifiers u32
	ascii     byte
	utf8      string

	// Resized event info
	width     int
	height    int
}

pub struct Context {
pub:
	cfg 		  Config
mut:
	read_buf      []byte
	print_buf     []byte
	paused        bool
	enable_su     bool
	enable_rgb    bool
pub mut:
	frame_count   u64
	window_width  int
	window_height int
}

pub struct Config {
	user_data            voidptr
	init_fn              fn(voidptr)
	frame_fn             fn(voidptr)
	cleanup_fn           fn(voidptr)
	event_fn             fn(&Event, voidptr)
	fail_fn              fn(string)

	buffer_size          int = 256
	frame_rate           int = 30
	use_x11              bool

	window_title         string
	hide_cursor          bool
	capture_events       bool
	use_alternate_buffer bool = true
	skip_init_checks     bool
	// All kill signals to set up exit listeners on
	reset                []int = [1, 2, 3, 4, 6, 7, 8, 9, 11, 13, 14, 15, 19]
}
