module term_input

pub enum KeyCode {
	null      = 0
	tab       = 9
	enter     = 10
	escape    = 27
	space     = 32
	backspace = 127

	exclamation    = 33
	double_quote    = 34
	hashtag    = 35
	dollar    = 36
	percent    = 37
	ampersand    = 38
	single_quote    = 39
	left_paren    = 40
	right_paren    = 41
	asterisk = 42
	plus    = 43
	comma    = 44
	minus    = 45
	period    = 46
	slash    = 47

	_0       = 48
	_1       = 49
	_2       = 50
	_3       = 51
	_4       = 52
	_5       = 53
	_6       = 54
	_7       = 55
	_8       = 56
	_9       = 57

	colon         = 58
	semicolon     = 59
	less_than     = 60
	equal         = 61
	greater_than  = 62
	question_mark = 63
	at            = 64

	a        = 97
	b        = 98
	c        = 99
	d        = 100
	e        = 101
	f        = 102
	g        = 103
	h        = 104
	i        = 105
	j        = 106
	k        = 107
	l        = 108
	m        = 109
	n        = 110
	o        = 111
	p        = 112
	q        = 113
	r        = 114
	s        = 115
	t        = 116
	u        = 117
	v        = 118
	w        = 119
	x        = 120
	y        = 121
	z        = 122

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

    insert           = 260
    delete           = 261
    up               = 262
    down             = 263
    right            = 264
    left             = 265
    page_up          = 266
    page_down        = 267
    home             = 268
    end              = 269

	f1       = 290
	f2       = 291
	f3       = 292
	f4       = 293
	f5       = 294
	f6       = 295
	f7       = 296
	f8       = 297
	f9       = 298
	f10      = 299
	f11      = 300
	f12      = 301
}

pub const (
	shift = 1 << 0
	ctrl  = 1 << 1
	alt   = 1 << 2
)

fn single_char(buf string) &Event {
	ch := buf[0]

	mut event := &Event{
		typ: .key_down
		ascii: ch
		code: KeyCode(ch)
		utf8: buf
	}

	match ch {
		// ctrl + letter

		// TODO: Fix assoc in V and remove this workaround :/
		// 1  ... 26 { event = { event | code: KeyCode(ch + 96), modifiers: ctrl  } } // TODO: bit ops (i.e. `96 | ch`) for perf?
		// 65 ... 90 { event = { event | code: KeyCode(ch + 32), modifiers: shift } }

		1  ... 26 { event = &Event{ typ: event.typ, ascii: event.ascii, utf8: event.utf8, code: KeyCode(ch + 96), modifiers: ctrl } } // TODO: bit ops (i.e. `96 | ch`) for perf?
		65 ... 90 { event = &Event{ typ: event.typ, ascii: event.ascii, utf8: event.utf8, code: KeyCode(ch + 32), modifiers: shift } }

		else {}
	}

	return event
}

[inline]
fn escape_end(buf string) int {
	mut i := 0
	for {
		if i + 1 == buf.len { return buf.len }

		if buf[i].is_letter() {
			if buf[i] == `O` && i + 2 < buf.len {
				n := buf[i+1]
				if (n >= `A` && n <= `D`) || (n >= `P` && n <= `R`) || n == `F` || n == `H` {
					return i + 2
				}
			}
			return i + 1
		}
		i++
	}
}

fn escape_sequence(buf_ string) (&Event, int) {
	end := escape_end(buf_)
	buf := buf_[1 .. end] // skip escape character, and read until the ending of a sequence

	if buf.len == 0 {
		return &Event{
			typ: .key_down
			ascii: 27
			code: .escape
			utf8: buf
		}, 1
	}

	if buf.len == 1 {
		c := single_char(buf)
		// return { c | modifiers: c.modifiers | alt }, 2

		return &Event{
			typ: c.typ
			ascii: c.ascii
			code: c.code
			utf8: c.utf8
			modifiers: c.modifiers | alt
		}, 2
	}

	// ----------------
	//   Mouse events
	// ----------------

	if buf.len > 2 && buf[1] == `<` { // Mouse control
		split := buf[2..].split(';')
		if split.len < 3 { return &Event(0), 0 }


		typ, x, y := split[0].int(), split[1].int(), split[2].int()

		match typ {
			0, 2 {
				last := buf[buf.len - 1]
				button := if typ == 0 { MouseButton.primary } else { MouseButton.secondary }
				event := if last == `M` { EventType.mouse_down } else { EventType.mouse_up }
				return &Event{ typ: event, x: x, y: y, button: button }, end
			}
			32, 34 {
				button := if typ == 32 { MouseButton.primary } else { MouseButton.secondary }
				return &Event{ typ: .mouse_drag, x: x, y: y, button: button }, end
			}
			35 {
				return &Event{ typ: .mouse_move, x: x, y: y }, end
			}
			64, 65 {
				direction := if typ == 64 { Direction.down } else { Direction.up }
				return &Event{ typ: .mouse_scroll, x: x, y: y, direction: direction }, end
			} else {}
		}
	}

	// ----------------------------
	//   Special key combinations
	// ----------------------------

	mut code := KeyCode.null
	mut modifiers := u32(0)
	match buf {
		'[A', 'OA'                { code = .up    }
		'[B', 'OB'                { code = .down  }
		'[C', 'OC'                { code = .right }
		'[D', 'OD'                { code = .left  }
		'[5~', '[[5~'             { code = .page_up }
		'[6~', '[[6~'             { code = .page_down }
		'[F', 'OF', '[4~', '[[8~' { code = .end }
		'[H', 'OH', '[1~', '[[7~' { code = .home }
		'[2~'                     { code = .insert }
		'[3~'                     { code = .delete }
		'OP', '[11~'              { code = .f1 }
		'OQ', '[12~'              { code = .f2 }
		'OR', '[13~'              { code = .f3 }
		'OS', '[14~'              { code = .f4 }
		'[15~'                    { code = .f5 }
		'[17~'                    { code = .f6 }
		'[18~'                    { code = .f7 }
		'[19~'                    { code = .f8 }
		'[20~'                    { code = .f9 }
		'[21~'                    { code = .f10 }
		'[23~'                    { code = .f11 }
		'[24~'                    { code = .f12 }
		else                      {}
	}

	if buf.len == 5 && buf[0] == `[` && buf[1] == `1` && buf[2] == `;` && buf[3] >= `2` && buf[3] <= `8` && buf[4] >= `A` && buf[4] <= `D` {
		code = KeyCode(buf[4] + 197)
		modifiers = match buf[3] {
			`2` { shift }
			`3` { alt }
			`4` { shift | alt }
			`5` { ctrl }
			`6` { ctrl | shift }
			`7` { ctrl | alt }
			`8` { ctrl | alt | shift }
			else { 0 } // unreachable
		}
	}

	return &Event{ typ: .key_down, code: code, utf8: buf_[..end], modifiers: modifiers }, end
}
