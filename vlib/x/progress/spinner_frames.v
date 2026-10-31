module progress

// The spinner frame sets in this file come from the `spinners` table of
// https://github.com/schollz/progressbar (a Go library), and are used under its
// MIT license, which follows. The numbering is the same, so set N here is
// spinner N there. The data was converted mechanically, and checked against the
// upstream table to be identical.
//
//   MIT License
//
//   Copyright (c) 2017 Zack
//
//   Permission is hereby granted, free of charge, to any person obtaining a copy
//   of this software and associated documentation files (the "Software"), to deal
//   in the Software without restriction, including without limitation the rights
//   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//   copies of the Software, and to permit persons to whom the Software is
//   furnished to do so, subject to the following conditions:
//
//   The above copyright notice and this permission notice shall be included in all
//   copies or substantial portions of the Software.
//
//   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//   SOFTWARE.

// Spinner frame sets. Each is a list of frames; a spinner shows one frame at a
// time, advancing every `SpinnerOptions.interval`. Pass one as
// `Spinner.new(frames: progress.spinner_line())`, any other set by index
// (`frames: progress.spinner_set(27)`), or your own `[]string`.
//
// They are functions rather than constants because V will not assign a const
// array into a struct field; each call returns a fresh copy you may modify.
//
// Frames within a set may differ in width (the spinner pads them), and sets 70
// and 39 are emoji, which some terminals draw double-width.

// spinner_dots returns the frames of braille dots, the default.
pub fn spinner_dots() []string {
	return spinner_table[14].clone()
}

// spinner_braille returns the frames of braille, eight frames.
pub fn spinner_braille() []string {
	return spinner_table[11].clone()
}

// spinner_line returns the frames of the classic pipe, slash, dash, backslash.
pub fn spinner_line() []string {
	return spinner_table[9].clone()
}

// spinner_arrows returns the frames of a rotating arrow.
pub fn spinner_arrows() []string {
	return spinner_table[0].clone()
}

// spinner_circle returns the frames of a half-filled circle turning.
pub fn spinner_circle() []string {
	return spinner_table[7].clone()
}

// spinner_quarters returns the frames of quarter arcs.
pub fn spinner_quarters() []string {
	return spinner_table[40].clone()
}

// spinner_grow returns the frames of a block growing and shrinking.
pub fn spinner_grow() []string {
	return spinner_table[1].clone()
}

// spinner_bounce returns the frames of a dot bouncing between parentheses.
pub fn spinner_bounce() []string {
	return spinner_table[52].clone()
}

// spinner_pulse returns the frames of a dot travelling across three.
pub fn spinner_pulse() []string {
	return spinner_table[69].clone()
}

// spinner_ellipsis returns the frames of dots filling and emptying.
pub fn spinner_ellipsis() []string {
	return spinner_table[59].clone()
}

// spinner_earth returns the frames of emoji globe.
pub fn spinner_earth() []string {
	return spinner_table[39].clone()
}

// spinner_moon returns the frames of emoji moon phases.
pub fn spinner_moon() []string {
	return spinner_table[70].clone()
}

// spinner_set_count is the number of frame sets: spinner_set(n) accepts 0 .. spinner_set_count() - 1.
pub fn spinner_set_count() int {
	return spinner_table.len
}

// spinner_set returns frame set `n`, a fresh copy. Panics if `n` is out
// of range, as that is a programming error.
pub fn spinner_set(n int) []string {
	if n < 0 || n >= spinner_table.len {
		panic('progress: there is no spinner frame set ${n} (valid: 0..${spinner_table.len - 1})')
	}
	return spinner_table[n].clone()
}

// spinner_table holds every frame set. Public access is through spinner_set(n),
// which returns a copy, because V will not assign a const array to a field.
const spinner_table = [
	['←', '↖', '↑', '↗', '→', '↘', '↓', '↙'], // 0
	['▁', '▃', '▄', '▅', '▆', '▇', '█', '▇', '▆', '▅', '▄', '▃', '▁'], // 1
	['▖', '▘', '▝', '▗'], // 2
	['┤', '┘', '┴', '└', '├', '┌', '┬', '┐'], // 3
	['◢', '◣', '◤', '◥'], // 4
	['◰', '◳', '◲', '◱'], // 5
	['◴', '◷', '◶', '◵'], // 6
	['◐', '◓', '◑', '◒'], // 7
	['.', 'o', 'O', '@', '*'], // 8
	['|', '/', '-', '\\'], // 9
	['◡◡', '⊙⊙', '◠◠'], // 10
	['⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷'], // 11
	[">))'>", " >))'>", "  >))'>", "   >))'>", "    >))'>", "   <'((<", "  <'((<", " <'((<"], // 12
	['⠁', '⠂', '⠄', '⡀', '⢀', '⠠', '⠐', '⠈'], // 13
	['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'], // 14
	['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
		's', 't', 'u', 'v', 'w', 'x', 'y', 'z'], // 15
	['▉', '▊', '▋', '▌', '▍', '▎', '▏', '▎', '▍', '▌', '▋', '▊', '▉'], // 16
	['■', '□', '▪', '▫'], // 17
	['←', '↑', '→', '↓'], // 18
	['╫', '╪'], // 19
	['⇐', '⇖', '⇑', '⇗', '⇒', '⇘', '⇓', '⇙'], // 20
	['⠁', '⠁', '⠉', '⠙', '⠚', '⠒', '⠂', '⠂', '⠒', '⠲', '⠴', '⠤', '⠄',
		'⠄', '⠤', '⠠', '⠠', '⠤', '⠦', '⠖', '⠒', '⠐', '⠐', '⠒', '⠓', '⠋',
		'⠉', '⠈', '⠈'], // 21
	['⠈', '⠉', '⠋', '⠓', '⠒', '⠐', '⠐', '⠒', '⠖', '⠦', '⠤', '⠠', '⠠',
		'⠤', '⠦', '⠖', '⠒', '⠐', '⠐', '⠒', '⠓', '⠋', '⠉', '⠈'], // 22
	['⠁', '⠉', '⠙', '⠚', '⠒', '⠂', '⠂', '⠒', '⠲', '⠴', '⠤', '⠄', '⠄',
		'⠤', '⠴', '⠲', '⠒', '⠂', '⠂', '⠒', '⠚', '⠙', '⠉', '⠁'], // 23
	['⠋', '⠙', '⠚', '⠒', '⠂', '⠂', '⠒', '⠲', '⠴', '⠦', '⠖', '⠒', '⠐',
		'⠐', '⠒', '⠓', '⠋'], // 24
	['ｦ', 'ｧ', 'ｨ', 'ｩ', 'ｪ', 'ｫ', 'ｬ', 'ｭ', 'ｮ', 'ｯ', 'ｱ', 'ｲ', 'ｳ',
		'ｴ', 'ｵ', 'ｶ', 'ｷ', 'ｸ', 'ｹ', 'ｺ', 'ｻ', 'ｼ', 'ｽ', 'ｾ', 'ｿ', 'ﾀ',
		'ﾁ', 'ﾂ', 'ﾃ', 'ﾄ', 'ﾅ', 'ﾆ', 'ﾇ', 'ﾈ', 'ﾉ', 'ﾊ', 'ﾋ', 'ﾌ', 'ﾍ',
		'ﾎ', 'ﾏ', 'ﾐ', 'ﾑ', 'ﾒ', 'ﾓ', 'ﾔ', 'ﾕ', 'ﾖ', 'ﾗ', 'ﾘ', 'ﾙ', 'ﾚ',
		'ﾛ', 'ﾜ', 'ﾝ'], // 25
	['.', '..', '...'], // 26
	['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█', '▉', '▊', '▋', '▌', '▍',
		'▎', '▏', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█', '▇', '▆', '▅',
		'▄', '▃', '▂', '▁'], // 27
	['.', 'o', 'O', '°', 'O', 'o', '.'], // 28
	['+', 'x'], // 29
	['v', '<', '^', '>'], // 30
	['>>--->', ' >>--->', '  >>--->', '   >>--->', '    >>--->', '    <---<<', '   <---<<', '  <---<<',
		' <---<<', '<---<<'], // 31
	['|', '||', '|||', '||||', '|||||', '|||||||', '||||||||', '|||||||', '||||||', '|||||', '||||',
		'|||', '||', '|'], // 32
	['[          ]', '[=         ]', '[==        ]', '[===       ]', '[====      ]', '[=====     ]',
		'[======    ]', '[=======   ]', '[========  ]', '[========= ]', '[==========]'], // 33
	['(*---------)', '(-*--------)', '(--*-------)', '(---*------)', '(----*-----)', '(-----*----)',
		'(------*---)', '(-------*--)', '(--------*-)', '(---------*)'], // 34
	['█▒▒▒▒▒▒▒▒▒', '███▒▒▒▒▒▒▒',
		'█████▒▒▒▒▒', '███████▒▒▒',
		'██████████'], // 35
	['[                    ]', '[=>                  ]', '[===>                ]',
		'[=====>              ]', '[======>             ]', '[========>           ]',
		'[==========>         ]', '[============>       ]', '[==============>     ]',
		'[================>   ]', '[==================> ]', '[===================>]'], // 36
	['ဝ', '၀'], // 37
	['▌', '▀', '▐▄'], // 38
	['🌍', '🌎', '🌏'], // 39
	['◜', '◝', '◞', '◟'], // 40
	['⬒', '⬔', '⬓', '⬕'], // 41
	['⬖', '⬘', '⬗', '⬙'], // 42
	['[>>>          >]', '[]>>>>        []', '[]  >>>>      []', '[]    >>>>    []', '[]      >>>>  []',
		'[]        >>>>[]', '[>>          >>]'], // 43
	['♠', '♣', '♥', '♦'], // 44
	['➞', '➟', '➠', '➡', '➠', '➟'], // 45
	['  |  ', ' \\   ', '_    ', ' \\   ', '  |  ', '   / ', '    _', '   / '], // 46
	['  . . . .', '.   . . .', '. .   . .', '. . .   .', '. . . .  ', '. . . . .'], // 47
	[' |     ', '  /    ', '   _   ', '    \\  ', '     | ', '    \\  ', '   _   ', '  /    '], // 48
	['⎺', '⎻', '⎼', '⎽', '⎼', '⎻'], // 49
	['▹▹▹▹▹', '▸▹▹▹▹', '▹▸▹▹▹', '▹▹▸▹▹', '▹▹▹▸▹',
		'▹▹▹▹▸'], // 50
	['[    ]', '[   =]', '[  ==]', '[ ===]', '[====]', '[=== ]', '[==  ]', '[=   ]'], // 51
	['( ●    )', '(  ●   )', '(   ●  )', '(    ● )', '(     ●)', '(    ● )', '(   ●  )',
		'(  ●   )', '( ●    )'], // 52
	['✶', '✸', '✹', '✺', '✹', '✷'], // 53
	['▐|\\____________▌', '▐_|\\___________▌', '▐__|\\__________▌', '▐___|\\_________▌',
		'▐____|\\________▌', '▐_____|\\_______▌', '▐______|\\______▌',
		'▐_______|\\_____▌', '▐________|\\____▌', '▐_________|\\___▌',
		'▐__________|\\__▌', '▐___________|\\_▌', '▐____________|\\▌',
		'▐____________/|▌', '▐___________/|_▌', '▐__________/|__▌', '▐_________/|___▌',
		'▐________/|____▌', '▐_______/|_____▌', '▐______/|______▌', '▐_____/|_______▌',
		'▐____/|________▌', '▐___/|_________▌', '▐__/|__________▌', '▐_/|___________▌',
		'▐/|____________▌'], // 54
	['▐⠂       ▌', '▐⠈       ▌', '▐ ⠂      ▌', '▐ ⠠      ▌', '▐  ⡀     ▌',
		'▐  ⠠     ▌', '▐   ⠂    ▌', '▐   ⠈    ▌', '▐    ⠂   ▌',
		'▐    ⠠   ▌', '▐     ⡀  ▌', '▐     ⠠  ▌', '▐      ⠂ ▌',
		'▐      ⠈ ▌', '▐       ⠂▌', '▐       ⠠▌', '▐       ⡀▌',
		'▐      ⠠ ▌', '▐      ⠂ ▌', '▐     ⠈  ▌', '▐     ⠂  ▌',
		'▐    ⠠   ▌', '▐    ⡀   ▌', '▐   ⠠    ▌', '▐   ⠂    ▌',
		'▐  ⠈     ▌', '▐  ⠂     ▌', '▐ ⠠      ▌', '▐ ⡀      ▌',
		'▐⠠       ▌'], // 55
	['¿', '?'], // 56
	['⢹', '⢺', '⢼', '⣸', '⣇', '⡧', '⡗', '⡏'], // 57
	['⢄', '⢂', '⢁', '⡁', '⡈', '⡐', '⡠'], // 58
	['.  ', '.. ', '...', ' ..', '  .', '   '], // 59
	['.', 'o', 'O', '°', 'O', 'o', '.'], // 60
	['▓', '▒', '░'], // 61
	['▌', '▀', '▐', '▄'], // 62
	['⊶', '⊷'], // 63
	['▪', '▫'], // 64
	['□', '■'], // 65
	['▮', '▯'], // 66
	['-', '=', '≡'], // 67
	['d', 'q', 'p', 'b'], // 68
	['∙∙∙', '●∙∙', '∙●∙', '∙∙●', '∙∙∙'], // 69
	['🌑 ', '🌒 ', '🌓 ', '🌔 ', '🌕 ', '🌖 ', '🌗 ', '🌘 '], // 70
	['☗', '☖'], // 71
	['⧇', '⧆'], // 72
	['◉', '◎'], // 73
	['㊂', '㊀', '㊁'], // 74
	['⦾', '⦿'], // 75
]
