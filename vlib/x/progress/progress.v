// Package progress draws terminal progress bars and spinners: one, or many at once.
//
// A single bar:
//
//	mut bar := progress.Bar.new(1000, progress.BarOptions{ text: 'work ' })
//	bar.start()
//	for _ in 0 .. 1000 {
//		bar.inc()
//	}
//	bar.wait()
//
// Several bars drawn together, advanced from different threads:
//
//	mut mb := progress.MultiBar.new()
//	mut a := mb.add_bar(100, progress.BarOptions{ text: 'a ' })
//	mut b := mb.add_bar(250, progress.BarOptions{ text: 'b ' })
//	mb.start()
//	// ... spawn workers that call a.inc() / b.add(n) ...
//	mb.wait()
//
// A spinner, for work with no known size, is driven the same way:
//
//	mut sp := progress.Spinner.new(progress.SpinnerOptions{ text: 'working ' })
//	sp.start()
//	// ... work ...
//	sp.finish()
//	sp.wait()
//
// MultiBar.add_spinner puts spinners and bars in the same display.
//
// Design notes
//
//   - Counters are atomic, so any number of threads may advance the same bar.
//   - Exactly one thread draws, at most every `MultiBarOptions.delay`
//     (default 50ms), and only when the picture changed.
//   - Output goes to stderr by default so stdout stays clean for piping.
//     When the output is not a terminal nothing live is drawn; each bar just
//     prints one final line when it completes (friendly to CI logs).
//   - The module installs nothing at import time. The cursor is hidden (and
//     SIGINT/SIGTERM hooks registered) only when a live display starts.
module progress

import term

// The narrowest the bar graphic itself may be squeezed to when the terminal
// is too small for the requested length.
const min_cells = 4

// cut_to shortens `s` so it occupies at most `n` terminal columns. It measures
// display width, not runes, so double-width glyphs (emoji, CJK) are counted as
// two and never overshoot. (string.limit() is not enough: it counts runes, so an
// emoji line would still overflow the terminal width.)
// A shortened line is plain text, so an escape cannot be split or left active.
fn cut_to(s string, n int) string {
	if n <= 0 {
		return ''
	}
	if term.printable_len(s) <= n {
		return s
	}
	mut used := 0
	mut out := []rune{}
	for r in plain_text(s).runes() {
		w := term.printable_len(r.str())
		if used + w > n {
			break
		}
		used += w
		out << r
	}
	return out.string()
}

// plain_text removes complete ANSI escapes using the same families as printable_len.
fn plain_text(s string) string {
	if !s.contains('\x1b') {
		return s
	}
	mut out := []u8{cap: s.len}
	mut i := 0
	for i < s.len {
		if s[i] == 0x1b {
			i = ansi_end(s, i)
		} else {
			out << s[i]
			i++
		}
	}
	return out.bytestr()
}

// ansi_end returns the first byte after the escape beginning at i.
fn ansi_end(s string, i int) int {
	mut j := i + 1
	if j >= s.len {
		return j
	}
	match s[j] {
		`[` { // CSI
			j++
			for j < s.len {
				if s[j] >= 0x40 && s[j] <= 0x7e {
					return j + 1
				}
				j++
			}
		}
		`]`, `P`, `X`, `^`, `_` { // OSC / DCS / SOS / PM / APC
			osc := s[j] == `]`
			j++
			for j < s.len {
				if osc && s[j] == 0x07 {
					return j + 1
				}
				if s[j] == 0x1b && j + 1 < s.len && s[j + 1] == `\\` {
					return j + 2
				}
				j++
			}
		}
		else {
			for j < s.len && s[j] >= 0x20 && s[j] <= 0x2f {
				j++
			}
			if j < s.len && s[j] >= 0x30 && s[j] <= 0x7e {
				return j + 1
			}
		}
	}
	return j
}
