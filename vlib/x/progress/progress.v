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

// The narrowest the bar graphic itself may be squeezed to when the terminal
// is too small for the requested length.
const min_cells = 4

// cut_to shortens `s` so it occupies at most `n` terminal columns. It measures
// display width, not runes, so double-width glyphs (emoji, CJK) are counted as
// two and never overshoot. (string.limit() is not enough: it counts runes, so an
// emoji line would still overflow the terminal width.)
fn cut_to(s string, n int) string {
	if n <= 0 {
		return ''
	}
	if utf8_str_visible_length(s) <= n {
		return s
	}
	mut used := 0
	mut out := []rune{}
	for r in s.runes() {
		w := utf8_str_visible_length(r.str())
		if used + w > n {
			break
		}
		used += w
		out << r
	}
	return out.string()
}
