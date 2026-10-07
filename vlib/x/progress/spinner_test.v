module progress

import term
import time

fn ms(n int) time.Duration {
	return n * time.millisecond
}

fn test_spinner_frame_follows_time() {
	mut s := Spinner.new(SpinnerOptions{
		frames:   ['a', 'b', 'c']
		interval: ms(100)
	})
	assert s.render(ms(0), false, 80) == 'a'
	assert s.render(ms(99), false, 80) == 'a'
	assert s.render(ms(100), false, 80) == 'b'
	assert s.render(ms(250), false, 80) == 'c'
	assert s.render(ms(300), false, 80) == 'a' // wraps
	assert s.render(ms(100 * 3 * 1_000_000 + 100), false, 80) == 'b' // long runs stay consistent
}

fn test_spinner_bad_options_fall_back() {
	// no frames -> the default set; zero interval -> 80ms (and no divide by zero)
	mut s := Spinner.new(SpinnerOptions{
		frames:   []string{}
		interval: 0
	})
	assert s.render(ms(0), false, 80) == spinner_dots()[0]
	assert s.render(ms(80), false, 80) == spinner_dots()[1]
}

fn test_spinner_layout_text_and_time() {
	mut s := Spinner.new(SpinnerOptions{
		frames:       ['-']
		text:         'work '
		show_elapsed: true
	})
	// frame, text (trailing space trimmed), elapsed: one space between parts
	assert s.render(5 * time.second, false, 80) == '- work 00:05'
	s.set_text('other')
	assert s.render(65 * time.second, false, 80) == '- other 01:05'
}

fn test_spinner_has_no_bar_columns() {
	mut s := Spinner.new(SpinnerOptions{
		frames: ['-']
		text:   'x'
	})
	line := s.render(5 * time.second, false, 80)
	assert line == '- x' // no percent, no rate, no ETA
	assert !line.contains('%')
}

// column of the text, measured in terminal columns (not bytes: the done mark
// is multi-byte)
fn text_column(line string) int {
	i := line.index('x') or { return -1 }
	return term.printable_len(line[..i])
}

fn test_spinner_text_does_not_jitter_when_frames_differ_in_width() {
	mut s := Spinner.new(SpinnerOptions{
		frames:   ['.', '..', '...']
		interval: ms(10)
		text:     'x'
	})
	a := text_column(s.render(ms(0), false, 80))
	b := text_column(s.render(ms(10), false, 80))
	c := text_column(s.render(ms(20), false, 80))
	assert a >= 0
	assert a == b
	assert b == c
	// the done marker is padded the same way
	assert text_column(s.render(ms(30), true, 80)) == a
}

fn test_spinner_done_shows_done_frame() {
	mut s := Spinner.new(SpinnerOptions{
		frames: ['a', 'b']
		text:   'x'
	})
	assert s.render(ms(500), true, 80) == '✓ x'
	mut custom := Spinner.new(SpinnerOptions{
		frames:     ['a', 'b']
		text:       'x'
		done_frame: 'OK'
	})
	assert custom.render(ms(500), true, 80).starts_with('OK')
}

fn test_spinner_with_no_text_has_no_trailing_space() {
	mut s := Spinner.new(SpinnerOptions{
		frames:   ['.', '..', '...']
		interval: ms(10)
	})
	assert s.render(ms(0), false, 80) == '.'
	assert s.render(ms(20), false, 80) == '...'
}

fn test_spinner_shortens_text_but_keeps_frame_and_time() {
	mut s := Spinner.new(SpinnerOptions{
		frames:       ['-']
		text:         'a very long label that cannot possibly fit in twenty columns'
		show_elapsed: true
	})
	line := s.render(7 * time.second, false, 20)
	assert term.printable_len(line) <= 19
	assert line.starts_with('- a very')
	assert line.contains('…')
	assert line.ends_with(' 00:07')
}

fn test_spinner_never_reaches_last_column() {
	for width in [2, 3, 5, 8, 12, 20, 40, 80] {
		for set in [spinner_dots(), spinner_set(54), spinner_set(52), spinner_moon()] {
			mut s := Spinner.new(SpinnerOptions{
				frames:       set
				text:         'some fairly long text here '
				show_elapsed: true
			})
			for t in [0, 90, 180, 1000] {
				line := s.render(ms(t), false, width)
				assert term.printable_len(line) <= width - 1
			}
		}
	}
}

fn test_spinner_normalises_trailing_spaces_in_frames() {
	// emoji sets ship with a trailing space; it must not double up with the separator
	mut s := Spinner.new(SpinnerOptions{
		frames: ['A ', 'B ']
		text:   'x'
	})
	assert s.render(ms(0), false, 80) == 'A x'
	mut moon := Spinner.new(SpinnerOptions{
		frames: spinner_moon()
		text:   'x'
	})
	assert moon.render(ms(0), false, 80).ends_with(' x')
	assert !moon.render(ms(0), false, 80).contains('  ')
	// a frame that is only spaces still animates (blank), at the same width
	mut dots := Spinner.new(SpinnerOptions{
		frames: spinner_ellipsis()
		text:   'x'
	})
	for i in 0 .. 6 {
		assert text_column(dots.render(ms(80 * i), false, 80)) == 4
	}
}
