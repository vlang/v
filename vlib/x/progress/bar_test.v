module progress

import term
import time

fn test_bar_counting() {
	mut b := Bar.new(10)
	assert b.value() == 0
	assert !b.is_done()
	b.inc()
	b.add(4)
	b.add(0) // ignored
	b.add(-3) // ignored
	assert b.value() == 5
	assert !b.is_done()
	b.add(100) // overshoot is clamped, not an error
	assert b.value() == 10
	assert b.is_done()
}

fn test_bar_large_increments_saturate_without_overflow() {
	mut b := Bar.new(10)
	b.inc()
	b.add(max_i64)
	assert b.value() == 10
	assert b.is_done()
	b.add(max_i64)
	b.inc()
	assert b.value() == 10
	assert b.is_done()

	mut huge := Bar.new(max_i64)
	huge.add(max_i64 - 1)
	assert huge.value() == max_i64 - 1
	assert !huge.is_done()
	huge.add(2)
	assert huge.value() == max_i64
	assert huge.is_done()
}

fn test_bar_set_and_finish() {
	mut b := Bar.new(100)
	b.set(40)
	assert b.value() == 40
	b.set(-5)
	assert b.value() == 0
	b.set(500)
	assert b.value() == 100
	assert b.is_done()

	mut c := Bar.new(100)
	c.add(10)
	c.finish() // done early, e.g. after an error
	assert c.is_done()
	assert c.value() == 10
}

fn test_empty_bar_is_already_done() {
	mut b := Bar.new(0) // e.g. "copy 0 files": must not panic or divide by zero
	assert b.is_done()
	snap := b.snapshot()
	assert snap.fraction() == 1.0
}

fn test_elapsed_freezes_on_completion() {
	mut b := Bar.new(2)
	b.add(2)
	e1 := b.elapsed()
	time.sleep(20 * time.millisecond)
	assert b.elapsed() == e1
}

fn count_worker(mut b Bar, n int) {
	for _ in 0 .. n {
		b.inc()
	}
}

fn test_concurrent_adds_lose_nothing() {
	mut b := Bar.new(8 * 100_000)
	mut ts := []thread{}
	for _ in 0 .. 8 {
		ts << spawn count_worker(mut b, 100_000)
	}
	ts.wait()
	assert b.value() == 800_000
	assert b.is_done()
}

fn test_render_exact_line() {
	mut b := Bar.new(100, BarOptions{
		text:         'x '
		length:       20
		show_rate:    true
		show_elapsed: true
	})
	snap := Snapshot{
		value:   50
		max:     100
		elapsed: 10 * time.second
		rate:    5.0
	}
	assert b.render(snap, 200) == 'x  50.00% [==========>         ]   5.0it/s 00:10 00:10'
}

fn test_render_unknown_rate_shows_placeholders() {
	mut b := Bar.new(100, BarOptions{
		length:    10
		show_rate: true
	})
	line := b.render(Snapshot{ value: 3, max: 100, rate: -1.0 }, 200)
	assert line.contains('--.-it/s')
	assert line.ends_with('--:--')
	// zero speed is "known", but an ETA from it would be nonsense
	line2 := b.render(Snapshot{ value: 3, max: 100, rate: 0.0 }, 200)
	assert line2.contains('  0.0it/s')
	assert line2.ends_with('--:--')
}

fn test_render_done_line() {
	mut b := Bar.new(100, BarOptions{ length: 10 })
	line := b.render(Snapshot{ value: 100, max: 100, rate: 50.0, done: true }, 200)
	assert line == '100.00% [==========] 00:00'
}

fn test_render_never_reaches_last_column() {
	for style in [Style(ClassicStyle{}), Style(BlockStyle{})] {
		for width in [12, 20, 30, 40, 60, 80, 120] {
			for len in [0, 10, 30, 200] {
				mut b := Bar.new(100, BarOptions{
					text:         'downloading '
					length:       len
					style:        style
					show_rate:    true
					show_elapsed: true
				})
				line := b.render(Snapshot{
					value:   37
					max:     100
					elapsed: 83 * time.second
					rate:    1234.5
				}, width)
				assert term.printable_len(line) <= width - 1
			}
		}
	}
}

fn test_render_length_zero_fills_terminal() {
	mut b := Bar.new(100, BarOptions{ length: 0 })
	line := b.render(Snapshot{ value: 10, max: 100, rate: 1.0 }, 80)
	assert term.printable_len(line) == 79
}

fn test_render_shrinks_bar_before_cutting_text() {
	mut b := Bar.new(100, BarOptions{
		text:   'name '
		length: 50
	})
	line := b.render(Snapshot{ value: 50, max: 100, rate: 1.0 }, 50)
	assert line.starts_with('name ')
	assert line.contains(']') // the frame survived: the bar was squeezed, not truncated
	assert term.printable_len(line) <= 49
}

fn test_set_text() {
	mut b := Bar.new(10, BarOptions{ text: 'a ' })
	b.set_text('b ')
	line := b.render(Snapshot{ value: 1, max: 10, rate: 1.0 }, 80)
	assert line.starts_with('b ')
}
