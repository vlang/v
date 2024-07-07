import os
import rand
import term
import term.termios
import time

const snooze = time.millisecond * 70
const symbols = '0123456789!@#$%^&*()-_=+[]{}|;:,.<>?¡±¥£¶ÿ'

struct RainColumn {
mut:
	col  int
	len  int // length of the rain column
	head int = 1 // y position of the head of rain column
}

fn main() {
	init_terminal()!
	rain() // ctrl-c to exit
}

fn rain() {
	mut rain_columns := []RainColumn{}
	width, height := term.get_terminal_size()

	for {
		// gradually add more rain columns
		if rain_columns.len < width {
			rain_columns << random_rain_column(width, height)
		}
		// update and print all rain columns
		for mut rc in rain_columns {
			update_rain_column(mut rc, width, height)
			print_rain_column(rc, height)
		}
		time.sleep(snooze)
	}
}

fn update_rain_column(mut rc RainColumn, width int, height int) {
	rc.head += 1
	if rc.head >= height + rc.len + 1 {
		rc = random_rain_column(width, height)
	}
}

fn random_rain_column(max_col int, max_height int) RainColumn {
	return RainColumn{
		col: rand.int_in_range(1, max_col + 1) or { 1 }
		len: rand.int_in_range(4, max_height / 4 * 3) or { 4 }
	}
}

fn print_rain_column(rc RainColumn, height int) {
	// print head in white
	if rc.head <= height {
		print_at(random_symbol(), rc.col, rc.head)
	}
	// print the char above the head in green to remove
	// white color of the previous head. Dim chars
	// randomly to add more interest to the effect
	if (rc.head - 1) <= height {
		symbol := random_dim(term.green(random_symbol()))
		print_at(symbol, rc.col, rc.head - 1)
	}
	// remove tail by printing a space
	t := rc.head - rc.len + 1
	if t >= 0 && t <= height {
		print_at(' ', rc.col, t)
	}
}

fn print_at(s string, x int, y int) {
	term.set_cursor_position(term.Coord{ x: x, y: y })
	print(s)
}

fn random_symbol() string {
	idx := rand.int_in_range(0, symbols.len) or { 0 }
	return symbols[idx].ascii_str()
}

fn random_dim(s string) string {
	i := rand.int_in_range(0, 10) or { 0 }
	return if i == 1 { term.dim(s) } else { s }
}

fn init_terminal() ! {
	mut old_state := termios.Termios{}
	termios.tcgetattr(0, mut old_state)
	// restore cursor, exit alternate buffer mode on Ctrl+C
	os.signal_opt(os.Signal.int, fn [mut old_state] (sig os.Signal) {
		println('\e[?1049l\e[?25h')
		termios.set_state(0, old_state)
		exit(0)
	})!
	// turn off cursor, enter alternate buffer mode
	print('\e[?1049h\e[?25l')
	mut new_state := old_state
	new_state.disable_echo()
	termios.set_state(0, new_state)
}
