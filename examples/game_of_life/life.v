module main

import time
import automaton

fn print_automaton(a &automaton.Automaton) {
	for y := 1; y < a.field.maxy; y++ {
		mut s := '    '
		for x := 1; x < a.field.maxx; x++ {
			cell := a.field.get(x, y)
			s += if cell == 1 { '@' } else { '.' }
		}
		println(s)
	}
	println('')
}

fn main() {
	mut a := automaton.gun()
	for {
		a.update()
		print_automaton(a)
		time.sleep(100 * time.millisecond)
	}
}
