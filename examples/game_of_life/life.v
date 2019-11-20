module main
import time
import automaton

fn print_automaton(a &automaton.Automaton){
	for line in a.field {
		mut s := '    '
		for j, cell in line {
			if j == 0 || j == line.len - 1{continue}
			s += if cell == 1{'@'} else { '.'}
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
		time.sleep_ms(100)
	}
}

