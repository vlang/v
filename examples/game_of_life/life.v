import time
import automaton

fn print_field(field []array_int) {
	for line in field {
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
	mut field := automaton.gun()
	print_field(field)
	for {
		field = automaton.update(field)
		print_field(field)
		time.sleep_ms(100)
	}
}

