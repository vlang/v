import term

fn main() {
	term.erase_clear()
	sleeping_line(5,5,5,'*')
	standing_line(5,5,5,'*')
	sleeping_line(5,10,5,'*')
	standing_line(9,5,5,'*')
	term.cursor_down(5)
	print('\n')
	println(term.bold(term.red('It Worked!')))
}

fn sleeping_line(x,y,size int, ch string) {
	mut i := 0
	for i < size {
		term.set_cursor_position(x+i,y)
		print(term.bold(term.yellow(ch)))
		i++
	}
}

fn standing_line(x,y,size int, ch string) {
	mut i := 0
	for i < size {
		term.set_cursor_position(x,y+i)
		print(term.bold(term.yellow(ch)))
		i++
	}
}