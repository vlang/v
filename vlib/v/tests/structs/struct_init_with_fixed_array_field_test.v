enum Piece {
	free
	x
	o
}

struct Game {
mut:
	board [9]Piece
}

fn test_struct_init_with_fixed_array_field() {
	s := 'xoxooxxxo'
	mut board := [9]Piece{}
	for i, ch in s {
		board[i] = match ch {
			`x` { Piece.x }
			`o` { Piece.o }
			else { Piece.free }
		}
	}
	println(board)
	assert '${board}' == '[x, o, x, o, o, x, x, x, o]'

	game := Game{board}
	println(game.board)
	assert '${game.board}' == '[x, o, x, o, o, x, x, x, o]'
}

// for issue 20361(part 1): returns struct with init mut fixed array fields without generics
struct Foo {
mut:
	buf [3]int
}

pub fn returns_struct_with_mut_fixed_array_init(mut fixed [3]int) Foo {
	return Foo{fixed}
}

fn test_returns_struct_with_mut_fixed_array_init() {
	mut fixed := [59, 101, 200]!
	mut foo := returns_struct_with_mut_fixed_array_init(mut fixed)
	assert foo.buf == [59, 101, 200]!
}

// for issue 20361(part 2): returns struct with init mut fixed array fields with generics
struct Bar[T] {
mut:
	buf T
}

pub fn returns_struct_with_mut_fixed_array_init_with_generics[T](mut fixed T) Bar[T] {
	return Bar[T]{fixed}
}

fn test_returns_struct_with_mut_fixed_array_init_with_generics() {
	mut fixed := [59, 101, 200]!
	mut bar := returns_struct_with_mut_fixed_array_init_with_generics(mut fixed)
	assert bar.buf == [59, 101, 200]!
}
