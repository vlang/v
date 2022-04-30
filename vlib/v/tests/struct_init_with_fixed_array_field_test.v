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
	assert '$board' == '[x, o, x, o, o, x, x, x, o]'

	game := Game{board}
	println(game.board)
	assert '$game.board' == '[x, o, x, o, o, x, x, x, o]'
}
