struct Test {
	board [2][2]?Piece
}

struct Piece {
	white bool
}

fn test_main() {
	t := Test{
		board: [[?Piece{
			white: false
		}, ?Piece{
			white: false
		}]!, [?Piece{
			white: true
		}, ?Piece{
			white: true
		}]!]!
	}
	assert '${t.board[1][1]}' == 'Option(Piece{
    white: true
})'
	assert t.str() == 'Test{
    board: [[Option(Piece{
    white: false
}), Option(Piece{
    white: false
})], [Option(Piece{
    white: true
}), Option(Piece{
    white: true
})]]
}'
}
