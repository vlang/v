module sion

struct Scanner{
mut:
	text			string
	pos				int
	last_nl_pos		int
	nlines 			int
	line_ends		[]int
	line_nr 		int
	line_comment 	string
	prev_tok		TokenKind
	table_name		string
	array_name		string
	started			bool
}

struct ScanRes{
	tok TokenKind
	lit string
}