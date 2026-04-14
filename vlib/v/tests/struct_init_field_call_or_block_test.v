module main

enum SeekMode {
	start
	current
	end
}

struct FileLike {}

fn (mut f FileLike) seek(pos int, mode SeekMode) !int {
	_ = mode
	return pos
}

struct Token {
	pos int
}

fn new_token(mut file FileLike, pos int) Token {
	return Token{
		pos: file.seek(pos, .start) or { return Token{} }
	}
}

struct HeapToken {
	kind   int
	offset int
	value  string
	pos    int
}

struct Lexer {
mut:
	file FileLike
}

fn new_heap_token(mut file FileLike, pos int) &HeapToken {
	return &HeapToken{0, 0, '', file.seek(pos, .start) or { return &HeapToken{} }}
}

fn (mut l Lexer) new_selector_heap_token(pos int) &HeapToken {
	return &HeapToken{0, 0, '', l.file.seek(pos, .start) or { return &HeapToken{} }}
}

fn test_struct_init_field_call_with_or_block() {
	mut file := FileLike{}
	token := new_token(mut file, 123)
	assert token.pos == 123
}

fn test_struct_init_field_call_with_or_block_in_heap_init() {
	mut file := FileLike{}
	token := new_heap_token(mut file, 456)
	assert token.pos == 456
}

fn test_struct_init_field_call_with_or_block_on_selector_receiver() {
	mut lexer := Lexer{}
	token := lexer.new_selector_heap_token(789)
	assert token.pos == 789
}
