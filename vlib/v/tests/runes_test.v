module main

fn test_main() {
	s := 'hello'
	arr := s.runes()
	assert arr.len == 5
}
