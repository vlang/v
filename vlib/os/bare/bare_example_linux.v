fn main() {
	sys_write(1, c'hello\n', 6)
	s := 'test string\n'
	sys_write(1, s.str, u64(s.len))
	a := s[0]
	println('Hello freestanding!')
	println(a)
}
