fn main() {
	a := 100
	b := 20
	c := 0
	unsafe asm amd64 {
		mov eax, a
		add eax, b
		mov c, eax
		: =r (c) as c // output 
		: r (a) as a // input 
		  r (b) as b
	}
	println('a: $a') // 100
	println('b: $b') // 20
	println('c: $c') // 120
}
