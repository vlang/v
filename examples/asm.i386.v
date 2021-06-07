fn main() {
	a := 100
	b := 20
	mut c := 0
	asm i386 {
		mov eax, a
		add eax, b
		mov c, eax
		; =r (c)
		; r (a)
		  r (b)
	}
	assert a == 100
	assert b == 20
	assert c == 120
	println('a: $a') // 100
	println('b: $b') // 20
	println('c: $c') // 120
}
