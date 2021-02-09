fn main() {
	a := 100
	b := 20
	c := 0 // set by asm
	asm x64 {
		'movl %[a], %[c]'
		'subl %[b], %[c]'
		: [c] "=r" (c) // output 
		: [a] "r" (a),
		  [b] "r" (b) // input 
		: '%ebx' // clobbered register 
	}

	println('a: $a') // 100
	println('b: $b') // 20
	println('c: $c') // 80
}
