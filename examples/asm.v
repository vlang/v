fn main() {
	a := 100
	b := 20
	c := 0
	asm x64 {
		'mov %[c], %[a]'
		'add %[c], %[b]'
		: [c] "=r" (c) // output 
		: [a] "r" (a),// input 
		  [b] "r" (b) 
	}

	println('a: $a') // 100
	println('b: $b') // 20
	println('c: $c') // 120
}
