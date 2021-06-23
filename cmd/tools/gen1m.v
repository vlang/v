fn main() {
	for i in 0 .. 100000 {
		println('
fn foo${i}() {
	x := $i
	mut a := 1
	a += 2
	print(a)
	a = 0
	a = 1
}
')
	}
	// println('fn main() {foo1()} ')
	println('fn main() { println("1m DONE") } ')
}
