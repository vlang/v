fn main() {
	println('fn println(a int) {}')
	println('fn print(a string) {}')
	for i in 0..100000 {
		println('
fn foo${i}() {
	x := $i
	mut a := x
	a += 2
	println(a)
	a = 0
	a = 1
}
')
	}
	//println('fn main() {foo1()} ')
	println('fn main() { print("1m DONE") } ')

}
