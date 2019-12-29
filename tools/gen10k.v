fn main() {
	for i in 0..10000 {
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
	println('fn main() {foo1()} ')

}
