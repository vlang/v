/* fn test(cond bool) int {
	a := if cond { 10 } else { 16 }
	return a
} */

struct AA {
	a int
}

/* fn test(cond bool) AA {
	a := if cond { AA{a: 10} } else { AA{b: 125} }
	return a
} */

/* fn take(test AA) {

} */

/* fn give(cond bool) {
	take(if cond { AA{a: 15} } else  { AA{a: 20} })
} */



/* fn test(cond bool) (int, int) {
	return if cond { 10, 15 } else { 30, 35 }
} */

/* fn test() {
	mut a := 10
	a = 25
	a = 223
} */

fn main() {
	println(-33)
}

/* fn test() {
	mut a, b := 10, 2
	a *= b
} */

/* fn if_cond(cond bool) (int, int) {
	return if cond { 10, 15 } else { 30, 35 }
}

fn main() {
	mut a := 0
	mut b := 0
	a, b = if_cond(true)
	println(true)
	println(a)
	println(b)
	a, b = if_cond(false)
	println(false)
	println(a)
	println(b)
} */