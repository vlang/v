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

fn test(cond bool) AA {
	a := if cond { AA{} } else { AA{a: 125} }
	return a
}

/* fn test(cond bool) (int, int) {
	return if cond { 10, 15 } else { 30, 35 }
} */