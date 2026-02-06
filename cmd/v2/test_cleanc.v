enum Color {
	red
	green
	blue
}

@[flag]
enum Permissions {
	read
	write
	execute
}

type MyInt = int

type Number = int | string

fn print_int(n int) {
	C.printf(c'%d\n', n)
}

fn print_str(s string) {
	C.puts(s.str)
}

interface Drawable {
	draw() int
}

struct Circle {
	radius int
}

fn (c Circle) draw() int {
	return c.radius
}

fn add(a int, b int) int {
	return a + b
}

fn sub(a int, b int) int {
	return a - b
}

fn mul(a int, b int) int {
	return a * b
}

fn main() {
	// 1. Basic variable declarations and arithmetic
	x := 3
	y := 2
	z := x + y
	print_int(z) // 5

	// 2. Function calls
	a := add(10, 20)
	print_int(a) // 30

	// 3. Nested function calls
	b := add(mul(3, 4), sub(10, 5))
	print_int(b) // 17

	// 4. Compound expressions
	c := (x + y) * 2
	print_int(c) // 10

	// 5. Multiple variables and operations
	d := 100
	e := d - 42
	print_int(e) // 58

	// 6. Boolean / comparison (int result)
	f := 10
	g := 20
	if f < g {
		print_int(1)
	}
	// 1

	// 7. String literal
	print_str('hello')

	// 8. Enum usage
	color := Color.red
	print_int(int(color)) // 0

	color2 := Color.blue
	print_int(int(color2)) // 2

	// 9. Flag enum
	perm := Permissions.read
	print_int(int(perm)) // 1

	// 10. Type alias
	my_val := MyInt(42)
	print_int(int(my_val)) // 42
}
