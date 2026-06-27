module main

struct Point {
mut:
	x int
	y int
}

struct Rect {
	origin Point
	width  int
	height int
}

enum Color {
	red
	green
	blue
}

enum Direction {
	up
	down
	left
	right
}

fn add(a int, b int) int {
	return a + b
}

fn multiply(a int, b int) int {
	return a * b
}

fn (p Point) sum() int {
	return p.x + p.y
}

fn (p Point) scale(factor int) Point {
	return Point{
		x: p.x * factor
		y: p.y * factor
	}
}

fn (r Rect) area() int {
	return r.width * r.height
}

fn max(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

fn abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

fn fibonacci(n int) int {
	if n <= 1 {
		return n
	}
	mut a := 0
	mut b := 1
	for i := 2; i <= n; i++ {
		c := a + b
		a = b
		b = c
	}
	return b
}

fn color_name(c Color) string {
	if int(c) == 0 {
		return 'red'
	} else if int(c) == 1 {
		return 'green'
	} else if int(c) == 2 {
		return 'blue'
	}
	return 'unknown'
}

fn collatz_steps(start int) int {
	mut n := start
	mut steps := 0
	for n > 1 {
		if n % 2 == 0 {
			n = n / 2
		} else {
			n = n * 3 + 1
		}
		steps = steps + 1
	}
	return steps
}

fn sum_range(start int, end int) int {
	mut total := 0
	for i := start; i < end; i++ {
		total = total + i
	}
	return total
}

fn is_even(n int) bool {
	return n % 2 == 0
}

fn count_even(limit int) int {
	mut c := 0
	for i := 0; i < limit; i++ {
		if is_even(i) {
			c = c + 1
		}
	}
	return c
}

fn direction_dx(d Direction) int {
	if int(d) == 2 {
		return -1
	} else if int(d) == 3 {
		return 1
	}
	return 0
}

fn direction_dy(d Direction) int {
	if int(d) == 0 {
		return -1
	} else if int(d) == 1 {
		return 1
	}
	return 0
}

__global g_val int

fn fib_recursive(n int) int {
	if n <= 1 {
		return n
	}
	return fib_recursive(n - 1) + fib_recursive(n - 2)
}

fn factorial_recursive(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial_recursive(n - 1)
}

fn gcd(a int, b int) int {
	if b == 0 {
		return a
	}
	return gcd(b, a % b)
}

fn power(base int, exp int) int {
	mut result := 1
	for i := 0; i < exp; i++ {
		result = result * base
	}
	return result
}

fn swap_point(mut p Point) {
	tmp := p.x
	p.x = p.y
	p.y = tmp
}

fn scale_point(mut p Point, factor int) {
	p.x = p.x * factor
	p.y = p.y * factor
}

fn nested_return(x int) int {
	if x < 10 {
		return 100
	} else {
		if x < 20 {
			return 200
		} else {
			return 300
		}
	}
}

fn sum_many(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a + b + c + d + e + f + g + h
}

fn clamp(x int, lo int, hi int) int {
	if x < lo {
		return lo
	} else if x > hi {
		return hi
	}
	return x
}

fn print_str(s string) {
	C.puts(s.str)
}

fn print_int(n int) {
	C.printf(c'%d\n', n)
}

fn main() {
	// 1. Basic arithmetic
	x := 10
	y := 20
	z := add(x, y)

	if z > 25 {
		print_str('big')
	} else {
		print_str('small')
	}

	// 2. For loop with sum
	mut sum := 0
	for i := 0; i < 5; i++ {
		sum = sum + i
	}
	if sum == 10 {
		print_str('sum ok')
	}

	// 3. Struct and method
	p := Point{
		x: 3
		y: 4
	}
	ps := p.sum()
	if ps == 7 {
		print_str('struct ok')
	}

	// 4. While loop
	mut count := 3
	for count > 0 {
		count = count - 1
	}
	if count == 0 {
		print_str('loop ok2')
	}

	// 5. Print the sum result
	print_int(z)

	// 6. Nested function calls
	result := add(multiply(3, 4), multiply(5, 6))
	if result == 42 {
		print_str('nested ok')
	}

	// 7. Multiple comparisons and boolean logic
	a := 15
	b := 25
	if a < b && b < 30 {
		print_str('logic ok')
	}

	if a == 15 || b == 99 {
		print_str('or ok')
	}

	// 8. Negation and abs
	neg := -42
	pos := abs(neg)
	if pos == 42 {
		print_str('abs ok')
	}

	// 9. Nested if/else
	val := 50
	if val < 0 {
		print_str('negative')
	} else if val == 0 {
		print_str('zero')
	} else if val < 100 {
		print_str('medium')
	} else {
		print_str('large')
	}

	// 10. Chained arithmetic
	chain := (10 + 20) * 3 - 5
	if chain == 85 {
		print_str('chain ok')
	}

	// 11. Bitwise operations
	bw := 0xFF & 0x0F
	if bw == 15 {
		print_str('bit and ok')
	}

	bw2 := 0xA0 | 0x05
	if bw2 == 0xA5 {
		print_str('bit or ok')
	}

	bw3 := 1 << 4
	if bw3 == 16 {
		print_str('shift ok')
	}

	// 12. Struct method returning struct
	p2 := Point{
		x: 2
		y: 3
	}
	p3 := p2.scale(10)
	if p3.x == 20 && p3.y == 30 {
		print_str('scale ok')
	}

	// 13. Nested struct
	r := Rect{
		origin: Point{
			x: 1
			y: 2
		}
		width:  10
		height: 5
	}
	if r.area() == 50 {
		print_str('rect ok')
	}
	if r.origin.x == 1 && r.origin.y == 2 {
		print_str('nested struct ok')
	}

	// 14. Fibonacci
	fib := fibonacci(10)
	if fib == 55 {
		print_str('fib ok')
	}

	// 15. Max function
	m := max(33, 77)
	if m == 77 {
		print_str('max ok')
	}

	// 16. Multiple mutable updates
	mut acc := 1
	for j := 1; j <= 5; j++ {
		acc = acc * j
	}
	if acc == 120 {
		print_str('factorial ok')
	}

	// 17. Comparison operators
	if 10 >= 10 {
		print_str('ge ok')
	}
	if 9 <= 10 {
		print_str('le ok')
	}
	if 10 != 11 {
		print_str('ne ok')
	}

	// 18. Modulo
	rem := 17 % 5
	if rem == 2 {
		print_str('mod ok')
	}

	// 19. Mutable struct fields
	mut mp := Point{
		x: 10
		y: 20
	}
	mp.x = 100
	mp.y = mp.x + 50
	if mp.x == 100 && mp.y == 150 {
		print_str('mut struct ok')
	}

	// 20. Division
	div := 100 / 3
	if div == 33 {
		print_str('div ok')
	}

	// 21. Enum values
	c1 := Color.red
	c2 := Color.blue
	if int(c1) == 0 && int(c2) == 2 {
		print_str('enum ok')
	}

	// 22. Enum in function
	name := color_name(Color.green)
	C.printf(c'color: %s\n', name.str)

	// 23. Bool return value
	if is_even(42) {
		print_str('even ok')
	}
	if !is_even(7) {
		print_str('odd ok')
	}

	// 24. Nested loops
	mut total := 0
	for i := 0; i < 3; i++ {
		for j := 0; j < 4; j++ {
			total = total + 1
		}
	}
	if total == 12 {
		print_str('nested loops ok')
	}

	// 25. Break in loop
	mut found := -1
	for i := 0; i < 100; i++ {
		if i * i > 50 {
			found = i
			break
		}
	}
	if found == 8 {
		print_str('break ok')
	}

	// 26. Continue in loop
	mut odd_sum := 0
	for i := 0; i < 10; i++ {
		if is_even(i) {
			continue
		}
		odd_sum = odd_sum + i
	}
	if odd_sum == 25 {
		print_str('continue ok')
	}

	// 27. Enum as direction
	dx := direction_dx(Direction.right)
	dy := direction_dy(Direction.down)
	if dx == 1 && dy == 1 {
		print_str('direction ok')
	}

	// 28. Compound assignment operators
	mut ca := 10
	ca += 5
	ca -= 3
	ca *= 2
	if ca == 24 {
		print_str('compound ok')
	}

	// 29. XOR
	xr := 0xFF ^ 0x0F
	if xr == 0xF0 {
		print_str('xor ok')
	}

	// 30. Right shift
	rs := 256 >> 3
	if rs == 32 {
		print_str('rshift ok')
	}

	// 31. Collatz conjecture (complex loop logic)
	steps := collatz_steps(27)
	if steps == 111 {
		print_str('collatz ok')
	}

	// 32. count_even (function call in loop condition body)
	ev := count_even(10)
	if ev == 5 {
		print_str('count ok')
	}

	// 33. Struct with zero fields
	origin := Point{}
	if origin.x == 0 && origin.y == 0 {
		print_str('zero struct ok')
	}

	// 34. Multiple early returns
	m2 := max(max(10, 20), max(15, 5))
	if m2 == 20 {
		print_str('multi ret ok')
	}

	// 35. sum_range
	sr := sum_range(1, 11)
	if sr == 55 {
		print_str('range sum ok')
	}

	// 36. Recursive fibonacci
	rf := fib_recursive(10)
	if rf == 55 {
		print_str('rec fib ok')
	}

	// 37. Recursive factorial
	fact := factorial_recursive(6)
	if fact == 720 {
		print_str('rec fact ok')
	}

	// 38. GCD
	g := gcd(48, 18)
	if g == 6 {
		print_str('gcd ok')
	}

	// 39. Power
	pw := power(2, 10)
	if pw == 1024 {
		print_str('power ok')
	}

	// 40. Heap allocation (&Point{})
	hp := &Point{
		x: 42
		y: 84
	}
	if hp.x == 42 && hp.y == 84 {
		print_str('heap ok')
	}

	// 41. Mut struct parameter (swap)
	mut sp := Point{
		x: 10
		y: 20
	}
	swap_point(mut sp)
	if sp.x == 20 && sp.y == 10 {
		print_str('swap ok')
	}

	// 42. Mut struct parameter (scale)
	mut sc := Point{
		x: 5
		y: 3
	}
	scale_point(mut sc, 10)
	if sc.x == 50 && sc.y == 30 {
		print_str('mut param ok')
	}

	// 43. Nested return (all branches return)
	nr1 := nested_return(5)
	nr2 := nested_return(15)
	nr3 := nested_return(25)
	if nr1 == 100 && nr2 == 200 && nr3 == 300 {
		print_str('nested ret ok')
	}

	// 44. Global variable
	g_val = 100
	g_val += 50
	g_val -= 20
	if g_val == 130 {
		print_str('global ok')
	}

	// 45. Many arguments (8 args)
	sm := sum_many(1, 2, 3, 4, 5, 6, 7, 8)
	if sm == 36 {
		print_str('8 args ok')
	}

	// 46. Infinite loop with break
	mut inf := 0
	for {
		inf++
		if inf == 10 {
			break
		}
	}
	if inf == 10 {
		print_str('inf loop ok')
	}

	// 47. Assert
	assert 2 + 2 == 4
	assert 10 > 5
	print_str('assert ok')

	// 48. Double negation
	dn := !!true
	if dn {
		print_str('double neg ok')
	}

	// 49. Clamp
	cl1 := clamp(5, 0, 10)
	cl2 := clamp(-5, 0, 10)
	cl3 := clamp(15, 0, 10)
	if cl1 == 5 && cl2 == 0 && cl3 == 10 {
		print_str('clamp ok')
	}

	// 50. Heap struct method call
	hp2 := &Point{
		x: 7
		y: 8
	}
	hs := hp2.sum()
	if hs == 15 {
		print_str('heap method ok')
	}
}
