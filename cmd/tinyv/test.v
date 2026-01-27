struct Point {
mut:
	x int
	y int
}

struct Rectangle {
mut:
	width  int
	height int
	origin Point
}

struct Node {
mut:
	value int
	left  int
	right int
}

__global (
	g_val   int
	g_count int
	g_flag  bool
	g_point Point
)

// ===================== METHODS =====================

fn (p Point) sum() int {
	return p.x + p.y
}

fn (p Point) product() int {
	return p.x * p.y
}

fn (p Point) scaled(factor int) Point {
	return Point{
		x: p.x * factor
		y: p.y * factor
	}
}

fn (mut p Point) double() {
	p.x = p.x * 2
	p.y = p.y * 2
}

fn (r Rectangle) area() int {
	return r.width * r.height
}

fn (r Rectangle) perimeter() int {
	return 2 * (r.width + r.height)
}

fn (n Node) total() int {
	return n.value + n.left + n.right
}

// ===================== HELPER FUNCTIONS =====================

fn fib(n int) int {
	if n < 2 {
		return n
	}
	return fib(n - 1) + fib(n - 2)
}

fn factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n - 1)
}

fn sum_recursive(n int) int {
	if n <= 0 {
		return 0
	}
	return n + sum_recursive(n - 1)
}

fn gcd(a int, b int) int {
	if b == 0 {
		return a
	}
	return gcd(b, a % b)
}

fn power(base int, exp int) int {
	if exp == 0 {
		return 1
	}
	return base * power(base, exp - 1)
}

fn sum_many(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a + b + c + d + e + f + g + h
}

fn mul_many(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a * b * c * d * e * f * g * h
}

fn max_of_eight(a int, b int, c int, d int, e int, f int, g int, h int) int {
	mut m := a
	if b > m {
		m = b
	}
	if c > m {
		m = c
	}
	if d > m {
		m = d
	}
	if e > m {
		m = e
	}
	if f > m {
		m = f
	}
	if g > m {
		m = g
	}
	if h > m {
		m = h
	}
	return m
}

fn weighted_sum(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a * 1 + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8
}

fn modify_struct(mut p Point) {
	p.x = 999
	p.y = 888
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

fn translate_point(mut p Point, dx int, dy int) {
	p.x = p.x + dx
	p.y = p.y + dy
}

fn reset_point(mut p Point) {
	p.x = 0
	p.y = 0
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

fn print_rec(n int) {
	if n == 0 {
		return
	}
	print_rec(n / 10)
	rem := n - (n / 10) * 10
	C.putchar(rem + 48)
}

fn print_int(n int) {
	if n == 0 {
		C.putchar(48)
		C.putchar(10)
		return
	}
	mut v := n
	if n < 0 {
		C.putchar(45)
		v = 0 - n
	}
	print_rec(v)
	C.putchar(10)
}

fn print_str(s string) {
	C.puts(s.str)
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

// ===================== IF-GUARD HELPERS =====================

// Returns the value if positive, none otherwise
fn maybe_positive(x int) ?int {
	if x > 0 {
		return x
	}
	return none
}

// Returns the doubled value if in range, none otherwise
fn maybe_double(x int) ?int {
	if x >= 0 && x <= 50 {
		return x * 2
	}
	return none
}

// Returns sum if both positive, none otherwise
fn maybe_sum(a int, b int) ?int {
	if a > 0 && b > 0 {
		return a + b
	}
	return none
}

// ===================== IF-EXPRESSION HELPERS =====================

fn int_abs(a int) int {
	return if a < 0 { -a } else { a }
}

fn int_max2(a int, b int) int {
	return if a > b { a } else { b }
}

fn int_min2(a int, b int) int {
	return if a < b { a } else { b }
}

fn sign(x int) int {
	return if x < 0 {
		-1
	} else {
		if x > 0 {
			1
		} else {
			0
		}
	}
}

fn clamp(x int, lo int, hi int) int {
	return if x < lo {
		lo
	} else {
		if x > hi {
			hi
		} else {
			x
		}
	}
}

// ===================== MAIN TEST FUNCTION =====================

fn main() {
	print_str('=== SSA Backend Test Suite ===')

	// ==================== 1. STRUCT DECL & INIT (5 tests) ====================
	print_str('--- 1. Struct Declaration & Initialization ---')

	// 1.1 Basic struct init
	p1 := Point{
		x: 10
		y: 20
	}
	print_int(p1.x) // 10
	print_int(p1.y) // 20

	// 1.2 Default zero init
	p2 := Point{}
	print_int(p2.x) // 0
	print_int(p2.y) // 0

	// 1.3 Mutable struct modification
	mut p3 := Point{
		x: 1
		y: 2
	}
	p3.x = 100
	p3.y = 200
	print_int(p3.x) // 100
	print_int(p3.y) // 200

	// 1.4 Struct with computed values
	base := 7
	p4 := Point{
		x: base * 2
		y: base * 3
	}
	print_int(p4.x) // 14
	print_int(p4.y) // 21

	// 1.5 Multiple struct instances
	p5a := Point{
		x: 1
		y: 2
	}
	p5b := Point{
		x: 3
		y: 4
	}
	print_int(p5a.x + p5b.x) // 4
	print_int(p5a.y + p5b.y) // 6

	// ==================== 2. CALLS & SELECTOR ASSIGN (5 tests) ====================
	print_str('--- 2. Calls & Selector Assignment ---')

	// 2.1 Basic function call with selector assign
	mut pt := Point{
		x: 10
		y: 20
	}
	pt.x = add(pt.x, 5)
	print_int(pt.x) // 15

	// 2.2 Chained calls
	pt.y = add(add(pt.y, 10), 5)
	print_int(pt.y) // 35

	// 2.3 Call result to selector with subtraction
	pt.x = sub(pt.x, 3)
	print_int(pt.x) // 12

	// 2.4 Multiple selectors updated via calls
	mut pt2 := Point{
		x: 5
		y: 5
	}
	pt2.x = mul(pt2.x, 3)
	pt2.y = mul(pt2.y, 4)
	print_int(pt2.x) // 15
	print_int(pt2.y) // 20

	// 2.5 Nested function calls with selectors
	mut pt3 := Point{
		x: 10
		y: 20
	}
	pt3.x = add(mul(pt3.x, 2), 5) // 10*2 + 5 = 25
	pt3.y = sub(mul(pt3.y, 3), 10) // 20*3 - 10 = 50
	print_int(pt3.x) // 25
	print_int(pt3.y) // 50

	// ==================== 3. GLOBALS & COMPOUND ASSIGN (5 tests) ====================
	print_str('--- 3. Globals & Compound Assignment ---')

	// 3.1 Basic global assignment and compound add
	g_val = 50
	g_val += 50
	print_int(g_val) // 100

	// 3.2 Compound subtract
	g_val = 100
	g_val -= 30
	print_int(g_val) // 70

	// 3.3 Compound multiply
	g_val = 5
	g_val *= 6
	print_int(g_val) // 30

	// 3.4 Compound divide
	g_val = 100
	g_val /= 4
	print_int(g_val) // 25

	// 3.5 Global struct
	g_point.x = 42
	g_point.y = 84
	g_point.x += 8
	print_int(g_point.x) // 50
	print_int(g_point.y) // 84

	// ==================== 4. BOOL & LOGIC (5 tests) ====================
	print_str('--- 4. Bool & Logic ---')

	// 4.1 Basic bool true
	flag1 := true
	if flag1 {
		print_int(1)
	} else {
		print_int(0)
	}

	// 4.2 Basic bool false
	flag2 := false
	if flag2 {
		print_int(1)
	} else {
		print_int(0)
	}

	// 4.3 Bool from comparison
	val := 10
	flag3 := val > 5
	if flag3 {
		print_int(1)
	} else {
		print_int(0)
	}

	// 4.4 Logical AND
	a_bool := true
	b_bool := true
	if a_bool && b_bool {
		print_int(1)
	} else {
		print_int(0)
	}

	// 4.5 Logical OR and NOT
	c_bool := false
	d_bool := true
	if c_bool || d_bool {
		print_int(1) // 1
	} else {
		print_int(0)
	}
	if !c_bool {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 5. LOOP WITH BREAK/CONTINUE (5 tests) ====================
	print_str('--- 5. Loop with Break/Continue ---')

	// 5.1 Basic continue (skip 5)
	mut sum1 := 0
	mut i1 := 0
	for i1 < 10 {
		i1++
		if i1 == 5 {
			continue
		}
		if i1 > 7 {
			break
		}
		sum1 += i1
	}
	print_int(sum1) // 1+2+3+4+6+7 = 23

	// 5.2 Multiple continues (skip even)
	mut sum2 := 0
	mut i2 := 0
	for i2 < 10 {
		i2++
		if i2 % 2 == 0 {
			continue
		}
		sum2 += i2
	}
	print_int(sum2) // 1+3+5+7+9 = 25

	// 5.3 Early break
	mut sum3 := 0
	mut i3 := 0
	for i3 < 100 {
		i3++
		if i3 > 5 {
			break
		}
		sum3 += i3
	}
	print_int(sum3) // 1+2+3+4+5 = 15

	// 5.4 Combined break and continue
	mut sum4 := 0
	mut i4 := 0
	for i4 < 20 {
		i4++
		if i4 % 3 == 0 {
			continue
		}
		if i4 > 10 {
			break
		}
		sum4 += i4
	}
	print_int(sum4) // 1+2+4+5+7+8+10 = 37

	// 5.5 Simple condition loop
	mut sum5 := 0
	mut i5 := 0
	for i5 < 5 {
		sum5 += i5
		i5++
	}
	print_int(sum5) // 0+1+2+3+4 = 10

	// ==================== 6. MATCH (5 tests) ====================
	print_str('--- 6. Match ---')

	// 6.1 Match with else
	x1 := 10
	match x1 {
		1 { print_int(1) }
		2 { print_int(2) }
		else { print_int(777) }
	}

	// 6.2 Match exact case
	x2 := 2
	match x2 {
		1 { print_int(100) }
		2 { print_int(200) }
		3 { print_int(300) }
		else { print_int(0) }
	}

	// 6.3 Match first case
	x3 := 1
	match x3 {
		1 { print_int(111) }
		2 { print_int(222) }
		else { print_int(999) }
	}

	// 6.4 Match with computation
	x4 := 5
	match x4 {
		1 { print_int(x4 * 10) }
		5 { print_int(x4 * 100) }
		else { print_int(0) }
	}

	// 6.5 Match with more cases
	x5 := 4
	match x5 {
		1 { print_int(10) }
		2 { print_int(20) }
		3 { print_int(30) }
		4 { print_int(40) }
		5 { print_int(50) }
		else { print_int(0) }
	}

	// ==================== 7. C-STYLE LOOP & FACTORIAL (5 tests) ====================
	print_str('--- 7. C-style Loop ---')

	// 7.1 Basic factorial
	mut fact1 := 1
	for k := 1; k <= 5; k++ {
		fact1 = fact1 * k
	}
	print_int(fact1) // 120

	// 7.2 Sum 1 to 10
	mut sum7 := 0
	for k := 1; k <= 10; k++ {
		sum7 += k
	}
	print_int(sum7) // 55

	// 7.3 Powers of 2
	mut pow2 := 1
	for k := 0; k < 8; k++ {
		pow2 = pow2 * 2
	}
	print_int(pow2) // 256

	// 7.4 Countdown
	mut countdown := 0
	for k := 10; k > 0; k-- {
		countdown += k
	}
	print_int(countdown) // 55

	// 7.5 Step by 2
	mut sum_even := 0
	for k := 0; k <= 10; k += 2 {
		sum_even += k
	}
	print_int(sum_even) // 0+2+4+6+8+10 = 30

	// ==================== 8. RECURSIVE FUNCTIONS (5 tests) ====================
	print_str('--- 8. Recursive Functions ---')

	// 8.1 Fibonacci
	print_int(fib(10)) // 55

	// 8.2 Factorial recursive
	print_int(factorial(6)) // 720

	// 8.3 Sum recursive
	print_int(sum_recursive(10)) // 55

	// 8.4 GCD
	print_int(gcd(48, 18)) // 6

	// 8.5 Power
	print_int(power(2, 10)) // 1024

	// ==================== 9. NESTED LOOPS (5 tests) ====================
	print_str('--- 9. Nested Loops ---')

	// 9.1 Basic 3x3
	mut count1 := 0
	mut r1 := 0
	for r1 < 3 {
		mut c1 := 0
		for c1 < 3 {
			count1++
			c1++
		}
		r1++
	}
	print_int(count1) // 9

	// 9.2 4x5 grid
	mut count2 := 0
	mut r2 := 0
	for r2 < 4 {
		mut c2 := 0
		for c2 < 5 {
			count2++
			c2++
		}
		r2++
	}
	print_int(count2) // 20

	// 9.3 Sum of products
	mut sum9 := 0
	mut r3 := 1
	for r3 <= 3 {
		mut c3 := 1
		for c3 <= 3 {
			sum9 += r3 * c3
			c3++
		}
		r3++
	}
	print_int(sum9) // (1+2+3) + (2+4+6) + (3+6+9) = 36

	// 9.4 2x3 with accumulator
	mut count4 := 0
	mut r4 := 0
	for r4 < 2 {
		mut c4 := 0
		for c4 < 3 {
			count4 += 1
			c4++
		}
		r4++
	}
	print_int(count4) // 6

	// 9.5 Inner break
	mut count5 := 0
	mut r5 := 0
	for r5 < 5 {
		mut c5 := 0
		for c5 < 10 {
			if c5 >= 3 {
				break
			}
			count5++
			c5++
		}
		r5++
	}
	print_int(count5) // 5*3 = 15

	// ==================== 10. INFINITE LOOP (5 tests) ====================
	print_str('--- 10. Infinite Loop ---')

	// 10.1 Basic infinite with break
	mut iter1 := 0
	for {
		iter1++
		if iter1 == 5 {
			break
		}
	}
	print_int(iter1) // 5

	// 10.2 Sum until threshold
	mut sum10 := 0
	mut n10 := 0
	for {
		n10++
		sum10 += n10
		if sum10 > 20 {
			break
		}
	}
	print_int(sum10) // 21 (1+2+3+4+5+6 = 21)

	// 10.3 Find first power of 2 > 100
	mut pow := 1
	for {
		pow = pow * 2
		if pow > 100 {
			break
		}
	}
	print_int(pow) // 128

	// 10.4 Countdown in infinite loop
	mut cd := 10
	for {
		cd--
		if cd == 0 {
			break
		}
	}
	print_int(cd) // 0

	// 10.5 Simple counter
	mut x10 := 0
	for {
		x10++
		if x10 >= 10 {
			break
		}
	}
	print_int(x10) // 10

	// ==================== 11. MANY ARGUMENTS (5 tests) ====================
	print_str('--- 11. Many Arguments ---')

	// 11.1 Sum of 8 ones
	print_int(sum_many(1, 1, 1, 1, 1, 1, 1, 1)) // 8

	// 11.2 Sum of sequence
	print_int(sum_many(1, 2, 3, 4, 5, 6, 7, 8)) // 36

	// 11.3 Product of small numbers
	print_int(mul_many(1, 2, 1, 2, 1, 2, 1, 2)) // 16

	// 11.4 Max of 8
	print_int(max_of_eight(3, 7, 2, 9, 1, 8, 4, 6)) // 9

	// 11.5 Weighted sum
	print_int(weighted_sum(1, 1, 1, 1, 1, 1, 1, 1)) // 1+2+3+4+5+6+7+8 = 36

	// ==================== 12. MODIFYING STRUCT (5 tests) ====================
	print_str('--- 12. Modifying Struct via Function ---')

	// 12.1 Basic modify
	mut pm1 := Point{
		x: 10
		y: 20
	}
	modify_struct(mut pm1)
	print_int(pm1.x) // 999
	print_int(pm1.y) // 888

	// 12.2 Swap
	mut pm2 := Point{
		x: 5
		y: 15
	}
	swap_point(mut pm2)
	print_int(pm2.x) // 15
	print_int(pm2.y) // 5

	// 12.3 Scale
	mut pm3 := Point{
		x: 10
		y: 20
	}
	scale_point(mut pm3, 3)
	print_int(pm3.x) // 30
	print_int(pm3.y) // 60

	// 12.4 Translate
	mut pm4 := Point{
		x: 5
		y: 10
	}
	translate_point(mut pm4, 100, 200)
	print_int(pm4.x) // 105
	print_int(pm4.y) // 210

	// 12.5 Reset
	mut pm5 := Point{
		x: 999
		y: 888
	}
	reset_point(mut pm5)
	print_int(pm5.x) // 0
	print_int(pm5.y) // 0

	// ==================== 13. ASSERT (5 tests) ====================
	print_str('--- 13. Assert ---')

	// 13.1 Basic equality
	assert 1 == 1
	print_str('Assert 1 passed')

	// 13.2 Computed equality
	assert 2 + 2 == 4
	print_str('Assert 2 passed')

	// 13.3 Boolean assert
	assert true
	print_str('Assert 3 passed')

	// 13.4 Comparison assert
	assert 10 > 5
	print_str('Assert 4 passed')

	// 13.5 Complex expression
	assert (3 * 4) == (2 * 6)
	print_str('Assert 5 passed')

	// ==================== 14. HEAP ALLOCATION (5 tests) ====================
	print_str('--- 14. Heap Allocation ---')

	// 14.1 Basic heap Point
	hp1 := &Point{
		x: 10
		y: 20
	}
	print_int(hp1.x) // 10
	print_int(hp1.y) // 20

	// 14.2 Heap with zero
	hp2 := &Point{
		x: 0
		y: 0
	}
	print_int(hp2.x) // 0
	print_int(hp2.y) // 0

	// 14.3 Heap with computed values
	hp3 := &Point{
		x: 5 * 5
		y: 6 * 6
	}
	print_int(hp3.x) // 25
	print_int(hp3.y) // 36

	// 14.4 Heap Rectangle (no nested access)
	hr := &Rectangle{
		width:  100
		height: 200
		origin: Point{
			x: 10
			y: 20
		}
	}
	print_int(hr.width) // 100
	print_int(hr.height) // 200

	// 14.5 Heap Node
	hn := &Node{
		value: 42
		left:  1
		right: 2
	}
	print_int(hn.value) // 42
	print_int(hn.left) // 1
	print_int(hn.right) // 2

	// ==================== 15. BITWISE OPERATIONS (5 tests) ====================
	print_str('--- 15. Bitwise Operations ---')

	// 15.1 Basic AND
	print_int(0b1100 & 0b1010) // 8

	// 15.2 Basic OR
	print_int(0b1100 | 0b1010) // 14

	// 15.3 Basic XOR
	print_int(0b1100 ^ 0b1010) // 6

	// 15.4 Mask extraction
	num := 0xABCD
	low_byte := num & 0xFF
	print_int(low_byte) // 0xCD = 205

	// 15.5 Bit set/clear
	mut flags := 0
	flags = flags | 0b0001 // set bit 0
	flags = flags | 0b0100 // set bit 2
	print_int(flags) // 5
	flags = flags & 0b1110 // clear bit 0
	print_int(flags) // 4

	// ==================== 16. SHIFT OPERATIONS (5 tests) ====================
	print_str('--- 16. Shift Operations ---')

	// 16.1 Left shift basic
	print_int(1 << 4) // 16

	// 16.2 Right shift basic
	print_int(32 >> 2) // 8

	// 16.3 Multiple shifts
	print_int(255 >> 4) // 15

	// 16.4 Shift for multiply
	val16 := 7
	print_int(val16 << 3) // 7 * 8 = 56

	// 16.5 Shift for divide
	val17 := 96
	print_int(val17 >> 4) // 96 / 16 = 6

	// ==================== 17. MODULO (5 tests) ====================
	print_str('--- 17. Modulo ---')

	// 17.1 Basic modulo
	print_int(17 % 5) // 2

	// 17.2 Modulo with larger divisor
	print_int(100 % 7) // 2

	// 17.3 Even/odd check
	print_int(15 % 2) // 1 (odd)
	print_int(16 % 2) // 0 (even)

	// 17.4 Clock arithmetic
	hour := 23
	new_hour := (hour + 5) % 24
	print_int(new_hour) // 4

	// 17.5 Digit extraction
	num17 := 12345
	last_digit := num17 % 10
	print_int(last_digit) // 5
	second_digit := (num17 / 10) % 10
	print_int(second_digit) // 4

	// ==================== 18. POINTER ARITHMETIC (5 tests) ====================
	print_str('--- 18. Pointer Arithmetic ---')

	// 18.1 Heap struct access
	hp_arr1 := &Point{
		x: 10
		y: 20
	}
	print_int(hp_arr1.x) // 10
	print_int(hp_arr1.y) // 20

	// 18.2 Multiple heap structs
	hp_arr2 := &Point{
		x: 100
		y: 200
	}
	hp_arr3 := &Point{
		x: 300
		y: 400
	}
	print_int(hp_arr2.x + hp_arr3.x) // 400
	print_int(hp_arr2.y + hp_arr3.y) // 600

	// 18.3 Heap struct with computed values
	base18 := 5
	hp_arr4 := &Point{
		x: base18 * 10
		y: base18 * 20
	}
	print_int(hp_arr4.x) // 50
	print_int(hp_arr4.y) // 100

	// 18.4 Multiple heap allocations in loop
	mut sum18 := 0
	mut i18 := 0
	for i18 < 3 {
		hp := &Point{
			x: i18 * 10
			y: i18 * 20
		}
		sum18 = sum18 + hp.x + hp.y
		i18++
	}
	print_int(sum18) // 0+0 + 10+20 + 20+40 = 90

	// 18.5 Heap node tree structure
	node1 := &Node{
		value: 100
		left:  0
		right: 0
	}
	node2 := &Node{
		value: 200
		left:  0
		right: 0
	}
	print_int(node1.value + node2.value) // 300

	// ==================== 19. NESTED STRUCT ACCESS (5 tests) ====================
	print_str('--- 19. Nested Struct Access ---')

	// 19.1 Basic nested access
	rect := Rectangle{
		width:  100
		height: 200
		origin: Point{
			x: 10
			y: 20
		}
	}
	print_int(rect.width) // 100
	print_int(rect.height) // 200

	// 19.2 Nested struct field via intermediate
	rect2 := Rectangle{
		width:  50
		height: 60
		origin: Point{
			x: 5
			y: 6
		}
	}
	print_int(rect2.width + rect2.height) // 110

	// 19.3 Mutable nested struct modification
	mut rect3 := Rectangle{
		width:  10
		height: 20
		origin: Point{
			x: 1
			y: 2
		}
	}
	rect3.width = 100
	rect3.height = 200
	print_int(rect3.width) // 100
	print_int(rect3.height) // 200

	// 19.4 Multiple rectangles
	rect4a := Rectangle{
		width:  10
		height: 20
		origin: Point{
			x: 0
			y: 0
		}
	}
	rect4b := Rectangle{
		width:  30
		height: 40
		origin: Point{
			x: 0
			y: 0
		}
	}
	print_int(rect4a.width + rect4b.width) // 40
	print_int(rect4a.height + rect4b.height) // 60

	// 19.5 Rectangle area
	rect5 := Rectangle{
		width:  12
		height: 10
		origin: Point{
			x: 0
			y: 0
		}
	}
	area := rect5.width * rect5.height
	print_int(area) // 120

	// ==================== 20. NEGATIVE NUMBERS (5 tests) ====================
	print_str('--- 20. Negative Numbers ---')

	// 20.1 Unary minus
	n1 := 0 - 42
	print_int(n1) // -42

	// 20.2 Negative addition
	n2 := 0 - 10
	n3 := n2 + 5
	print_int(n3) // -5

	// 20.3 Negative subtraction
	n4 := 0 - 20
	n5 := n4 - 10
	print_int(n5) // -30

	// 20.4 Negative multiplication
	n6 := 0 - 7
	n7 := n6 * 3
	print_int(n7) // -21

	// 20.5 Double negative (positive)
	n8 := 0 - 50
	n9 := 0 - n8
	print_int(n9) // 50

	// ==================== 21. UNARY OPERATIONS (5 tests) ====================
	print_str('--- 21. Unary Operations ---')

	// 21.1 Logical not on false
	u1 := !false
	if u1 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 21.2 Logical not on true
	u2 := !true
	if u2 {
		print_int(1)
	} else {
		print_int(0) // 0
	}

	// 21.3 Double negation
	u3 := !!true
	if u3 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 21.4 Not with comparison
	u4 := !(5 > 10)
	if u4 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 21.5 Not with variable
	u5 := false
	u6 := !u5
	if u6 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 22. COMPLEX BOOLEAN (5 tests) ====================
	print_str('--- 22. Complex Boolean ---')

	// 22.1 Multiple ANDs
	if true && true && true {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 22.2 Multiple ORs
	if false || false || true {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 22.3 AND with OR
	if (true && false) || (true && true) {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 22.4 Complex condition with variables
	aa := 10
	bb := 20
	cc := 30
	if aa < bb && bb < cc {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 22.5 Chained comparisons
	if aa < 15 && bb > 15 && cc == 30 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 23. COMPARISON AS EXPRESSION (5 tests) ====================
	print_str('--- 23. Comparison as Expression ---')

	// 23.1 Comparison result in variable
	cmp1 := 10 > 5
	if cmp1 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 23.2 Multiple comparison results
	cmp2 := 3 < 5
	cmp3 := 7 > 2
	if cmp2 && cmp3 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 23.3 Equality comparison
	cmp4 := 42 == 42
	if cmp4 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 23.4 Inequality comparison
	cmp5 := 10 != 20
	if cmp5 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 23.5 Comparison with expressions
	cmp6 := (5 + 5) == (2 * 5)
	if cmp6 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 24. DEEPLY NESTED IF (5 tests) ====================
	print_str('--- 24. Deeply Nested If ---')

	// 24.1 Three levels deep
	dn1 := 5
	if dn1 > 0 {
		if dn1 > 3 {
			if dn1 > 4 {
				print_int(1) // 1
			} else {
				print_int(0)
			}
		} else {
			print_int(0)
		}
	} else {
		print_int(0)
	}

	// 24.2 Nested with else chains
	dn2 := 2
	if dn2 == 1 {
		print_int(10)
	} else {
		if dn2 == 2 {
			print_int(20) // 20
		} else {
			if dn2 == 3 {
				print_int(30)
			} else {
				print_int(0)
			}
		}
	}

	// 24.3 Mixed nesting with match
	dn3 := 3
	if dn3 > 0 {
		match dn3 {
			1 { print_int(100) }
			2 { print_int(200) }
			3 { print_int(300) } // 300
			else { print_int(0) }
		}
	} else {
		print_int(0)
	}

	// 24.4 Nested loops with conditionals
	mut dn4_sum := 0
	mut dn4_i := 0
	for dn4_i < 3 {
		mut dn4_j := 0
		for dn4_j < 3 {
			if dn4_i == dn4_j {
				dn4_sum += 1
			}
			dn4_j++
		}
		dn4_i++
	}
	print_int(dn4_sum) // 3 (diagonal: 0-0, 1-1, 2-2)

	// 24.5 If inside loop with break
	mut dn5_result := 0
	mut dn5_k := 0
	for dn5_k < 100 {
		if dn5_k > 5 {
			if dn5_k > 7 {
				dn5_result = dn5_k
				break
			}
		}
		dn5_k++
	}
	print_int(dn5_result) // 8

	// ==================== 25. LARGE CONSTANTS (5 tests) ====================
	print_str('--- 25. Large Constants ---')

	// 25.1 Value > 65535
	big1 := 100000
	print_int(big1) // 100000

	// 25.2 Large multiplication result
	big2 := 1000 * 1000
	print_int(big2) // 1000000

	// 25.3 Large addition
	big3 := 50000 + 50000
	print_int(big3) // 100000

	// 25.4 Large subtraction
	big4 := 200000 - 100000
	print_int(big4) // 100000

	// 25.5 Large division
	big5 := 1000000 / 100
	print_int(big5) // 10000

	// ==================== 26. MIXED OPERATIONS (5 tests) ====================
	print_str('--- 26. Mixed Operations ---')

	// 26.1 Arithmetic then comparison
	mix1 := (10 + 5) * 2
	if mix1 == 30 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 26.2 Comparison then logic
	mix2a := 10 > 5
	mix2b := 20 < 30
	if mix2a && mix2b {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 26.3 Chained arithmetic
	mix3 := 2 + 3 * 4 - 6 / 2
	print_int(mix3) // 2 + 12 - 3 = 11

	// 26.4 Bitwise with arithmetic
	mix4 := (5 | 3) + (4 & 6)
	print_int(mix4) // 7 + 4 = 11

	// 26.5 Complex expression
	mix5 := ((10 + 5) * 2 - 10) / 5
	print_int(mix5) // (30 - 10) / 5 = 4

	// ==================== 27. EDGE CASES (5 tests) ====================
	print_str('--- 27. Edge Cases ---')

	// 27.1 Zero operations
	edge1 := 0 + 0
	print_int(edge1) // 0
	edge2 := 100 * 0
	print_int(edge2) // 0
	edge3 := 0 / 7
	print_int(edge3) // 0

	// 27.2 Identity operations
	edge4 := 42 + 0
	print_int(edge4) // 42
	edge5 := 42 * 1
	print_int(edge5) // 42
	edge6 := 42 / 1
	print_int(edge6) // 42

	// 27.3 Division edge cases
	edge7 := 7 / 7
	print_int(edge7) // 1
	edge8 := 100 / 10
	print_int(edge8) // 10

	// 27.4 Modulo edge cases
	edge9 := 10 % 10
	print_int(edge9) // 0
	edge10 := 5 % 7
	print_int(edge10) // 5

	// 27.5 Comparison edge cases
	if 0 == 0 {
		print_int(1) // 1
	} else {
		print_int(0)
	}
	if 0 < 1 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 28. COMPLEX RECURSION (5 tests) ====================
	print_str('--- 28. Complex Recursion ---')

	// 28.1 Deep recursion test
	print_int(sum_recursive(100)) // 5050

	// 28.2 Mutual dependency via gcd
	print_int(gcd(252, 105)) // 21

	// 28.3 Multiple recursive calls
	print_int(fib(15)) // 610

	// 28.4 Power with larger exponent
	print_int(power(3, 5)) // 243

	// 28.5 Factorial with larger input
	print_int(factorial(7)) // 5040

	// ==================== 29. STRUCT OPERATIONS (5 tests) ====================
	print_str('--- 29. Struct Operations ---')

	// 29.1 Struct as function result (via modify)
	mut sp1 := Point{
		x: 0
		y: 0
	}
	scale_point(mut sp1, 10)
	print_int(sp1.x) // 0
	print_int(sp1.y) // 0

	// 29.2 Chained struct modifications
	mut sp2 := Point{
		x: 1
		y: 1
	}
	scale_point(mut sp2, 5)
	translate_point(mut sp2, 10, 20)
	print_int(sp2.x) // 15
	print_int(sp2.y) // 25

	// 29.3 Struct field arithmetic
	sp3 := Point{
		x: 100
		y: 200
	}
	sp3_sum := sp3.x + sp3.y
	sp3_diff := sp3.y - sp3.x
	print_int(sp3_sum) // 300
	print_int(sp3_diff) // 100

	// 29.4 Multiple struct parameters
	mut sp4 := Point{
		x: 10
		y: 20
	}
	swap_point(mut sp4)
	scale_point(mut sp4, 2)
	print_int(sp4.x) // 40
	print_int(sp4.y) // 20

	// 29.5 Struct in conditional
	sp5 := Point{
		x: 5
		y: 10
	}
	if sp5.x < sp5.y {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// ==================== 30. CONTROL FLOW EDGE CASES (5 tests) ====================
	print_str('--- 30. Control Flow Edge Cases ---')

	// 30.1 Empty else
	mut cf1 := 0
	if true {
		cf1 = 1
	}
	print_int(cf1) // 1

	// 30.2 Multiple sequential ifs
	mut cf2 := 0
	if true {
		cf2 += 1
	}
	if true {
		cf2 += 2
	}
	if true {
		cf2 += 4
	}
	print_int(cf2) // 7

	// 30.3 Nested match
	cf3 := 2
	match cf3 {
		1 {
			match cf3 {
				1 { print_int(11) }
				else { print_int(10) }
			}
		}
		2 {
			match cf3 {
				2 { print_int(22) } // 22
				else { print_int(20) }
			}
		}
		else {
			print_int(0)
		}
	}

	// 30.4 Loop with compound condition (&&)
	mut cf4 := 0
	mut cf4_i := 0
	for cf4_i < 20 && cf4 < 50 {
		cf4 += cf4_i
		cf4_i++
	}
	print_int(cf4) // 55 (0+1+2+3+4+5+6+7+8+9+10 = 55, stops when >= 50)

	// 30.5 Return inside nested control flow
	print_int(nested_return(5)) // 100
	print_int(nested_return(15)) // 200
	print_int(nested_return(25)) // 300

	// 30.6 Loop with || condition
	mut cf5 := 0
	mut cf5_i := 0
	for cf5_i < 3 || cf5 < 10 {
		cf5 += 5
		cf5_i++
	}
	print_int(cf5) // 15 (loop runs 3 times: 5, 10, 15 - stops when both conditions false)

	// ==================== 31. METHODS ====================
	print_str('--- 31. Methods ---')

	// 31.1 Basic method call
	m1 := Point{
		x: 10
		y: 20
	}
	print_int(m1.sum()) // 30

	// 31.2 Method with multiplication
	m2 := Point{
		x: 5
		y: 6
	}
	print_int(m2.product()) // 30

	// 31.3 Mutable receiver method
	mut m4 := Point{
		x: 7
		y: 8
	}
	m4.double()
	print_int(m4.x) // 14
	print_int(m4.y) // 16

	// 31.4 Rectangle methods
	m5 := Rectangle{
		width:  10
		height: 5
		origin: Point{
			x: 0
			y: 0
		}
	}
	print_int(m5.area()) // 50
	print_int(m5.perimeter()) // 30

	// 31.5 Node method
	m6 := Node{
		value: 100
		left:  10
		right: 20
	}
	print_int(m6.total()) // 130

	// 31.6 Method on heap-allocated struct
	m8 := &Point{
		x: 4
		y: 5
	}
	print_int(m8.sum()) // 9
	print_int(m8.product()) // 20

	// ==================== 32. IF-EXPRESSIONS ====================
	print_str('--- 32. If-Expressions ---')

	// 32.1 Basic if-expression (abs)
	print_int(int_abs(-5)) // 5
	print_int(int_abs(7)) // 7
	print_int(int_abs(0)) // 0

	// 32.2 If-expression for max
	print_int(int_max2(10, 20)) // 20
	print_int(int_max2(30, 15)) // 30
	print_int(int_max2(5, 5)) // 5

	// 32.3 If-expression for min
	print_int(int_min2(10, 20)) // 10
	print_int(int_min2(30, 15)) // 15
	print_int(int_min2(8, 8)) // 8

	// 32.4 Nested if-expression (sign)
	print_int(sign(-100)) // -1
	print_int(sign(100)) // 1
	print_int(sign(0)) // 0

	// 32.5 Double nested if-expression (clamp)
	print_int(clamp(5, 0, 10)) // 5 (in range)
	print_int(clamp(-5, 0, 10)) // 0 (below lo)
	print_int(clamp(15, 0, 10)) // 10 (above hi)

	// 32.6 If-expression in local variable
	val2 := 25
	result := if val2 > 20 { val2 * 2 } else { val2 }
	print_int(result) // 50

	// 32.7 If-expression with complex condition
	a := 10
	b := 20
	c := if a < b && b < 30 { a + b } else { 0 }
	print_int(c) // 30

	// ==================== 33. ARRAY INITIALIZATION ====================
	print_str('--- 33. Array Initialization ---')

	// 33.1 Basic array literal
	arr1 := [10, 20, 30]
	print_int(arr1[0]) // 10
	print_int(arr1[1]) // 20
	print_int(arr1[2]) // 30

	// 33.2 Array element sum
	arr2 := [5, 10, 15]
	arr2_sum := arr2[0] + arr2[1] + arr2[2]
	print_int(arr2_sum) // 30

	// 33.3 Array with computed values
	base_val := 7
	arr3 := [base_val, base_val * 2, base_val * 3]
	print_int(arr3[0]) // 7
	print_int(arr3[1]) // 14
	print_int(arr3[2]) // 21

	// 33.4 Array element in expression
	arr4 := [100, 200, 300]
	result4 := arr4[0] * 2 + arr4[1]
	print_int(result4) // 400

	// 33.5 Array with function call on elements
	arr5 := [3, 4, 5]
	print_int(add(arr5[0], arr5[1])) // 7
	print_int(mul(arr5[1], arr5[2])) // 20

	// ==================== 34. STRING INTERPOLATION ====================
	print_str('--- 34. String Interpolation ---')

	// 34.1 Basic integer interpolation
	interp_x := 42
	s1 := 'The answer is ${interp_x}'
	print_str(s1) // The answer is 42

	// 34.2 Multiple interpolations
	interp_a := 10
	interp_b := 20
	s2 := '${interp_a} + ${interp_b} = ${interp_a + interp_b}'
	print_str(s2) // 10 + 20 = 30

	// 34.3 String at beginning and end
	interp_val := 100
	s3 := 'Value: ${interp_val}!'
	print_str(s3) // Value: 100!

	// 34.4 Just interpolation (no literal parts)
	interp_num := 999
	s4 := '${interp_num}'
	print_str(s4) // 999

	// 34.5 Multiple consecutive values
	interp_v1 := 1
	interp_v2 := 2
	interp_v3 := 3
	s5 := '${interp_v1}-${interp_v2}-${interp_v3}'
	print_str(s5) // 1-2-3

	// ==================== 35. IF-GUARD EXPRESSIONS ====================
	print_str('--- 35. If-Guard Expressions ---')

	// 35.1 Basic if-guard with success (positive value)
	if g1_val := maybe_positive(42) {
		print_int(g1_val) // 42
	} else {
		print_int(0)
	}

	// 35.2 If-guard with failure (non-positive value)
	if g2_val := maybe_positive(-5) {
		print_int(g2_val)
	} else {
		print_int(999) // 999 (else branch taken)
	}

	// 35.3 If-guard with computation in then branch
	if g3_val := maybe_double(25) {
		print_int(g3_val + 10) // 50 + 10 = 60
	} else {
		print_int(0)
	}

	// 35.4 If-guard with function call
	if g4_result := maybe_sum(10, 20) {
		print_int(g4_result) // 30
	} else {
		print_int(0)
	}

	// 35.5 Nested if with if-guard
	g5_outer := 5
	if g5_outer > 0 {
		if g5_inner := maybe_positive(g5_outer * 10) {
			print_int(g5_inner) // 50
		} else {
			print_int(0)
		}
	}

	// 35.6 If-guard in sequence
	mut g6_sum := 0
	if g6_a := maybe_positive(10) {
		g6_sum += g6_a
	}
	if g6_b := maybe_positive(20) {
		g6_sum += g6_b
	}
	if g6_c := maybe_positive(-5) {
		g6_sum += g6_c
	}
	print_int(g6_sum) // 10 + 20 + 0 = 30

	// 35.7 If-guard with else-if chain
	g7_test := 100
	if g7_val := maybe_positive(-g7_test) {
		print_int(g7_val)
	} else {
		if g7_val2 := maybe_positive(g7_test) {
			print_int(g7_val2) // 100
		} else {
			print_int(0)
		}
	}

	print_str('=== All tests completed ===')
}
