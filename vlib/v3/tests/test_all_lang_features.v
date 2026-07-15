module main

import math
import os
import sync
import strings
import v3.bench
import v.token

// Cat represents cat data used by v3 tests.
struct Cat {
	name string
	age  int
}

// Dog represents dog data used by v3 tests.
struct Dog {
	name   string
	tricks int
}

// Animal aliases animal values used by v3 tests.
type Animal = Cat | Dog

// UserId104 aliases user id104 values used by v3 tests.
type UserId104 = int

// Holder represents holder data used by v3 tests.
struct Holder {
mut:
	pet Animal
}

// describe_holder supports describe holder handling for v3 tests.
fn describe_holder(h Holder) string {
	if h.pet is Cat {
		return h.pet.name
	} else if h.pet is Dog {
		return h.pet.name
	}
	return 'unknown'
}

// holder_detail supports holder detail handling for v3 tests.
fn holder_detail(h Holder) int {
	match h.pet {
		Cat {
			return h.pet.age
		}
		Dog {
			return h.pet.tricks
		}
	}

	return 0
}

// Point represents point data used by v3 tests.
struct Point {
mut:
	x int
	y int
}

// Node represents node data used by v3 tests.
struct Node {
mut:
	value int
	left  int
	right int
}

// Rectangle represents rectangle data used by v3 tests.
struct Rectangle {
mut:
	width  int
	height int
	origin Point
}

__global (
	g_val    int
	g_count  int
	g_flag   bool
	g_point  Point
	g_vec    Vec3
	g_acc    int
	g_toggle bool
)

// ===================== HELPER FUNCTIONS =====================

// fib supports fib handling for v3 tests.
fn fib(n int) int {
	if n < 2 {
		return n
	}
	return fib(n - 1) + fib(n - 2)
}

// factorial supports factorial handling for v3 tests.
fn factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n - 1)
}

// sum_recursive supports sum recursive handling for v3 tests.
fn sum_recursive(n int) int {
	if n <= 0 {
		return 0
	}
	return n + sum_recursive(n - 1)
}

// Foo97 represents foo97 data used by v3 tests.
struct Foo97 {
	x    int
	y    int
	name string
	val  int
}

// new creates a Foo97 value for v3 tests.
fn Foo97.new(x int, y int) Foo97 {
	return Foo97{
		x: x
		y: y
	}
}

// with_name supports with name handling for Foo97.
fn Foo97.with_name(name string, val int) Foo97 {
	return Foo97{
		name: name
		val:  val
	}
}

// multiply supports multiply handling for v3 tests.
fn multiply(a int, b int) int {
	return a * b
}

// apply_op supports apply op handling for v3 tests.
fn apply_op(f fn (int, int) int, x int, y int) int {
	return f(x, y)
}

// gcd supports gcd handling for v3 tests.
fn gcd(a int, b int) int {
	if b == 0 {
		return a
	}
	return gcd(b, a % b)
}

// power supports power handling for v3 tests.
fn power(base int, exp int) int {
	if exp == 0 {
		return 1
	}
	return base * power(base, exp - 1)
}

// add updates add state for v3 tests.
fn add(a int, b int) int {
	return a + b
}

// sub supports sub handling for v3 tests.
fn sub(a int, b int) int {
	return a - b
}

// mul supports mul handling for v3 tests.
fn mul(a int, b int) int {
	return a * b
}

// print_rec updates print rec state for v3 tests.
fn print_rec(n int) {
	if n == 0 {
		return
	}
	print_rec(n / 10)
	rem := n - (n / 10) * 10
	C.putchar(rem + 48)
}

// print_int updates print int state for v3 tests.
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

// print_str updates print str state for v3 tests.
fn print_str(s string) {
	C.puts(s.str)
}

// repeat supports repeat handling for v3 tests.
fn repeat(c u8, n int) int {
	if c == `x` && n == 3 {
		return 8
	}
	return 0
}

// next_in_value returns next in value data for v3 tests.
fn next_in_value() int {
	g_count = g_count + 1
	return g_count
}

// maybe_review_value supports maybe review value handling for v3 tests.
fn maybe_review_value(ok bool) ?int {
	if ok {
		return 42
	}
	return none
}

// optional_arg_int104 supports optional arg int104 handling for v3 tests.
fn optional_arg_int104(x ?int) int {
	return x or { -1 }
}

// optional_arg_point104 supports optional arg point104 handling for v3 tests.
fn optional_arg_point104(p ?Point) int {
	pt := p or { return -1 }
	return pt.x + pt.y
}

// make_review_animal builds make review animal data for v3 tests.
fn make_review_animal() Animal {
	return Cat{
		name: 'Milo'
		age:  4
	}
}

// next supports next handling for UserId104.
fn (id UserId104) next() int {
	return int(id) + 1
}

// sum_many supports sum many handling for v3 tests.
fn sum_many(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a + b + c + d + e + f + g + h
}

// mul_many supports mul many handling for v3 tests.
fn mul_many(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a * b * c * d * e * f * g * h
}

// max_of_eight supports max of eight handling for v3 tests.
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

// weighted_sum supports weighted sum handling for v3 tests.
fn weighted_sum(a int, b int, c int, d int, e int, f int, g int, h int) int {
	return a * 1 + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8
}

// Color represents color data used by v3 tests.
struct Color {
mut:
	r int
	g int
	b int
	a int
}

// Vec3 represents vec3 data used by v3 tests.
struct Vec3 {
mut:
	x int
	y int
	z int
}

// Matrix2x2 represents matrix2x2 data used by v3 tests.
struct Matrix2x2 {
mut:
	a int
	b int
	c int
	d int
}

// LinkedNode represents linked node data used by v3 tests.
struct LinkedNode {
mut:
	val  int
	next int
}

// Stats represents stats data used by v3 tests.
struct Stats {
mut:
	min_v int
	max_v int
	sum   int
	count int
}

// make_point builds make point data for v3 tests.
fn make_point(px int, py int) Point {
	return Point{
		x: px
		y: py
	}
}

// add_points updates add points state for v3 tests.
fn add_points(a Point, b Point) Point {
	return Point{
		x: a.x + b.x
		y: a.y + b.y
	}
}

// abs_val supports abs val handling for v3 tests.
fn abs_val(n int) int {
	if n < 0 {
		return 0 - n
	}
	return n
}

// min_val supports min val handling for v3 tests.
fn min_val(a int, b int) int {
	if a < b {
		return a
	}
	return b
}

// max_val supports max val handling for v3 tests.
fn max_val(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

// clamp supports clamp handling for v3 tests.
fn clamp(val int, lo int, hi int) int {
	if val < lo {
		return lo
	}
	if val > hi {
		return hi
	}
	return val
}

// collatz_steps supports collatz steps handling for v3 tests.
fn collatz_steps(start int) int {
	mut v := start
	mut steps := 0
	for v != 1 {
		if v % 2 == 0 {
			v = v / 2
		} else {
			v = v * 3 + 1
		}
		steps++
	}
	return steps
}

// sum_digits supports sum digits handling for v3 tests.
fn sum_digits(n int) int {
	mut v := n
	if v < 0 {
		v = 0 - v
	}
	mut s := 0
	for v > 0 {
		s += v % 10
		v = v / 10
	}
	return s
}

// count_bits supports count bits handling for v3 tests.
fn count_bits(n int) int {
	mut v := n
	mut count := 0
	for v != 0 {
		count += v & 1
		v = v >> 1
	}
	return count
}

// classify supports classify handling for v3 tests.
fn classify(n int) int {
	if n > 100 {
		return 3
	} else if n > 50 {
		return 2
	} else if n > 0 {
		return 1
	} else {
		return 0
	}
}

// point_quadrant supports point quadrant handling for v3 tests.
fn point_quadrant(p Point) int {
	if p.x > 0 && p.y > 0 {
		return 1
	} else if p.x < 0 && p.y > 0 {
		return 2
	} else if p.x < 0 && p.y < 0 {
		return 3
	} else if p.x > 0 && p.y < 0 {
		return 4
	} else {
		return 0
	}
}

// make_color builds make color data for v3 tests.
fn make_color(r int, g int, b int, a int) Color {
	return Color{
		r: r
		g: g
		b: b
		a: a
	}
}

// color_brightness supports color brightness handling for v3 tests.
fn color_brightness(c Color) int {
	return (c.r + c.g + c.b) / 3
}

// scale_rect supports scale rect handling for v3 tests.
fn scale_rect(mut r Rectangle, factor int) {
	r.width = r.width * factor
	r.height = r.height * factor
}

// modify_struct supports modify struct handling for v3 tests.
fn modify_struct(mut p Point) {
	p.x = 999
	p.y = 888
}

// swap_point supports swap point handling for v3 tests.
fn swap_point(mut p Point) {
	tmp := p.x
	p.x = p.y
	p.y = tmp
}

// scale_point supports scale point handling for v3 tests.
fn scale_point(mut p Point, factor int) {
	p.x = p.x * factor
	p.y = p.y * factor
}

// translate_point supports translate point handling for v3 tests.
fn translate_point(mut p Point, dx int, dy int) {
	p.x = p.x + dx
	p.y = p.y + dy
}

// reset_point updates reset point state for v3 tests.
fn reset_point(mut p Point) {
	p.x = 0
	p.y = 0
}

// vec3_dot supports vec3 dot handling for v3 tests.
fn vec3_dot(a Vec3, b Vec3) int {
	return a.x * b.x + a.y * b.y + a.z * b.z
}

// vec3_cross_z supports vec3 cross z handling for v3 tests.
fn vec3_cross_z(a Vec3, b Vec3) int {
	return a.x * b.y - a.y * b.x
}

// vec3_len_sq supports vec3 len sq handling for v3 tests.
fn vec3_len_sq(v Vec3) int {
	return v.x * v.x + v.y * v.y + v.z * v.z
}

// vec3_add supports vec3 add handling for v3 tests.
fn vec3_add(a Vec3, b Vec3) Vec3 {
	return Vec3{
		x: a.x + b.x
		y: a.y + b.y
		z: a.z + b.z
	}
}

// vec3_scale supports vec3 scale handling for v3 tests.
fn vec3_scale(v Vec3, s int) Vec3 {
	return Vec3{
		x: v.x * s
		y: v.y * s
		z: v.z * s
	}
}

// mat_det supports mat det handling for v3 tests.
fn mat_det(m Matrix2x2) int {
	return m.a * m.d - m.b * m.c
}

// mat_mul supports mat mul handling for v3 tests.
fn mat_mul(m Matrix2x2, n Matrix2x2) Matrix2x2 {
	return Matrix2x2{
		a: m.a * n.a + m.b * n.c
		b: m.a * n.b + m.b * n.d
		c: m.c * n.a + m.d * n.c
		d: m.c * n.b + m.d * n.d
	}
}

// mat_trace supports mat trace handling for v3 tests.
fn mat_trace(m Matrix2x2) int {
	return m.a + m.d
}

// is_prime reports whether is prime applies in v3 tests.
fn is_prime(n int) bool {
	if n < 2 {
		return false
	}
	mut i := 2
	for i * i <= n {
		if n % i == 0 {
			return false
		}
		i++
	}
	return true
}

// isqrt supports isqrt handling for v3 tests.
fn isqrt(n int) int {
	if n <= 0 {
		return 0
	}
	mut x := n
	mut y := (x + 1) / 2
	for y < x {
		x = y
		y = (x + n / x) / 2
	}
	return x
}

// reverse_int supports reverse int handling for v3 tests.
fn reverse_int(n int) int {
	mut v := n
	mut neg := false
	if v < 0 {
		neg = true
		v = 0 - v
	}
	mut result := 0
	for v > 0 {
		result = result * 10 + v % 10
		v = v / 10
	}
	if neg {
		return 0 - result
	}
	return result
}

// count_digits supports count digits handling for v3 tests.
fn count_digits(n int) int {
	if n == 0 {
		return 1
	}
	mut v := n
	if v < 0 {
		v = 0 - v
	}
	mut count := 0
	for v > 0 {
		count++
		v = v / 10
	}
	return count
}

// is_palindrome_num reports whether is palindrome num applies in v3 tests.
fn is_palindrome_num(n int) bool {
	if n < 0 {
		return false
	}
	return n == reverse_int(n)
}

// update_stats supports update stats handling for v3 tests.
fn update_stats(mut s Stats, val int) {
	if s.count == 0 || val < s.min_v {
		s.min_v = val
	}
	if s.count == 0 || val > s.max_v {
		s.max_v = val
	}
	s.sum += val
	s.count++
}

// binary_search_step supports binary search step handling for v3 tests.
fn binary_search_step(target int, lo int, hi int, a0 int, a1 int, a2 int, a3 int, a4 int) int {
	if lo > hi {
		return 0 - 1
	}
	mid := (lo + hi) / 2
	mut mid_val := 0
	if mid == 0 {
		mid_val = a0
	} else if mid == 1 {
		mid_val = a1
	} else if mid == 2 {
		mid_val = a2
	} else if mid == 3 {
		mid_val = a3
	} else {
		mid_val = a4
	}

	if mid_val == target {
		return mid
	} else if mid_val < target {
		return binary_search_step(target, mid + 1, hi, a0, a1, a2, a3, a4)
	}
	return binary_search_step(target, lo, mid - 1, a0, a1, a2, a3, a4)
}

// ackermann supports ackermann handling for v3 tests.
fn ackermann(m int, n int) int {
	if m == 0 {
		return n + 1
	}
	if n == 0 {
		return ackermann(m - 1, 1)
	}
	return ackermann(m - 1, ackermann(m, n - 1))
}

// triangle_area_2x supports triangle area 2x handling for v3 tests.
fn triangle_area_2x(x1 int, y1 int, x2 int, y2 int, x3 int, y3 int) int {
	area := x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)
	if area < 0 {
		return 0 - area
	}
	return area
}

// rotate_point_90 supports rotate point 90 handling for v3 tests.
fn rotate_point_90(p Point) Point {
	return Point{
		x: 0 - p.y
		y: p.x
	}
}

// manhattan_dist supports manhattan dist handling for v3 tests.
fn manhattan_dist(a Point, b Point) int {
	mut dx := a.x - b.x
	mut dy := a.y - b.y
	if dx < 0 {
		dx = 0 - dx
	}
	if dy < 0 {
		dy = 0 - dy
	}
	return dx + dy
}

// digital_root supports digital root handling for v3 tests.
fn digital_root(n int) int {
	mut v := n
	if v < 0 {
		v = 0 - v
	}
	for v >= 10 {
		v = sum_digits(v)
	}
	return v
}

// lerp supports lerp handling for v3 tests.
fn lerp(a int, b int, t_num int, t_den int) int {
	return a + (b - a) * t_num / t_den
}

// sign supports sign handling for v3 tests.
fn sign(n int) int {
	if n > 0 {
		return 1
	} else if n < 0 {
		return 0 - 1
	}
	return 0
}

// popcount_loop supports popcount loop handling for v3 tests.
fn popcount_loop(n int) int {
	mut v := n
	mut c := 0
	for v != 0 {
		v = v & (v - 1)
		c++
	}
	return c
}

// leading_zeros supports leading zeros handling for v3 tests.
fn leading_zeros(n int) int {
	if n == 0 {
		return 32
	}
	mut v := n
	mut count := 0
	mut mask := 1 << 30
	for mask > 0 {
		if v & mask != 0 {
			return count
		}
		count++
		mask = mask >> 1
	}
	return count
}

// ===================== METHODS =====================

// sum supports sum handling for Point.
fn (p Point) sum() int {
	return p.x + p.y
}

// product supports product handling for Point.
fn (p Point) product() int {
	return p.x * p.y
}

// double supports double handling for Point.
fn (mut p Point) double() {
	p.x = p.x * 2
	p.y = p.y * 2
}

// area supports area handling for Rectangle.
fn (r Rectangle) area() int {
	return r.width * r.height
}

// perimeter supports perimeter handling for Rectangle.
fn (r Rectangle) perimeter() int {
	return 2 * (r.width + r.height)
}

// total supports total handling for Node.
fn (n Node) total() int {
	return n.value + n.left + n.right
}

// ===================== IF-EXPRESSION HELPERS =====================

// int_abs supports int abs handling for v3 tests.
fn int_abs(a int) int {
	return if a < 0 { 0 - a } else { a }
}

// int_max2 supports int max2 handling for v3 tests.
fn int_max2(a int, b int) int {
	return if a > b { a } else { b }
}

// int_min2 supports int min2 handling for v3 tests.
fn int_min2(a int, b int) int {
	return if a < b { a } else { b }
}

// sign_expr supports sign expr handling for v3 tests.
fn sign_expr(x int) int {
	return if x < 0 {
		0 - 1
	} else {
		if x > 0 {
			1
		} else {
			0
		}
	}
}

// clamp_expr supports clamp expr handling for v3 tests.
fn clamp_expr(x int, lo int, hi int) int {
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

// ===================== DEFER HELPERS =====================

// defer_return_test supports defer return test handling for v3 tests.
fn defer_return_test() int {
	defer {
		g_acc += 100
	}
	return 42
}

// defer_order_test supports defer order test handling for v3 tests.
fn defer_order_test() {
	defer { g_acc += 1 }
	defer { g_acc += 10 }
	defer { g_acc += 100 }
}

// ===================== ENUMS =====================

// nested_return supports nested return handling for v3 tests.
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

// operator_to_name converts operator to name data for v3 tests.
fn operator_to_name(op string) string {
	return match op {
		'+' { '__plus' }
		'-' { '__minus' }
		'*' { '__mul' }
		'/' { '__div' }
		else { op }
	}
}

// get_type_name returns get type name data for v3 tests.
fn get_type_name(is_signed bool, size int) string {
	return if is_signed {
		match size {
			8 { 'i8' }
			16 { 'i16' }
			32 { 'int' }
			64 { 'i64' }
			else { 'int' }
		}
	} else {
		match size {
			8 { 'u8' }
			16 { 'u16' }
			32 { 'u32' }
			else { 'u64' }
		}
	}
}

// test_return_if_expr validates return if expr behavior in v3 tests.
fn test_return_if_expr() {
	assert get_type_name(true, 32) == 'int'
	assert get_type_name(false, 64) == 'u64'
	assert get_type_name(true, 8) == 'i8'
	assert get_type_name(false, 16) == 'u16'
	print_str('return if expr: ok')
}

// Direction lists direction values used by v3 tests.
enum Direction {
	up
	down
	left
	right
}

// ===================== MULTI-RETURN FUNCTIONS =====================

// two_vals supports two vals handling for v3 tests.
fn two_vals() (int, string) {
	return 42, 'hello'
}

// swap_ints supports swap ints handling for v3 tests.
fn swap_ints(a int, b int) (int, int) {
	return b, a
}

// three_ints supports three ints handling for v3 tests.
fn three_ints(x int) (int, int, int) {
	return x, x * 2, x * 3
}

// ===================== STRUCT OPERATORS =====================

// operator + implements the + operator for Point.
fn (a Point) + (b Point) Point {
	return Point{
		x: a.x + b.x
		y: a.y + b.y
	}
}

// operator == implements the == operator for Point.
fn (a Point) == (b Point) bool {
	return a.x == b.x && a.y == b.y
}

// operator < implements the < operator for Point.
fn (a Point) < (b Point) bool {
	return a.x + a.y < b.x + b.y
}

// ===================== RETURN MATCH / IF-EXPR-STRING =====================

// classify_str supports classify str handling for v3 tests.
fn classify_str(n int) string {
	return match n {
		0 { 'zero' }
		1 { 'one' }
		2 { 'two' }
		else { 'other' }
	}
}

// describe_sign supports describe sign handling for v3 tests.
fn describe_sign(n int) string {
	return if n > 0 {
		'positive'
	} else if n < 0 {
		'negative'
	} else {
		'zero'
	}
}

// return_assoc_point supports return assoc point handling for v3 tests.
fn return_assoc_point(p Point, new_x int) Point {
	return Point{
		...p
		x: new_x
	}
}

// ===================== MAIN TEST FUNCTION =====================

// try_get_value supports try get value handling for v3 tests.
fn try_get_value(ok bool) ?int {
	if ok {
		return 42
	}
	return none
}

// CallInfo115 represents call info115 data used by v3 tests.
struct CallInfo115 {
	name  string
	score int
}

// maybe_call_info115 supports maybe call info115 handling for v3 tests.
fn maybe_call_info115(ok bool) ?CallInfo115 {
	if ok {
		return CallInfo115{
			name:  'resolved'
			score: 7
		}
	}
	return none
}

// make_scores115 builds make scores115 data for v3 tests.
fn make_scores115() map[string]int {
	mut scores := map[string]int{}
	scores['alpha'] = 2
	scores['beta'] = 3
	scores['gamma'] = 4
	return scores
}

// Defaults116 represents defaults116 data used by v3 tests.
struct Defaults116 {
	name  string = 'v' + '3'
	count int    = 42
}

// MetaType117 aliases meta type117 values used by v3 tests.
type MetaType117 = MetaNamed117
	| MetaIface117
	| MetaArray117
	| MetaMap117
	| MetaFn117
	| MetaPointer117

// CastLeak117 aliases cast leak117 values used by v3 tests.
type CastLeak117 = CastNamed117 | CastOther117

// SumNameLeft117 aliases sum name left117 values used by v3 tests.
type SumNameLeft117 = SumNameLeftNamed117 | SumNameLeftOther117

// SumNameRight117 aliases sum name right117 values used by v3 tests.
type SumNameRight117 = SumNameRightNamed117 | SumNameRightOther117

// MetaNamed117 represents meta named117 data used by v3 tests.
struct MetaNamed117 {
	name string
}

// MetaIface117 represents meta iface117 data used by v3 tests.
struct MetaIface117 {
	name string
}

// MetaArray117 represents meta array117 data used by v3 tests.
struct MetaArray117 {
	elem MetaType117
}

// MetaMap117 represents meta map117 data used by v3 tests.
struct MetaMap117 {
	key MetaType117
	val MetaType117
}

// MetaFn117 represents meta fn117 data used by v3 tests.
struct MetaFn117 {
	params []MetaType117
	ret    MetaType117
}

// MetaPointer117 represents meta pointer117 data used by v3 tests.
struct MetaPointer117 {
	base MetaType117
}

// Registry117 represents registry117 data used by v3 tests.
struct Registry117 {
	prefix string
mut:
	items map[string]MetaType117
}

// Stack117 represents stack117 data used by v3 tests.
struct Stack117 {
mut:
	values []int
}

// Instr117 represents instr117 data used by v3 tests.
struct Instr117 {
	op       int
	operands []int
}

// FieldBag117 represents field bag117 data used by v3 tests.
struct FieldBag117 {
	values []int
}

// TypeStore117 represents type store117 data used by v3 tests.
struct TypeStore117 {
	types []FieldBag117
}

// Symbol117 represents symbol117 data used by v3 tests.
struct Symbol117 {
	sect int
mut:
	value int
}

// SymbolStore117 represents symbol store117 data used by v3 tests.
struct SymbolStore117 {
mut:
	symbols []Symbol117
}

// ForField117 represents for field117 data used by v3 tests.
struct ForField117 {
	name string
	typ  MetaType117
}

// OptionalContext117 represents optional context117 data used by v3 tests.
struct OptionalContext117 {
	expr    string
	variant string
	sum     string
}

// OptionalContextHolder117 represents optional context holder117 data used by v3 tests.
struct OptionalContextHolder117 {
	items []OptionalContext117
}

// ConstHolder117 represents const holder117 data used by v3 tests.
struct ConstHolder117 {
	value int
}

const zero_const_holder117 = ConstHolder117{}

// CastNamed117 represents cast named117 data used by v3 tests.
struct CastNamed117 {
	name string
}

// CastOther117 represents cast other117 data used by v3 tests.
struct CastOther117 {
	id int
}

// SumNameLeftNamed117 represents sum name left named117 data used by v3 tests.
struct SumNameLeftNamed117 {
	value string
}

// SumNameLeftOther117 represents sum name left other117 data used by v3 tests.
struct SumNameLeftOther117 {}

// SumNameRightNamed117 represents sum name right named117 data used by v3 tests.
struct SumNameRightNamed117 {
	value string
}

// SumNameRightOther117 represents sum name right other117 data used by v3 tests.
struct SumNameRightOther117 {}

// Marker117 lists marker117 values used by v3 tests.
enum Marker117 {
	off
	on
}

// ScalarAlias117 aliases scalar alias117 values used by v3 tests.
type ScalarAlias117 = int

const fixed_len117 = 3

// FixedHolder117 represents fixed holder117 data used by v3 tests.
struct FixedHolder117 {
	values [fixed_len117]int
}

// FnHolder117 represents fn holder117 data used by v3 tests.
struct FnHolder117 {
	op fn (int, int) int
}

// smartcast_name117 supports smartcast name117 handling for v3 tests.
fn smartcast_name117(t CastLeak117) string {
	if t is CastNamed117 {
		return t.name
	}
	return 'other'
}

// smartcast_as_name117 converts smartcast as name117 data for v3 tests.
fn smartcast_as_name117(t CastLeak117) string {
	if t is CastNamed117 {
		named := t as CastNamed117
		return named.name
	}
	return 'other'
}

// is_on reports whether is on applies in v3 tests.
fn (t Marker117) is_on() bool {
	return t == .on
}

// qualify supports qualify handling for Registry117.
fn (r &Registry117) qualify(name string) string {
	if r.prefix.len == 0 {
		return name
	}
	return '${r.prefix}.${name}'
}

// meta_name117 supports meta name117 handling for v3 tests.
fn meta_name117(t MetaType117) string {
	if t is MetaNamed117 {
		return t.name
	}
	if t is MetaArray117 {
		return '[]' + meta_name117(t.elem)
	}
	if t is MetaMap117 {
		return 'map[' + meta_name117(t.key) + ']' + meta_name117(t.val)
	}
	if t is MetaPointer117 {
		return '&' + meta_name117(t.base)
	}
	return 'unknown'
}

// meta_array_elem117 supports meta array elem117 handling for v3 tests.
fn meta_array_elem117(t MetaType117) MetaType117 {
	if t is MetaArray117 {
		return t.elem
	}
	return MetaType117(MetaNamed117{
		name: 'none'
	})
}

// meta_fn_signature117 supports meta fn signature117 handling for v3 tests.
fn meta_fn_signature117(t MetaType117) string {
	if t is MetaFn117 {
		mut parts := []string{}
		for param117 in t.params {
			parts << meta_name117(param117)
		}
		return 'fn(${parts.join(', ')}) ${meta_name117(t.ret)}'
	}
	return 'not-fn'
}

// meta_iface_name117 supports meta iface name117 handling for v3 tests.
fn meta_iface_name117(t MetaType117) string {
	if t is MetaIface117 {
		return t.name
	}
	return 'not-interface'
}

// meta_iface_passthrough117 supports meta iface passthrough117 handling for v3 tests.
fn meta_iface_passthrough117(v MetaIface117) string {
	return v.name
}

// meta_iface_call117 supports meta iface call117 handling for v3 tests.
fn meta_iface_call117(t MetaType117) string {
	if t is MetaIface117 {
		return meta_iface_passthrough117(t)
	}
	return 'not-interface'
}

// pointer_array_elem117 supports pointer array elem117 handling for v3 tests.
fn pointer_array_elem117(t MetaType117) string {
	mut arr := MetaArray117{
		elem: MetaType117(MetaNamed117{
			name: 'none'
		})
	}
	if t is MetaPointer117 {
		ptr := t
		base := ptr.base
		if base is MetaArray117 {
			arr = base
		}
	}
	return meta_name117(arr.elem)
}

// short_variant117 supports short variant117 handling for v3 tests.
fn short_variant117(name string) string {
	mut short := name
	if name.contains('.') {
		short = name.all_after_last('.')
	}
	return short
}

// optional_match_arith117 supports optional match arith117 handling for v3 tests.
fn optional_match_arith117(op string, left int, right int) ?int {
	match op {
		'div' {
			if right == 0 {
				return none
			}
			return left / right
		}
		'mod' {
			if right == 0 {
				return none
			}
			return left % right
		}
		else {
			return none
		}
	}
}

// maybe_strings117 supports maybe strings117 handling for v3 tests.
fn maybe_strings117(ok bool) ?[]string {
	if ok {
		mut values := []string{}
		values << 'ssa'
		values << 'arm64'
		return values
	}
	return none
}

// trim_last117 transforms trim last117 data for v3 tests.
fn trim_last117(mut values []int) {
	values.delete_last()
}

// trim_stack117 transforms trim stack117 data for v3 tests.
fn trim_stack117(mut stack Stack117) {
	stack.values.delete_last()
}

// scalar_alias_if117 converts scalar alias if117 data for v3 tests.
fn scalar_alias_if117(ok bool) ScalarAlias117 {
	return if ok { ScalarAlias117(11) } else { ScalarAlias117(12) }
}

// fixed_holder_sum117 supports fixed holder sum117 handling for v3 tests.
fn fixed_holder_sum117(holder FixedHolder117) int {
	return holder.values[0] + holder.values[1] + holder.values[2]
}

// add117 supports add117 handling for v3 tests.
fn add117(a int, b int) int {
	return a + b
}

// fn_holder_call117 supports fn holder call117 handling for v3 tests.
fn fn_holder_call117(holder FnHolder117) int {
	return holder.op(4, 5)
}

// operand_total117 supports operand total117 handling for Instr117.
fn (i &Instr117) operand_total117(extra int) int {
	return i.op + i.operands[0] + extra
}

// indexed_struct_field117 supports indexed struct field117 handling for v3 tests.
fn indexed_struct_field117(store TypeStore117, idx int) int {
	bag := store.types[idx]
	if bag.values.len > 0 {
		return bag.values[0]
	}
	return -1
}

// const_holder_value117 supports const holder value117 handling for v3 tests.
fn const_holder_value117() int {
	holder := unsafe { &zero_const_holder117 }
	return holder.value
}

// adjust_symbol_values117 supports adjust symbol values117 handling for v3 tests.
fn adjust_symbol_values117(mut store SymbolStore117, base int) int {
	for i in 0 .. store.symbols.len {
		if store.symbols[i].sect == 2 {
			store.symbols[i].value += base
		}
	}
	return store.symbols[0].value + store.symbols[1].value
}

// sum_string_for_in117 supports sum string for in117 handling for v3 tests.
fn sum_string_for_in117(values []string) int {
	mut total := 0
	for value117 in values {
		total += value117.len
	}
	return total
}

// find_field_type117 resolves find field type117 information for v3 tests.
fn find_field_type117(fields []ForField117, wanted string) string {
	for field117 in fields {
		if field117.name == wanted {
			return meta_name117(field117.typ)
		}
	}
	return 'missing'
}

// map_clone_pair117 supports map clone pair117 handling for v3 tests.
fn map_clone_pair117() int {
	original117 := make_scores115()
	mut cloned117 := make_scores115()
	cloned117 = original117.clone()
	cloned117['alpha'] = 7
	orig117 := original117['alpha'] or { -1 }
	copy117 := cloned117['alpha'] or { -1 }
	return orig117 * 10 + copy117
}

// optional_context117 supports optional context117 handling for v3 tests.
fn optional_context117(holder OptionalContextHolder117, wanted string) ?OptionalContext117 {
	mut i := holder.items.len - 1
	for i >= 0 {
		if holder.items[i].expr == wanted {
			return holder.items[i]
		}
		i--
	}
	return none
}

// optional_context_score117 supports optional context score117 handling for v3 tests.
fn optional_context_score117() int {
	holder117 := OptionalContextHolder117{
		items: [
			OptionalContext117{
				expr:    'node'
				variant: 'Ident'
				sum:     'Expr'
			},
			OptionalContext117{
				expr:    'other'
				variant: 'Other'
				sum:     'Expr'
			},
		]
	}
	ctx117 := optional_context117(holder117, 'node') or { return 0 }
	if ctx117.expr == 'node' && ctx117.variant == 'Ident' && ctx117.sum == 'Expr'
		&& !ctx117.variant.contains('.') {
		return 1
	}
	return 0
}

// signed_narrow_load_score117 supports signed narrow load score117 handling for v3 tests.
fn signed_narrow_load_score117() int {
	mut values117 := []i8{}
	values117 << i8(-1)
	values117 << i8(7)
	if values117[0] < 0 && values117[1] > 0 {
		return 15
	}
	return 0
}

// push_mut_stack_selector117 updates push mut stack selector117 state for v3 tests.
fn push_mut_stack_selector117(mut stack Stack117, value int) {
	stack.values << value
}

// mut_selector_base_score117 supports mut selector base score117 handling for v3 tests.
fn mut_selector_base_score117() int {
	mut stack117 := Stack117{
		values: [3]
	}
	push_mut_stack_selector117(mut stack117, 8)
	return stack117.values[0] * 10 + stack117.values[1]
}

// chained_if_expr_score117 supports chained if expr score117 handling for v3 tests.
fn chained_if_expr_score117(value int) int {
	typ117 := if value == 0 {
		MetaType117(MetaNamed117{
			name: 'zero'
		})
	} else if value == 7 {
		MetaType117(MetaArray117{
			elem: MetaType117(MetaNamed117{
				name: 'int'
			})
		})
	} else {
		MetaType117(MetaPointer117{
			base: MetaType117(MetaNamed117{
				name: 'fallback'
			})
		})
	}
	if meta_name117(typ117) == '[]int' {
		return 80
	}
	return 0
}

// name returns name data for SumNameLeft117.
fn (t SumNameLeft117) name() string {
	if t is SumNameLeftNamed117 {
		return t.value
	}
	return 'left-other'
}

// name returns name data for SumNameRight117.
fn (t SumNameRight117) name() string {
	if t is SumNameRightNamed117 {
		return t.value
	}
	return 'right-other'
}

// sum_type_method_same_name_score117
// supports helper handling in v3 tests.
fn sum_type_method_same_name_score117() int {
	left117 := SumNameLeft117(SumNameLeftNamed117{
		value: 'left'
	})
	right117 := SumNameRight117(SumNameRightNamed117{
		value: 'right'
	})
	if left117.name() == 'left' && right117.name() == 'right' {
		return 33
	}
	return 0
}

// array_pop_score117 supports array pop score117 handling for v3 tests.
fn array_pop_score117() int {
	mut values117 := [4, 6, 9]
	last117 := values117.pop()
	return last117 * 10 + values117.len
}

// array_repeat_score117 supports array repeat score117 handling for v3 tests.
fn array_repeat_score117() int {
	base117 := []int{len: 2, init: 5}
	values117 := base117.repeat(3)
	if values117.len == 6 {
		return 6
	}
	return 0
}

// discard_assignment_side_effect_score117
// supports helper handling in v3 tests.
fn discard_assignment_side_effect_score117() int {
	g_count = 0
	_ = next_in_value()
	return g_count
}

// recursive_sum_method_score117 supports recursive sum method score117 handling for MetaType117.
fn (t MetaType117) recursive_sum_method_score117() int {
	if t is MetaPointer117 {
		if meta_name117(t.base) == 'core' {
			return 41
		}
	}
	return 0
}

// large_sumtype_method_receiver_score117 supports large_sumtype_method_receiver_score117 handling.
fn large_sumtype_method_receiver_score117() int {
	typ117 := MetaType117(MetaPointer117{
		base: MetaType117(MetaNamed117{
			name: 'core'
		})
	})
	return typ117.recursive_sum_method_score117()
}

// qualified_import_same_short_score117 supports qualified_import_same_short_score117 handling.
fn qualified_import_same_short_score117() int {
	mut score117 := repeat(`x`, 3)
	repeated117 := strings.repeat(`x`, 3)
	if repeated117.len == 3 && repeated117 == 'xxx' {
		score117 += 10
	}
	return score117
}

// sum_nine116 supports sum nine116 handling for v3 tests.
fn sum_nine116(a int, b int, c int, d int, e int, f int, g int, h int, i int) int {
	return a + b + c + d + e + f + g + h + i
}

// use_int116 supports use int116 handling for v3 tests.
fn use_int116(x int) int {
	return x
}

// BuildArch118 lists build arch118 values used by v3 tests.
enum BuildArch118 {
	amd64
	arm64
}

// callback_value118 supports callback value118 handling for v3 tests.
fn callback_value118(cb fn () int) int {
	return cb()
}

// maybe_label118 supports maybe label118 handling for v3 tests.
fn maybe_label118(ok bool) !string {
	if ok {
		return 'ok'
	}
	return error('bad')
}

// pass_error118 supports pass error118 handling for v3 tests.
fn pass_error118() !string {
	value := maybe_label118(false) or { return err }
	return value
}

// array_callback_enum_score118 supports array callback enum score118 handling for v3 tests.
fn array_callback_enum_score118() int {
	mut args118 := []string{}
	args118 << 'beta'
	args118 << '-old'
	args118 << 'alpha'
	filtered118 := args118.filter(it != '-old')
	mut sorted118 := filtered118.clone()
	sorted118.sort()
	mut score118 := sorted118.len * 10
	if sorted118[0] == 'alpha' && sorted118[1] == 'beta' {
		score118 += 3
	}
	if BuildArch118.arm64.str() == 'arm64' {
		score118 += 5
	}
	score118 += callback_value118(fn () int {
		return 7
	})
	_ := pass_error118() or { 'fallback' }
	return score118
}

// NestedIdent119 represents nested ident119 data used by v3 tests.
struct NestedIdent119 {
	name string
}

// NestedInfixExpr119 represents nested infix expr119 data used by v3 tests.
struct NestedInfixExpr119 {
	op string
}

// NestedMatchBranch119 represents nested match branch119 data used by v3 tests.
struct NestedMatchBranch119 {}

// NestedIfBranch119 represents nested if branch119 data used by v3 tests.
struct NestedIfBranch119 {}

// NestedExpr119 aliases nested expr119 values used by v3 tests.
type NestedExpr119 = NestedIdent119 | NestedInfixExpr119

// NestedNode119 aliases nested node119 values used by v3 tests.
type NestedNode119 = NestedExpr119 | NestedMatchBranch119 | NestedIfBranch119

// LoopHolder119 represents loop holder119 data used by v3 tests.
struct LoopHolder119 {
mut:
	current &NestedNode119 = unsafe { nil }
}

// SumPointerNilHolder119 represents sum pointer nil holder119 data used by v3 tests.
struct SumPointerNilHolder119 {
	node &NestedNode119 = unsafe { nil }
}

// SortItem119 represents sort item119 data used by v3 tests.
struct SortItem119 {
	rank int
	id   int
}

// OpVersion119 represents op version119 data used by v3 tests.
struct OpVersion119 {
	major int
}

// InterfaceField119 defines the interface field119 contract used by v3 tests.
interface InterfaceField119 {
	value int
}

// InterfaceFieldImpl119 represents interface field impl119 data used by v3 tests.
struct InterfaceFieldImpl119 {
	value int
}

// NilLink119 represents nil link119 data used by v3 tests.
struct NilLink119 {
mut:
	next &NilLink119 = unsafe { nil }
}

// IErrorDefault119 represents ierror default119 data used by v3 tests.
struct IErrorDefault119 {
	err IError = none
}

// OptionalInterp119 represents optional interp119 data used by v3 tests.
struct OptionalInterp119 {
	stop ?string
}

// MapMethodValue119 represents map method value119 data used by v3 tests.
struct MapMethodValue119 {
	kind int
}

// StaticNewLocal119 represents static new local119 data used by v3 tests.
struct StaticNewLocal119 {
	value int
}

// ZeroMapHolder119 represents zero map holder119 data used by v3 tests.
struct ZeroMapHolder119 {
mut:
	data map[string]int
}

// FeatureFlags119 lists feature flags119 values used by v3 tests.
@[flag]
enum FeatureFlags119 {
	name
	version
}

// RecursiveIf119 represents recursive if119 data used by v3 tests.
struct RecursiveIf119 {
	next &RecursiveExpr119 = unsafe { nil }
}

// RecursiveHash119 represents recursive hash119 data used by v3 tests.
struct RecursiveHash119 {
	id int
}

// HeapLocal119 represents heap local119 data used by v3 tests.
struct HeapLocal119 {
	value int
}

// SsaSumPayload119 represents ssa sum payload119 data used by v3 tests.
struct SsaSumPayload119 {
	code int
}

// FnFieldCall119 represents fn field call119 data used by v3 tests.
struct FnFieldCall119 {
	op fn (int, int) int
}

// RecursiveExpr119 aliases recursive expr119 values used by v3 tests.
type RecursiveExpr119 = RecursiveIf119 | RecursiveHash119

// RecursiveHashNode119 aliases recursive hash node119 values used by v3 tests.
type RecursiveHashNode119 = RecursiveIf119 | RecursiveHash119

// SsaSum119 aliases ssa sum119 values used by v3 tests.
type SsaSum119 = RecursiveHash119 | SsaSumPayload119

// C.atomic_fetch_add_u32 declares the C atomic_fetch_add_u32 symbol used by v3 tests.
fn C.atomic_fetch_add_u32(voidptr, u32) u32

// C.atomic_load_u16 declares the C atomic_load_u16 symbol used by v3 tests.
fn C.atomic_load_u16(voidptr) u16

// C.atomic_store_u16 declares the C atomic_store_u16 symbol used by v3 tests.
fn C.atomic_store_u16(voidptr, u16)

// C.atomic_compare_exchange_strong_u16 declares a C symbol used by v3 tests.
fn C.atomic_compare_exchange_strong_u16(voidptr, voidptr, u16) bool

// C.atomic_compare_exchange_weak_u32 declares a C symbol used by v3 tests.
fn C.atomic_compare_exchange_weak_u32(voidptr, voidptr, u32) bool

// C.atomic_compare_exchange_weak_byte declares a C symbol used by v3 tests.
fn C.atomic_compare_exchange_weak_byte(voidptr, voidptr, u8) bool

// C.atomic_compare_exchange_weak_u64 declares a C symbol used by v3 tests.
fn C.atomic_compare_exchange_weak_u64(voidptr, voidptr, u64) bool

// C.atomic_load_ptr declares the C atomic_load_ptr symbol used by v3 tests.
fn C.atomic_load_ptr(voidptr) voidptr

// C.atomic_store_ptr declares the C atomic_store_ptr symbol used by v3 tests.
fn C.atomic_store_ptr(voidptr, voidptr)

// C._wymix declares the C _wymix symbol used by v3 tests.
fn C._wymix(u64, u64) u64

// C.v_filelock_lock declares the C v_filelock_lock symbol used by v3 tests.
fn C.v_filelock_lock(i32, i32, i32, u64, u64) i32

// C.v_filelock_unlock declares the C v_filelock_unlock symbol used by v3 tests.
fn C.v_filelock_unlock(i32, u64, u64) i32

// C.v_prealloc_atomic_add_i32 declares the C v_prealloc_atomic_add_i32 symbol used by v3 tests.
fn C.v_prealloc_atomic_add_i32(voidptr, int) int

// C.v_prealloc_atomic_load_i32 declares the C v_prealloc_atomic_load_i32 symbol used by v3 tests.
fn C.v_prealloc_atomic_load_i32(voidptr) int

// C.v_prealloc_atomic_store_i32 declares the C v_prealloc_atomic_store_i32 symbol used by v3 tests.
fn C.v_prealloc_atomic_store_i32(voidptr, int) int

// C.v_prealloc_atomic_cas_i32 declares the C v_prealloc_atomic_cas_i32 symbol used by v3 tests.
fn C.v_prealloc_atomic_cas_i32(voidptr, int, int) int

// C.v_signal_with_handler_cast declares the C v_signal_with_handler_cast symbol used by v3 tests.
fn C.v_signal_with_handler_cast(i32, voidptr) voidptr

// maybe_const119 supports maybe const119 handling for v3 tests.
fn maybe_const119(ok bool) ?string {
	if ok {
		return 'set'
	}
	return none
}

const const_or119 = maybe_const119(false) or { 'fallback' }

// return_if_branch119 supports return if branch119 handling for v3 tests.
fn return_if_branch119(flag bool) int {
	return if flag { return 12 } else { 7 }
}

// match_smartcast_return119 supports match smartcast return119 handling for v3 tests.
fn match_smartcast_return119(expr NestedExpr119) string {
	ident119 := match expr {
		NestedIdent119 {
			expr as NestedIdent119
		}
		else {
			return 'not-ident'
		}
	}

	return ident119.name
}

// indexed_smartcast119 supports indexed smartcast119 handling for v3 tests.
fn indexed_smartcast119(items []NestedExpr119) string {
	first119 := items[0]
	if first119 is NestedIdent119 && first119.name == 'idx' {
		return first119.name
	}
	return 'none'
}

// nested_type_membership119 supports nested type membership119 handling for v3 tests.
fn nested_type_membership119(node NestedNode119) int {
	if node !in [NestedMatchBranch119, NestedIfBranch119, NestedInfixExpr119] {
		return 1
	}
	return 0
}

// sum_pointer_target119 supports sum pointer target119 handling for v3 tests.
fn sum_pointer_target119(node NestedNode119) int {
	mut holder119 := LoopHolder119{}
	match node {
		NestedIfBranch119 {
			holder119.current = unsafe { &node }
		}
		else {}
	}

	if isnil(holder119.current) {
		return 0
	}
	return 6
}

// compare_sort_item119 supports compare sort item119 handling for v3 tests.
fn compare_sort_item119(a &SortItem119, b &SortItem119) int {
	if a.rank < b.rank {
		return -1
	}
	if a.rank > b.rank {
		return 1
	}
	return 0
}

// sort_with_compare119 updates sort with compare119 state for v3 tests.
fn sort_with_compare119() int {
	mut items119 := [
		SortItem119{
			rank: 3
			id:   1
		},
		SortItem119{
			rank: 1
			id:   2
		},
		SortItem119{
			rank: 2
			id:   3
		},
	]
	items119.sort_with_compare(fn (a &SortItem119, b &SortItem119) int {
		if a.rank < b.rank {
			return -1
		}
		if a.rank > b.rank {
			return 1
		}
		return 0
	})
	sorted119 := items119.sorted_with_compare(compare_sort_item119)
	return items119[0].rank * 100 + items119[1].rank * 10 + items119[2].rank + sorted119[0].id
}

// operator == implements the == operator for OpVersion119.
fn (a OpVersion119) == (b OpVersion119) bool {
	return a.major == b.major
}

// operator < implements the < operator for OpVersion119.
fn (a OpVersion119) < (b OpVersion119) bool {
	return a.major < b.major
}

// operator_compare119 supports operator compare119 handling for v3 tests.
fn operator_compare119() int {
	low119 := OpVersion119{
		major: 1
	}
	high119 := OpVersion119{
		major: 2
	}
	same119 := OpVersion119{
		major: 2
	}
	mut score119 := 0
	if high119 > low119 {
		score119 += 2
	}
	if high119 >= low119 {
		score119 += 3
	}
	if low119 <= high119 {
		score119 += 4
	}
	if high119 != low119 {
		score119 += 5
	}
	if high119 >= same119 {
		score119 += 6
	}
	if high119 <= same119 {
		score119 += 7
	}
	return score119
}

// read_interface_field119 reads read interface field119 input for v3 tests.
fn read_interface_field119(item InterfaceField119) int {
	return item.value
}

// read_interface_field_ptr119 reads read interface field ptr119 input for v3 tests.
fn read_interface_field_ptr119(item &InterfaceField119) int {
	return item.value
}

// interface_field119 supports interface field119 handling for v3 tests.
fn interface_field119() int {
	item119 := InterfaceFieldImpl119{
		value: 19
	}
	return read_interface_field119(item119) + read_interface_field_ptr119(&item119)
}

// math_generic119 supports math generic119 handling for v3 tests.
fn math_generic119() int {
	return math.abs(0 - 6) + math.min(8, 5)
}

// module_const_method119 supports module const method119 handling for v3 tests.
fn module_const_method119() int {
	return token.scanner_matcher.find('fn')
}

// qualified_static_new119 supports qualified static new119 handling for v3 tests.
fn qualified_static_new119() int {
	mut matcher119 := token.KeywordsMatcherTrie.new(2)
	matcher119.add_word('module', 71)
	return matcher119.find('module')
}

// new creates a StaticNewLocal119 value for v3 tests.
fn StaticNewLocal119.new(value int) StaticNewLocal119 {
	return StaticNewLocal119{
		value: value + 2
	}
}

// local_static_new119 supports local static new119 handling for v3 tests.
fn local_static_new119() int {
	item119 := StaticNewLocal119.new(73)
	return item119.value
}

// module_function_new119 supports module function new119 handling for v3 tests.
fn module_function_new119() int {
	mut bench119 := bench.new()
	_ = bench119
	return 101
}

// nil_newline_pointer_assignment119
// supports helper handling in v3 tests.
fn nil_newline_pointer_assignment119() int {
	mut item119 := NilLink119{}
	mut link119 := &item119.next
	unsafe {
		item119.next = nil
		*link119 = &item119
	}
	if isnil(item119.next) {
		return 0
	}
	return 21
}

// ierror_none_field119 supports ierror none field119 handling for v3 tests.
fn ierror_none_field119() int {
	default119 := IErrorDefault119{}
	explicit119 := IErrorDefault119{
		err: none
	}
	_ = default119
	_ = explicit119
	return 29
}

// sum_pointer_nil_default119 supports sum pointer nil default119 handling for v3 tests.
fn sum_pointer_nil_default119() int {
	holder119 := SumPointerNilHolder119{}
	if isnil(holder119.node) {
		return 31
	}
	return 0
}

// optional_string_interp119 supports optional string interp119 handling for v3 tests.
fn optional_string_interp119() string {
	default119 := OptionalInterp119{}
	set119 := OptionalInterp119{
		stop: 'halt'
	}
	return '${default119.stop}|${set119.stop}'
}

// optional_cast_field119 supports optional cast field119 handling for v3 tests.
fn optional_cast_field119(arg string) string {
	data119 := OptionalInterp119{
		stop: ?string(arg)
	}
	return '${data119.stop}'
}

// optional_passthrough_field119 supports optional passthrough field119 handling for v3 tests.
fn optional_passthrough_field119(default_value ?string) string {
	data119 := OptionalInterp119{
		stop: default_value
	}
	return '${data119.stop}'
}

// escape_default_string supports escape default string handling for v3 tests.
fn escape_default_string(value string) string {
	return value
}

// flag_default_value supports flag default value handling for v3 tests.
fn flag_default_value(value string) string {
	return 'not-lowered:${value}'
}

// flag_default_value_lowering119 supports flag default value lowering119 handling for v3 tests.
fn flag_default_value_lowering119() string {
	return flag_default_value('abc')
}

// query_kind119 supports query kind119 handling for map[string]MapMethodValue119.
fn (m map[string]MapMethodValue119) query_kind119(name string) ?MapMethodValue119 {
	if name in m {
		return m[name]
	}
	return none
}

// map_receiver_method119 supports map receiver method119 handling for v3 tests.
fn map_receiver_method119() int {
	mut kinds119 := map[string]MapMethodValue119{}
	kinds119['name'] = MapMethodValue119{
		kind: 41
	}
	got119 := kinds119.query_kind119('name') or { return 0 }
	return got119.kind
}

// map_index_or_none119 supports map index or none119 handling for v3 tests.
fn map_index_or_none119(items map[string]MapMethodValue119, name string) ?MapMethodValue119 {
	return items[name] or { none }
}

// map_index_bang119 supports map index bang119 handling for v3 tests.
fn map_index_bang119(items map[string]MapMethodValue119, name string) !MapMethodValue119 {
	return items[name]!
}

// map_index_optional_return119 supports map index optional return119 handling for v3 tests.
fn map_index_optional_return119() int {
	mut kinds119 := map[string]MapMethodValue119{}
	kinds119['name'] = MapMethodValue119{
		kind: 41
	}
	got119 := map_index_or_none119(kinds119, 'name') or { return 0 }
	missing119 := map_index_or_none119(kinds119, 'missing') or {
		MapMethodValue119{
			kind: 7
		}
	}
	got_bang119 := map_index_bang119(kinds119, 'name') or { return 0 }
	missing_bang119 := map_index_bang119(kinds119, 'missing') or {
		MapMethodValue119{
			kind: 5
		}
	}
	return got119.kind + missing119.kind + got_bang119.kind + missing_bang119.kind
}

// ReceiverMethodLive119 represents receiver method live119 data used by v3 tests.
struct ReceiverMethodLive119 {
	base int
}

// hidden119 supports hidden119 handling for ReceiverMethodLive119.
fn (r ReceiverMethodLive119) hidden119(extra int) int {
	return r.base + extra
}

// visible119 supports visible119 handling for ReceiverMethodLive119.
fn (r ReceiverMethodLive119) visible119(flag bool) int {
	if flag {
		return r.hidden119(64)
	}
	return r.hidden119(1)
}

// receiver_method_liveness119 supports receiver method liveness119 handling for v3 tests.
fn receiver_method_liveness119() int {
	r := ReceiverMethodLive119{
		base: 53
	}
	return r.visible119(true)
}

// zero_map_lookup119 supports zero map lookup119 handling for v3 tests.
fn zero_map_lookup119() int {
	holder119 := ZeroMapHolder119{}
	if 'missing' !in holder119.data {
		return 79
	}
	return 0
}

// flag_enum_zero119 supports flag enum zero119 handling for v3 tests.
fn flag_enum_zero119() int {
	show119 := ~FeatureFlags119.zero() ^ .name
	if show119.has(.version) && !show119.has(.name) {
		return 43
	}
	return 0
}

// sum_alias_pointer_cast119 converts sum alias pointer cast119 data for v3 tests.
fn sum_alias_pointer_cast119(expr RecursiveExpr119) int {
	alias119 := &RecursiveHashNode119(expr as RecursiveIf119)
	if isnil(alias119) {
		return 0
	}
	return 9
}

// at_location119 supports at location119 handling for v3 tests.
fn at_location119() int {
	location119 := @LOCATION
	if location119.len > 0 {
		return 4
	}
	return 0
}

// c_atomic_wymix119 supports c atomic wymix119 handling for v3 tests.
fn c_atomic_wymix119() int {
	mut counter119 := u32(4)
	old119 := C.atomic_fetch_add_u32(voidptr(&counter119), u32(3))
	mix119 := C._wymix(u64(11), u64(17))
	mut score119 := int(old119) + int(counter119)
	if mix119 != 0 {
		score119 += 2
	}
	return score119
}

// c_atomic_channel_helpers119 supports c atomic channel helpers119 handling for v3 tests.
fn c_atomic_channel_helpers119() int {
	mut score119 := 0
	mut flag119 := u16(0)
	mut expected16_119 := u16(0)
	if C.atomic_compare_exchange_strong_u16(voidptr(&flag119), voidptr(&expected16_119), u16(3)) {
		score119 += int(C.atomic_load_u16(voidptr(&flag119)))
	}
	C.atomic_store_u16(voidptr(&flag119), u16(4))
	score119 += int(C.atomic_load_u16(voidptr(&flag119)))
	mut counter119 := u32(2)
	mut expected32_119 := u32(2)
	if C.atomic_compare_exchange_weak_u32(voidptr(&counter119), voidptr(&expected32_119), u32(5)) {
		score119 += int(counter119)
	}
	mut locked119 := u8(0)
	mut expected_byte119 := u8(0)
	if C.atomic_compare_exchange_weak_byte(voidptr(&locked119), voidptr(&expected_byte119), u8(8)) {
		score119 += int(locked119)
	}
	mut big119 := u64(10)
	mut expected64_119 := u64(10)
	if C.atomic_compare_exchange_weak_u64(voidptr(&big119), voidptr(&expected64_119), u64(12)) {
		score119 += int(big119)
	}
	value119 := 9
	mut slot119 := unsafe { nil }
	C.atomic_store_ptr(voidptr(&slot119), voidptr(&value119))
	if C.atomic_load_ptr(voidptr(&slot119)) == voidptr(&value119) {
		score119 += 6
	}
	return score119
}

// c_filelock_helpers119 supports c filelock helpers119 handling for v3 tests.
fn c_filelock_helpers119() int {
	lock_result119 := C.v_filelock_lock(i32(-1), 1, 1, u64(0), u64(0))
	unlock_result119 := C.v_filelock_unlock(i32(-1), u64(0), u64(0))
	if lock_result119 != 0 && unlock_result119 != 0 {
		return 47
	}
	return 0
}

// local_address_return119 supports local address return119 handling for v3 tests.
fn local_address_return119() &HeapLocal119 {
	item119 := HeapLocal119{
		value: 53
	}
	return &item119
}

// heap_local_return119 supports heap local return119 handling for v3 tests.
fn heap_local_return119() int {
	item119 := local_address_return119()
	return item119.value
}

// rune_array_string119 supports rune array string119 handling for v3 tests.
fn rune_array_string119() int {
	runes119 := [rune(`v`), rune(`3`)]
	if runes119.string() == 'v3' {
		return 61
	}
	return 0
}

// prealloc_atomic_helpers119 supports prealloc atomic helpers119 handling for v3 tests.
fn prealloc_atomic_helpers119() int {
	mut value119 := 1
	C.v_prealloc_atomic_store_i32(voidptr(&value119), 2)
	mut score119 := value119
	score119 += C.v_prealloc_atomic_load_i32(voidptr(&value119))
	score119 += C.v_prealloc_atomic_add_i32(voidptr(&value119), 3)
	if C.v_prealloc_atomic_cas_i32(voidptr(&value119), 5, 7) != 0 {
		score119 += value119
	}
	return score119
}

// signal_handler_cast119 supports signal handler cast119 handling for v3 tests.
fn signal_handler_cast119() int {
	prev119 := C.v_signal_with_handler_cast(0, unsafe { nil })
	if isnil(prev119) {
		return 67
	}
	return 67
}

// printing_builtin_helpers119 supports printing builtin helpers119 handling for v3 tests.
fn printing_builtin_helpers119() int {
	print('')
	eprint('')
	eprintln('')
	return 89
}

// string_last_part_helpers119 supports string last part helpers119 handling for v3 tests.
fn string_last_part_helpers119() int {
	path119 := 'vlib/v3/v3.v'
	if path119.all_before_last('/') == 'vlib/v3' && path119.all_after_last('/') == 'v3.v' {
		return 97
	}
	return 0
}

// join_path_variadic119 supports join path variadic119 handling for v3 tests.
fn join_path_variadic119() int {
	sep119 := os.path_separator
	expected119 := 'vlib' + sep119 + 'v3' + sep119 + 'v3.v'
	path119 := os.join_path('vlib', 'v3', 'v3.v')
	if path119 == expected119 {
		return 107
	}
	return 0
}

// ssa_sum_payload_store119 supports ssa sum payload store119 handling for v3 tests.
fn ssa_sum_payload_store119() int {
	payload119 := SsaSum119(SsaSumPayload119{
		code: 109
	})
	if payload119 is SsaSumPayload119 {
		return payload119.code
	}
	return 0
}

// add_fn_field119 updates add fn field119 state for v3 tests.
fn add_fn_field119(a int, b int) int {
	return a + b
}

// make_fn_field119 builds make fn field119 data for v3 tests.
fn make_fn_field119() FnFieldCall119 {
	return FnFieldCall119{
		op: add_fn_field119
	}
}

// selector_fn_call_base119 supports selector fn call base119 handling for v3 tests.
fn selector_fn_call_base119() int {
	return make_fn_field119().op(50, 63)
}

// channel_runtime119 supports channel runtime119 handling for v3 tests.
fn channel_runtime119() int {
	mutex119 := sync.new_mutex()
	_ = mutex119
	ch119 := chan bool{cap: 1}
	ch119 <- true
	got119 := <-ch119
	ch119.close()
	mut score119 := 0
	if got119 {
		score119 += 17
	}
	if ch119.closed {
		score119 += 19
	}
	waiters119 := [ch119]
	if waiters119[0] == ch119 {
		score119 += 23
	}
	return score119
}

// ZeroDefaultStruct119 represents zero default struct119 data used by v3 tests.
struct ZeroDefaultStruct119 {
	name    string
	count   int
	enabled bool
}

// zero_default_struct119 supports zero default struct119 handling for v3 tests.
fn zero_default_struct119() int {
	item119 := ZeroDefaultStruct119{}
	if item119.name == '' && item119.count == 0 && !item119.enabled {
		return 103
	}
	return 0
}

// main runs the v3 tests entry point.
fn main() {
	print_str('=== v3 Test Suite ===')

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
	cmp_val := 10
	flag3 := cmp_val > 5
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

	// 14.4 Heap Rectangle
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

	// ==================== 21. ELSE-IF CHAINS (5 tests) ====================
	print_str('--- 21. Else-If Chains ---')

	// 21.1 classify function (> 100)
	print_int(classify(200)) // 3

	// 21.2 classify (> 50)
	print_int(classify(75)) // 2

	// 21.3 classify (> 0)
	print_int(classify(25)) // 1

	// 21.4 classify (<= 0)
	print_int(classify(0)) // 0

	// 21.5 Multiple else-if inline
	val21 := 42
	mut r21 := 0
	if val21 > 100 {
		r21 = 5
	} else if val21 > 50 {
		r21 = 4
	} else if val21 > 40 {
		r21 = 3
	} else if val21 > 30 {
		r21 = 2
	} else {
		r21 = 1
	}
	print_int(r21) // 3

	// ==================== 22. FUNCTION RETURNING STRUCT (5 tests) ====================
	print_str('--- 22. Function Returning Struct ---')

	// 22.1 Basic make_point
	rp1 := make_point(10, 20)
	print_int(rp1.x) // 10
	print_int(rp1.y) // 20

	// 22.2 make_point with computation
	rp2 := make_point(3 * 5, 4 * 6)
	print_int(rp2.x) // 15
	print_int(rp2.y) // 24

	// 22.3 add_points
	rp3 := add_points(Point{ x: 10, y: 20 }, Point{
		x: 30
		y: 40
	})
	print_int(rp3.x) // 40
	print_int(rp3.y) // 60

	// 22.4 Chained struct returns
	rp4 := add_points(make_point(1, 2), make_point(3, 4))
	print_int(rp4.x) // 4
	print_int(rp4.y) // 6

	// 22.5 Return struct used in arithmetic
	rp5 := make_point(100, 200)
	print_int(rp5.x + rp5.y) // 300

	// ==================== 23. EARLY RETURN (5 tests) ====================
	print_str('--- 23. Early Return ---')

	// 23.1 abs positive
	print_int(abs_val(42)) // 42

	// 23.2 abs negative
	print_int(abs_val(0 - 17)) // 17

	// 23.3 abs zero
	print_int(abs_val(0)) // 0

	// 23.4 min
	print_int(min_val(10, 20)) // 10

	// 23.5 max
	print_int(max_val(10, 20)) // 20

	// ==================== 24. CLAMP & MULTI-ARG FUNCTIONS (5 tests) ====================
	print_str('--- 24. Clamp & Multi-Arg Functions ---')

	// 24.1 clamp below
	print_int(clamp(5, 10, 100)) // 10

	// 24.2 clamp above
	print_int(clamp(200, 10, 100)) // 100

	// 24.3 clamp in range
	print_int(clamp(50, 10, 100)) // 50

	// 24.4 clamp at boundary
	print_int(clamp(10, 10, 100)) // 10

	// 24.5 clamp at upper boundary
	print_int(clamp(100, 10, 100)) // 100

	// ==================== 25. POSTFIX INC/DEC (5 tests) ====================
	print_str('--- 25. Postfix Inc/Dec ---')

	// 25.1 Basic increment
	mut pi1 := 10
	pi1++
	print_int(pi1) // 11

	// 25.2 Basic decrement
	mut pd1 := 10
	pd1--
	print_int(pd1) // 9

	// 25.3 Multiple increments
	mut pi2 := 0
	pi2++
	pi2++
	pi2++
	pi2++
	pi2++
	print_int(pi2) // 5

	// 25.4 Inc and dec combined
	mut pid := 100
	pid++
	pid++
	pid--
	print_int(pid) // 101

	// 25.5 Postfix in loop
	mut pi3 := 0
	mut cnt25 := 0
	for pi3 < 10 {
		pi3++
		cnt25++
	}
	print_int(cnt25) // 10

	// ==================== 26. COMPOUND BITWISE ASSIGNMENT (5 tests) ====================
	print_str('--- 26. Compound Bitwise Assignment ---')

	// 26.1 OR assign
	mut bw1 := 0b0011
	bw1 |= 0b1100
	print_int(bw1) // 15

	// 26.2 AND assign
	mut bw2 := 0b1111
	bw2 &= 0b1010
	print_int(bw2) // 10

	// 26.3 XOR assign
	mut bw3 := 0b1100
	bw3 ^= 0b1010
	print_int(bw3) // 6

	// 26.4 Shift left assign
	mut bw4 := 1
	bw4 <<= 4
	print_int(bw4) // 16

	// 26.5 Shift right assign
	mut bw5 := 128
	bw5 >>= 3
	print_int(bw5) // 16

	// ==================== 27. COMPLEX BOOLEAN (5 tests) ====================
	print_str('--- 27. Complex Boolean ---')

	// 27.1 AND chain
	if 10 > 5 && 20 > 10 && 30 > 20 {
		print_int(1)
	} else {
		print_int(0)
	}

	// 27.2 OR chain
	if false || false || true {
		print_int(1)
	} else {
		print_int(0)
	}

	// 27.3 Mixed AND/OR
	if (true && false) || (true && true) {
		print_int(1)
	} else {
		print_int(0)
	}

	// 27.4 NOT with AND
	if !false && !false {
		print_int(1)
	} else {
		print_int(0)
	}

	// 27.5 Complex condition
	v27 := 42
	if v27 > 10 && v27 < 100 && v27 % 2 == 0 {
		print_int(1)
	} else {
		print_int(0)
	}

	// ==================== 28. ITERATIVE ALGORITHMS (5 tests) ====================
	print_str('--- 28. Iterative Algorithms ---')

	// 28.1 Collatz for 6 (6->3->10->5->16->8->4->2->1 = 8 steps)
	print_int(collatz_steps(6)) // 8

	// 28.2 Collatz for 27 (111 steps)
	print_int(collatz_steps(27)) // 111

	// 28.3 Collatz for 1 (0 steps)
	print_int(collatz_steps(1)) // 0

	// 28.4 Sum digits
	print_int(sum_digits(12345)) // 15

	// 28.5 Sum digits of large number
	print_int(sum_digits(99999)) // 45

	// ==================== 29. BIT COUNTING (5 tests) ====================
	print_str('--- 29. Bit Counting ---')

	// 29.1 count_bits of 0
	print_int(count_bits(0)) // 0

	// 29.2 count_bits of 7 (111)
	print_int(count_bits(7)) // 3

	// 29.3 count_bits of 255 (11111111)
	print_int(count_bits(255)) // 8

	// 29.4 count_bits of 1024 (10000000000)
	print_int(count_bits(1024)) // 1

	// 29.5 count_bits of 0b10101010
	print_int(count_bits(0b10101010)) // 4

	// ==================== 30. GLOBAL COUNTER PATTERNS (5 tests) ====================
	print_str('--- 30. Global Counter Patterns ---')

	// 30.1 Global counter in loop
	g_count = 0
	mut ig := 0
	for ig < 10 {
		g_count += ig
		ig++
	}
	print_int(g_count) // 45

	// 30.2 Global flag
	g_flag = false
	if g_count > 40 {
		g_flag = true
	}
	if g_flag {
		print_int(1)
	} else {
		print_int(0)
	}

	// 30.3 Global struct modification in loop
	g_point.x = 0
	g_point.y = 0
	mut ig2 := 1
	for ig2 <= 5 {
		g_point.x += ig2
		g_point.y += ig2 * ig2
		ig2++
	}
	print_int(g_point.x) // 15
	print_int(g_point.y) // 55

	// 30.4 Global with compound multiply
	g_val = 1
	mut ig3 := 1
	for ig3 <= 5 {
		g_val *= ig3
		ig3++
	}
	print_int(g_val) // 120

	// 30.5 Global reset and reuse
	g_val = 999
	g_val = 0
	g_val += 42
	print_int(g_val) // 42

	// ==================== 31. NESTED STRUCT MUTATION (5 tests) ====================
	print_str('--- 31. Nested Struct Mutation ---')

	// 31.1 Modify nested struct width/height
	mut rm1 := Rectangle{
		width:  10
		height: 20
		origin: Point{
			x: 1
			y: 2
		}
	}
	rm1.width = 50
	rm1.height = 60
	print_int(rm1.width) // 50
	print_int(rm1.height) // 60

	// 31.2 Scale rectangle via function
	mut rm2 := Rectangle{
		width:  10
		height: 20
		origin: Point{
			x: 0
			y: 0
		}
	}
	scale_rect(mut rm2, 5)
	print_int(rm2.width) // 50
	print_int(rm2.height) // 100

	// 31.3 Multiple rectangle modifications
	mut rm3 := Rectangle{
		width:  5
		height: 5
		origin: Point{
			x: 0
			y: 0
		}
	}
	rm3.width += 10
	rm3.height += 20
	print_int(rm3.width) // 15
	print_int(rm3.height) // 25

	// 31.4 Rectangle area after modification
	mut rm4 := Rectangle{
		width:  3
		height: 4
		origin: Point{
			x: 0
			y: 0
		}
	}
	rm4.width *= 10
	rm4.height *= 10
	print_int(rm4.width * rm4.height) // 1200

	// 31.5 Modify struct then pass to function
	mut rm5 := Point{
		x: 5
		y: 10
	}
	rm5.x *= 2
	rm5.y *= 3
	scale_point(mut rm5, 2)
	print_int(rm5.x) // 20
	print_int(rm5.y) // 60

	// ==================== 32. QUADRANT & STRUCT PASSING (5 tests) ====================
	print_str('--- 32. Quadrant & Struct Passing ---')

	// 32.1 Quadrant 1
	print_int(point_quadrant(Point{ x: 5, y: 5 })) // 1

	// 32.2 Quadrant 2
	print_int(point_quadrant(Point{ x: 0 - 5, y: 5 })) // 2

	// 32.3 Quadrant 3
	print_int(point_quadrant(Point{ x: 0 - 5, y: 0 - 5 })) // 3

	// 32.4 Quadrant 4
	print_int(point_quadrant(Point{ x: 5, y: 0 - 5 })) // 4

	// 32.5 Origin
	print_int(point_quadrant(Point{ x: 0, y: 0 })) // 0

	// ==================== 33. 4-FIELD STRUCT (5 tests) ====================
	print_str('--- 33. 4-Field Struct ---')

	// 33.1 Basic Color init
	c1 := Color{
		r: 255
		g: 128
		b: 64
		a: 255
	}
	print_int(c1.r) // 255
	print_int(c1.g) // 128

	// 33.2 Color brightness
	c2 := Color{
		r: 90
		g: 120
		b: 90
		a: 255
	}
	print_int(color_brightness(c2)) // 100

	// 33.3 make_color function return
	c3 := make_color(10, 20, 30, 40)
	print_int(c3.r + c3.g + c3.b + c3.a) // 100

	// 33.4 Color with zero alpha
	c4 := Color{
		r: 100
		g: 200
		b: 50
		a: 0
	}
	print_int(c4.a) // 0
	print_int(c4.b) // 50

	// 33.5 Mutable color
	mut c5 := Color{
		r: 0
		g: 0
		b: 0
		a: 0
	}
	c5.r = 255
	c5.g = 255
	c5.b = 255
	c5.a = 128
	print_int(c5.r + c5.g + c5.b) // 765
	print_int(c5.a) // 128

	// ==================== 34. FIBONACCI ITERATIVE (5 tests) ====================
	print_str('--- 34. Fibonacci Iterative ---')

	// 34.1 Fib(10) iteratively
	mut fa := 0
	mut fb := 1
	for fi := 0; fi < 10; fi++ {
		tmp := fa + fb
		fa = fb
		fb = tmp
	}
	print_int(fa) // 55

	// 34.2 Fib(20) iteratively
	fa = 0
	fb = 1
	for fi := 0; fi < 20; fi++ {
		tmp := fa + fb
		fa = fb
		fb = tmp
	}
	print_int(fa) // 6765

	// 34.3 Sum of first 10 fib numbers
	mut fsum := 0
	fa = 0
	fb = 1
	for fi := 0; fi < 10; fi++ {
		fsum += fa
		tmp := fa + fb
		fa = fb
		fb = tmp
	}
	print_int(fsum) // 88

	// 34.4 Count fib numbers below 100
	mut fcnt := 0
	fa = 0
	fb = 1
	for fa < 100 {
		fcnt++
		tmp := fa + fb
		fa = fb
		fb = tmp
	}
	print_int(fcnt) // 12

	// 34.5 Largest fib below 1000
	fa = 0
	fb = 1
	for fb < 1000 {
		tmp := fa + fb
		fa = fb
		fb = tmp
	}
	print_int(fa) // 987

	// ==================== 35. NESTED LOOPS WITH FLOW CONTROL (5 tests) ====================
	print_str('--- 35. Nested Loops with Flow Control ---')

	// 35.1 Nested with outer break
	mut sum35 := 0
	mut r35 := 0
	for r35 < 10 {
		mut c35 := 0
		for c35 < 10 {
			sum35++
			c35++
		}
		r35++
		if r35 >= 3 {
			break
		}
	}
	print_int(sum35) // 30

	// 35.2 Nested with inner continue
	mut sum35b := 0
	for r35b := 0; r35b < 5; r35b++ {
		for c35b := 0; c35b < 5; c35b++ {
			if c35b % 2 == 0 {
				continue
			}
			sum35b++
		}
	}
	print_int(sum35b) // 10 (5 rows * 2 odd cols)

	// 35.3 Triple nested
	mut sum35c := 0
	for i35 := 0; i35 < 3; i35++ {
		for j35 := 0; j35 < 3; j35++ {
			for k35 := 0; k35 < 3; k35++ {
				sum35c++
			}
		}
	}
	print_int(sum35c) // 27

	// 35.4 Nested with accumulating product
	mut prod35 := 0
	for i35 := 1; i35 <= 3; i35++ {
		for j35 := 1; j35 <= 3; j35++ {
			prod35 += i35 * j35
		}
	}
	print_int(prod35) // 36

	// 35.5 Skip diagonal
	mut sum35d := 0
	for i35 := 0; i35 < 4; i35++ {
		for j35 := 0; j35 < 4; j35++ {
			if i35 == j35 {
				continue
			}
			sum35d++
		}
	}
	print_int(sum35d) // 12

	// ==================== 36. COMPLEX MATCH (5 tests) ====================
	print_str('--- 36. Complex Match ---')

	// 36.1 Match with function call in body
	x36 := 3
	match x36 {
		1 { print_int(fib(5)) }
		2 { print_int(fib(6)) }
		3 { print_int(fib(7)) }
		else { print_int(0) }
	}

	// 13

	// 36.2 Match with computation in body
	x36b := 2
	match x36b {
		1 { print_int(10 * 10) }
		2 { print_int(20 * 20) }
		3 { print_int(30 * 30) }
		else { print_int(0) }
	}

	// 400

	// 36.3 Match on computed value
	x36c := 15 % 4
	match x36c {
		0 { print_int(100) }
		1 { print_int(200) }
		2 { print_int(300) }
		3 { print_int(400) }
		else { print_int(500) }
	}

	// 400

	// 36.4 Match in loop
	mut sum36 := 0
	for i36 := 0; i36 < 5; i36++ {
		match i36 {
			0 { sum36 += 1 }
			1 { sum36 += 10 }
			2 { sum36 += 100 }
			else { sum36 += 1000 }
		}
	}
	print_int(sum36) // 1 + 10 + 100 + 1000 + 1000 = 2111

	// 36.5 Sequential matches
	mut r36 := 0
	x36d := 5
	match x36d {
		5 { r36 += 100 }
		else { r36 += 1 }
	}

	match x36d {
		5 { r36 += 200 }
		else { r36 += 2 }
	}

	print_int(r36) // 300

	// ==================== 37. CHAINED FUNCTION CALLS (5 tests) ====================
	print_str('--- 37. Chained Function Calls ---')

	// 37.1 add(add(add(1,2),3),4) = 10
	print_int(add(add(add(1, 2), 3), 4)) // 10

	// 37.2 Nested mul and add
	print_int(add(mul(3, 4), mul(5, 6))) // 42

	// 37.3 sub(mul(add(2,3),4),5) = 15
	print_int(sub(mul(add(2, 3), 4), 5)) // 15

	// 37.4 min of max
	print_int(min_val(max_val(10, 20), max_val(5, 15))) // 15

	// 37.5 max of min
	print_int(max_val(min_val(10, 20), min_val(25, 30))) // 25

	// ==================== 38. MIXED ARITHMETIC (5 tests) ====================
	print_str('--- 38. Mixed Arithmetic ---')

	// 38.1 Shift + add
	print_int((1 << 8) + 1) // 257

	// 38.2 Bitwise + arithmetic
	print_int((0xFF & 0x0F) + 16) // 31

	// 38.3 Modulo + multiply
	print_int((100 % 7) * 10) // 20

	// 38.4 Shift + bitwise
	print_int((1 << 4) | (1 << 2)) // 20

	// 38.5 Complex expression
	v38 := 100
	print_int((v38 * 2 + v38 / 2) - (v38 % 3)) // 249

	// ==================== 39. LARGE COMPUTATIONS (5 tests) ====================
	print_str('--- 39. Large Computations ---')

	// 39.1 Large factorial (10!)
	mut lf := 1
	for li := 1; li <= 10; li++ {
		lf *= li
	}
	print_int(lf) // 3628800

	// 39.2 Power of 2^20
	mut lp := 1
	for li := 0; li < 20; li++ {
		lp *= 2
	}
	print_int(lp) // 1048576

	// 39.3 Sum of squares 1..20
	mut lsq := 0
	for li := 1; li <= 20; li++ {
		lsq += li * li
	}
	print_int(lsq) // 2870

	// 39.4 Triangular number T(100)
	mut tri := 0
	for li := 1; li <= 100; li++ {
		tri += li
	}
	print_int(tri) // 5050

	// 39.5 Product of 1..8
	print_int(mul_many(1, 2, 3, 4, 5, 6, 7, 8)) // 40320

	// ==================== 40. INTEGRATION TEST (5 tests) ====================
	print_str('--- 40. Integration Test ---')

	// 40.1 Struct + loop + function
	mut ip := make_point(0, 0)
	for ii := 1; ii <= 5; ii++ {
		ip = add_points(ip, make_point(ii, ii * 2))
	}
	print_int(ip.x) // 15
	print_int(ip.y) // 30

	// 40.2 Conditional + struct + global
	g_val = 0
	mut ip2 := Point{
		x: 1
		y: 1
	}
	for ii := 0; ii < 10; ii++ {
		if ii % 2 == 0 {
			ip2.x += ii
			g_val += 1
		} else {
			ip2.y += ii
		}
	}
	print_int(ip2.x) // 1 + 0 + 2 + 4 + 6 + 8 = 21
	print_int(ip2.y) // 1 + 1 + 3 + 5 + 7 + 9 = 26
	print_int(g_val) // 5

	// 40.3 Nested function + assert
	assert abs_val(0 - 42) == 42
	assert min_val(10, 20) == 10
	assert max_val(10, 20) == 20
	print_str('Integration asserts passed')

	// 40.4 Algorithm + match
	mut sum40 := 0
	for ii := 1; ii <= 10; ii++ {
		match classify(ii * 10) {
			1 { sum40 += 1 }
			2 { sum40 += 10 }
			3 { sum40 += 100 }
			else { sum40 += 0 }
		}
	}
	print_int(sum40) // 10: 1, 20: 1, 30: 1, 40: 1, 50: 1, 60: 10, 70: 10, 80: 10, 90: 10, 100: 10 = 5 + 50 = 55
	// Actually: 10->1, 20->1, 30->1, 40->1, 50->1, 60->2(>50), 70->2, 80->2, 90->2, 100->2(not >100)
	// so: 1*5 + 10*5 = 55

	// 40.5 Heap struct in loop with accumulation
	mut hsum := 0
	for ii := 0; ii < 5; ii++ {
		hp := &Point{
			x: ii * 3
			y: ii * 7
		}
		hsum += hp.x + hp.y
	}
	print_int(hsum) // (0+0)+(3+7)+(6+14)+(9+21)+(12+28) = 0+10+20+30+40 = 100

	// ==================== 41. VECTOR MATH (5 tests) ====================
	print_str('--- 41. Vector Math ---')

	va := Vec3{
		x: 1
		y: 2
		z: 3
	}
	vb := Vec3{
		x: 4
		y: 5
		z: 6
	}
	print_int(vec3_dot(va, vb)) // 1*4+2*5+3*6 = 32
	print_int(vec3_len_sq(va)) // 1+4+9 = 14
	vsum := vec3_add(va, vb)
	print_int(vsum.x) // 5
	print_int(vsum.y) // 7
	print_int(vsum.z) // 9

	// ==================== 42. VECTOR SCALE & CROSS (5 tests) ====================
	print_str('--- 42. Vector Scale & Cross ---')

	vs := vec3_scale(va, 3)
	print_int(vs.x) // 3
	print_int(vs.y) // 6
	print_int(vs.z) // 9
	print_int(vec3_cross_z(va, vb)) // 1*5 - 2*4 = -3
	vc := vec3_add(vec3_scale(va, 2), vec3_scale(vb, 3))
	print_int(vc.x) // 2+12 = 14

	// ==================== 43. MATRIX OPERATIONS (5 tests) ====================
	print_str('--- 43. Matrix Operations ---')

	m1 := Matrix2x2{
		a: 1
		b: 2
		c: 3
		d: 4
	}
	m2 := Matrix2x2{
		a: 5
		b: 6
		c: 7
		d: 8
	}
	print_int(mat_det(m1)) // 1*4-2*3 = -2
	print_int(mat_trace(m1)) // 1+4 = 5
	m3 := mat_mul(m1, m2)
	print_int(m3.a) // 1*5+2*7 = 19
	print_int(m3.b) // 1*6+2*8 = 22
	print_int(m3.d) // 3*6+4*8 = 50

	// ==================== 44. PRIME CHECKING (5 tests) ====================
	print_str('--- 44. Prime Checking ---')

	mut prime_count := 0
	for ii := 2; ii <= 30; ii++ {
		if is_prime(ii) {
			prime_count++
		}
	}
	print_int(prime_count) // primes: 2,3,5,7,11,13,17,19,23,29 = 10

	if is_prime(97) {
		print_int(1)
	} else {
		print_int(0)
	} // 1
	if is_prime(100) {
		print_int(1)
	} else {
		print_int(0)
	} // 0
	if is_prime(2) {
		print_int(1)
	} else {
		print_int(0)
	} // 1
	if is_prime(1) {
		print_int(1)
	} else {
		print_int(0)
	} // 0

	// ==================== 45. INTEGER SQUARE ROOT (5 tests) ====================
	print_str('--- 45. Integer Square Root ---')

	print_int(isqrt(0)) // 0
	print_int(isqrt(1)) // 1
	print_int(isqrt(4)) // 2
	print_int(isqrt(100)) // 10
	print_int(isqrt(99)) // 9

	// ==================== 46. REVERSE & PALINDROME (5 tests) ====================
	print_str('--- 46. Reverse & Palindrome ---')

	print_int(reverse_int(12345)) // 54321
	print_int(reverse_int(100)) // 1
	print_int(count_digits(12345)) // 5
	if is_palindrome_num(12321) {
		print_int(1)
	} else {
		print_int(0)
	} // 1
	if is_palindrome_num(12345) {
		print_int(1)
	} else {
		print_int(0)
	} // 0

	// ==================== 47. STATS TRACKING (5 tests) ====================
	print_str('--- 47. Stats Tracking ---')

	mut st_min := 0
	mut st_max := 0
	mut st_sum := 0
	mut st_cnt := 0
	// Inline stats tracking for values: 10, 3, 25, 7, 15
	// val=10
	st_min = 10
	st_max = 10
	st_sum = 10
	st_cnt = 1
	// val=3
	if 3 < st_min { st_min = 3 }
	if 3 > st_max { st_max = 3 }
	st_sum += 3
	st_cnt++
	// val=25
	if 25 < st_min { st_min = 25 }
	if 25 > st_max { st_max = 25 }
	st_sum += 25
	st_cnt++
	// val=7
	if 7 < st_min { st_min = 7 }
	if 7 > st_max { st_max = 7 }
	st_sum += 7
	st_cnt++
	// val=15
	if 15 < st_min { st_min = 15 }
	if 15 > st_max { st_max = 15 }
	st_sum += 15
	st_cnt++
	print_int(st_min) // 3
	print_int(st_max) // 25
	print_int(st_sum) // 60
	print_int(st_cnt) // 5
	print_int(st_sum / st_cnt) // 12

	// ==================== 48. BINARY SEARCH (5 tests) ====================
	print_str('--- 48. Binary Search ---')

	print_int(binary_search_step(30, 0, 4, 10, 20, 30, 40, 50)) // 2
	print_int(binary_search_step(10, 0, 4, 10, 20, 30, 40, 50)) // 0
	print_int(binary_search_step(50, 0, 4, 10, 20, 30, 40, 50)) // 4
	print_int(binary_search_step(35, 0, 4, 10, 20, 30, 40, 50)) // -1
	print_int(binary_search_step(40, 0, 4, 10, 20, 30, 40, 50)) // 3

	// ==================== 49. ACKERMANN FUNCTION (5 tests) ====================
	print_str('--- 49. Ackermann Function ---')

	print_int(ackermann(0, 0)) // 1
	print_int(ackermann(1, 1)) // 3
	print_int(ackermann(2, 2)) // 7
	print_int(ackermann(3, 2)) // 29
	print_int(ackermann(0, 5)) // 6

	// ==================== 50. TRIANGLE & GEOMETRY (5 tests) ====================
	print_str('--- 50. Triangle & Geometry ---')

	print_int(triangle_area_2x(0, 0, 4, 0, 0, 3)) // 12 (area=6, 2x=12)
	print_int(triangle_area_2x(0, 0, 10, 0, 0, 10)) // 100
	rp := rotate_point_90(Point{ x: 3, y: 4 })
	print_int(rp.x) // -4
	print_int(rp.y) // 3
	print_int(manhattan_dist(Point{ x: 1, y: 2 }, Point{
		x: 4
		y: 6
	})) // 3+4 = 7

	// ==================== 51. DIGITAL ROOT (5 tests) ====================
	print_str('--- 51. Digital Root ---')

	print_int(digital_root(0)) // 0
	print_int(digital_root(5)) // 5
	print_int(digital_root(39)) // 3+9=12, 1+2=3
	print_int(digital_root(999)) // 9+9+9=27, 2+7=9
	print_int(digital_root(12345)) // 1+2+3+4+5=15, 1+5=6

	// ==================== 52. INTERPOLATION & SIGN (5 tests) ====================
	print_str('--- 52. Interpolation & Sign ---')

	print_int(lerp(0, 100, 1, 2)) // 50
	print_int(lerp(10, 20, 3, 10)) // 13
	print_int(sign(42)) // 1
	print_int(sign(0 - 7)) // -1
	print_int(sign(0)) // 0

	// ==================== 53. BIT MANIPULATION (5 tests) ====================
	print_str('--- 53. Bit Manipulation ---')

	print_int(popcount_loop(0)) // 0
	print_int(popcount_loop(7)) // 3 (111)
	print_int(popcount_loop(255)) // 8
	print_int(leading_zeros(1)) // 30 (bit 0 set, 30 zeros before it in 31-bit range)
	print_int(leading_zeros(0)) // 32

	// ==================== 54. CHAINED STRUCT OPERATIONS (5 tests) ====================
	print_str('--- 54. Chained Struct Operations ---')

	// Build point from multiple operations
	p54a := make_point(3, 4)
	p54b := make_point(5, 12)
	p54c := add_points(p54a, p54b)
	print_int(p54c.x) // 8
	print_int(p54c.y) // 16
	// Chain: add two results
	p54d := add_points(add_points(p54a, p54b), make_point(2, 2))
	print_int(p54d.x) // 10
	print_int(p54d.y) // 18
	// Manhattan distance through chained calls
	print_int(manhattan_dist(p54a, p54b)) // |3-5|+|4-12| = 2+8 = 10

	// ==================== 55. GLOBAL ACCUMULATION (5 tests) ====================
	print_str('--- 55. Global Accumulation ---')

	g_val = 0
	g_count = 0
	g_acc = 0
	for ii := 1; ii <= 5; ii++ {
		g_val += ii
		g_count += ii * 2
		g_acc += ii * 3
	}
	print_int(g_val) // 15
	print_int(g_count) // 30
	print_int(g_acc) // 45
	print_int(g_val + g_count + g_acc) // 90
	print_int(g_val * g_val + g_count * g_count + g_acc * g_acc) // 225+900+2025 = 3150

	// ==================== 56. SIEVE OF ERATOSTHENES (simulated) (5 tests) ====================
	print_str('--- 56. Sieve Simulation ---')

	mut sum_primes := 0
	mut last_prime := 0
	mut first_prime := 0
	for ii := 2; ii <= 50; ii++ {
		if is_prime(ii) {
			sum_primes += ii
			last_prime = ii
			if first_prime == 0 {
				first_prime = ii
			}
		}
	}
	print_int(sum_primes) // 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47 = 328
	print_int(last_prime) // 47
	print_int(first_prime) // 2
	mut twin_count := 0
	for ii := 2; ii <= 48; ii++ {
		if is_prime(ii) && is_prime(ii + 2) {
			twin_count++
		}
	}
	print_int(twin_count) // (3,5),(5,7),(11,13),(17,19),(29,31),(41,43) = 6
	assert sum_primes == 328

	// ==================== 57. COMPLEX LOOP PATTERNS (5 tests) ====================
	print_str('--- 57. Complex Loop Patterns ---')

	// Triangular number via nested loop
	mut tri57 := 0
	for ii := 1; ii <= 10; ii++ {
		for jj := 1; jj <= ii; jj++ {
			tri57++
		}
	}
	print_int(tri57) // 1+2+3+...+10 = 55

	// Sum until threshold with early break
	mut sum57 := 0
	mut count57 := 0
	for ii := 1; ii <= 100; ii++ {
		sum57 += ii
		count57++
		if sum57 > 50 {
			break
		}
	}
	print_int(sum57) // 1+2+...+10 = 55 (55 > 50, break at i=10)
	print_int(count57) // 10

	// Double accumulation in single loop
	mut a57 := 0
	mut b57 := 1
	for ii := 0; ii < 10; ii++ {
		tmp57 := a57
		a57 = b57
		b57 = tmp57 + b57
	}
	print_int(a57) // fib(10) = 55
	print_int(b57) // fib(11) = 89

	// ==================== 58. HEAP STRUCT COMPUTATIONS (5 tests) ====================
	print_str('--- 58. Heap Struct Computations ---')

	hv58 := &Vec3{
		x: 10
		y: 20
		z: 30
	}
	print_int(hv58.x * hv58.x + hv58.y * hv58.y + hv58.z * hv58.z) // 100+400+900 = 1400
	hp58 := &Point{
		x: 7
		y: 24
	}
	print_int(hp58.x * hp58.x + hp58.y * hp58.y) // 49+576 = 625
	hm58 := &Matrix2x2{
		a: 2
		b: 0
		c: 0
		d: 2
	}
	print_int(hm58.a * hm58.d - hm58.b * hm58.c) // 4
	hr58 := &Rectangle{
		width:  8
		height: 5
		origin: Point{
			x: 0
			y: 0
		}
	}
	print_int(hr58.width * hr58.height) // 40
	hc58 := &Color{
		r: 100
		g: 150
		b: 200
		a: 255
	}
	print_int((hc58.r + hc58.g + hc58.b) / 3) // (100+150+200)/3 = 150

	// ==================== 59. MULTI-FUNCTION PIPELINE (5 tests) ====================
	print_str('--- 59. Multi-Function Pipeline ---')

	// Chain: abs → clamp → classify
	print_int(classify(clamp(abs_val(0 - 75), 0, 100))) // abs(-75)=75, clamp(75,0,100)=75, classify(75)=2
	print_int(classify(clamp(abs_val(0 - 200), 0, 100))) // abs=200, clamp=100, classify(100)=1 (not >100)

	// Chain: operations → assertions
	mut pipe_sum := 0
	for ii := 1; ii <= 8; ii++ {
		pipe_sum += clamp(ii * ii, 5, 50)
	}
	// 1→5, 4→5, 9→9, 16→16, 25→25, 36→36, 49→49, 64→50
	print_int(pipe_sum) // 5+5+9+16+25+36+49+50 = 195

	// Recursive + iterative agreement
	print_int(fib(10)) // 55
	// Also compute fib iteratively
	mut fa59 := 0
	mut fb59 := 1
	for ii := 0; ii < 10; ii++ {
		tmp59 := fa59
		fa59 = fb59
		fb59 = tmp59 + fb59
	}
	print_int(fa59) // 55 (should match fib(10))

	// ==================== 60. STRESS INTEGRATION (5 tests) ====================
	print_str('--- 60. Stress Integration ---')

	// Combine everything: structs, globals, loops, functions, heap, match, assert
	g_acc = 0
	g_toggle = false
	for ii := 0; ii < 20; ii++ {
		if ii % 3 == 0 {
			g_acc += ii * 2
		} else if ii % 3 == 1 {
			g_acc += ii
		} else {
			g_acc -= 1
		}
		g_toggle = !g_toggle
	}
	// ii%3==0: 0,3,6,9,12,15,18 → 0+6+12+18+24+30+36 = 126
	// ii%3==1: 1,4,7,10,13,16,19 → 1+4+7+10+13+16+19 = 70
	// ii%3==2: 2,5,8,11,14,17 → -1*6 = -6
	// total: 126+70-6 = 190
	print_int(g_acc) // 190
	if g_toggle {
		print_int(1)
	} else {
		print_int(0)
	} // 20 iterations, toggle starts false, ends false → 0

	// Nested struct mutation in loop with conditionals
	mut pts60 := Point{
		x: 0
		y: 0
	}
	for ii := 1; ii <= 10; ii++ {
		if ii % 2 == 0 {
			translate_point(mut pts60, ii, 0)
		} else {
			translate_point(mut pts60, 0, ii)
		}
	}
	// x: 2+4+6+8+10 = 30
	// y: 1+3+5+7+9 = 25
	print_int(pts60.x) // 30
	print_int(pts60.y) // 25

	// Final verification with multiple algorithms
	assert fib(10) == 55
	assert factorial(7) == 5040
	assert gcd(48, 18) == 6
	assert power(2, 10) == 1024
	assert is_prime(97) == true
	assert isqrt(144) == 12
	assert reverse_int(1234) == 4321
	assert digital_root(9999) == 9
	print_str('All stress assertions passed')

	print_str('=== ALL 60 TESTS PASSED ===')

	// ==================== 61. METHODS (8 tests) ====================
	print_str('--- 61. Methods ---')

	// 61.1 Basic method call
	mp1 := Point{
		x: 10
		y: 20
	}
	print_int(mp1.sum()) // 30

	// 61.2 Method with multiplication
	mp2 := Point{
		x: 5
		y: 6
	}
	print_int(mp2.product()) // 30

	// 61.3 Mutable receiver method
	mut mp3 := Point{
		x: 7
		y: 8
	}
	mp3.double()
	print_int(mp3.x) // 14
	print_int(mp3.y) // 16

	// 61.4 Rectangle methods
	mr1 := Rectangle{
		width:  10
		height: 5
		origin: Point{
			x: 0
			y: 0
		}
	}
	print_int(mr1.area()) // 50
	print_int(mr1.perimeter()) // 30

	// 61.5 Node method
	mn1 := Node{
		value: 100
		left:  10
		right: 20
	}
	print_int(mn1.total()) // 130

	// 61.6 Method on heap-allocated struct
	mhp := &Point{
		x: 4
		y: 5
	}
	print_int(mhp.sum()) // 9
	print_int(mhp.product()) // 20

	// ==================== 62. IF-EXPRESSIONS (7 tests) ====================
	print_str('--- 62. If-Expressions ---')

	// 62.1 Basic if-expression (abs)
	print_int(int_abs(-5)) // 5
	print_int(int_abs(7)) // 7
	print_int(int_abs(0)) // 0

	// 62.2 If-expression for max
	print_int(int_max2(10, 20)) // 20
	print_int(int_max2(30, 15)) // 30
	print_int(int_max2(5, 5)) // 5

	// 62.3 If-expression for min
	print_int(int_min2(10, 20)) // 10
	print_int(int_min2(30, 15)) // 15
	print_int(int_min2(8, 8)) // 8

	// 62.4 Nested if-expression (sign)
	print_int(sign_expr(-100)) // -1
	print_int(sign_expr(100)) // 1
	print_int(sign_expr(0)) // 0

	// 62.5 Nested if-expression (clamp)
	print_int(clamp_expr(5, 0, 10)) // 5
	print_int(clamp_expr(-5, 0, 10)) // 0
	print_int(clamp_expr(15, 0, 10)) // 10

	// 62.6 If-expression in local variable
	val62 := 25
	result62 := if val62 > 20 { val62 * 2 } else { val62 }
	print_int(result62) // 50

	// 62.7 If-expression with complex condition
	a62 := 10
	b62 := 20
	c62 := if a62 < b62 && b62 < 30 { a62 + b62 } else { 0 }
	print_int(c62) // 30

	// ==================== 63. STRING INTERPOLATION (5 tests) ====================
	print_str('--- 63. String Interpolation ---')

	// 63.1 Basic integer interpolation
	interp_x := 42
	si1 := 'The answer is ${interp_x}'
	print_str(si1) // The answer is 42

	// 63.2 Multiple interpolations
	interp_a := 10
	interp_b := 20
	si2 := '${interp_a} + ${interp_b} = ${interp_a + interp_b}'
	print_str(si2) // 10 + 20 = 30

	// 63.3 String at beginning and end
	interp_val := 100
	si3 := 'Value: ${interp_val}!'
	print_str(si3) // Value: 100!

	// 63.4 Just interpolation
	interp_num := 999
	si4 := '${interp_num}'
	print_str(si4) // 999

	// 63.5 Multiple consecutive values
	iv1 := 1
	iv2 := 2
	iv3 := 3
	si5 := '${iv1}-${iv2}-${iv3}'
	print_str(si5) // 1-2-3

	// 63.6 String concatenation with + operator
	str_a := 'Hello'
	str_b := ' World'
	str_c := str_a + str_b
	print_str(str_c) // Hello World

	// 63.7 Chained string concatenation
	str_chain := 'A' + 'B' + 'C'
	print_str(str_chain) // ABC

	// ==================== 64. FOR-IN RANGE (7 tests) ====================
	print_str('--- 64. For-In Range ---')

	// 64.1 Basic for-in range
	mut forin_sum1 := 0
	for i in 0 .. 5 {
		forin_sum1 += i
	}
	print_int(forin_sum1) // 10

	// 64.2 Non-zero start
	mut forin_sum2 := 0
	for i in 5 .. 10 {
		forin_sum2 += i
	}
	print_int(forin_sum2) // 35

	// 64.3 Range with variable bounds
	forin_start := 2
	forin_end := 6
	mut forin_sum3 := 0
	for i in forin_start .. forin_end {
		forin_sum3 += i
	}
	print_int(forin_sum3) // 14

	// 64.4 Nested for-in ranges
	mut forin_count := 0
	for i in 0 .. 3 {
		for j in 0 .. 4 {
			forin_count += i + j + 1
		}
	}
	print_int(forin_count) // 42

	// 64.5 For-in with computation
	mut forin_product := 1
	for i in 1 .. 6 {
		forin_product *= i
	}
	print_int(forin_product) // 120

	// 64.6 For-in with break
	mut forin_sum4 := 0
	for i in 0 .. 100 {
		if i >= 5 {
			break
		}
		forin_sum4 += i
	}
	print_int(forin_sum4) // 10

	// 64.7 For-in with continue
	mut forin_sum5 := 0
	for i in 0 .. 10 {
		if i % 2 == 0 {
			continue
		}
		forin_sum5 += i
	}
	print_int(forin_sum5) // 25

	// ==================== 65. ENUMS (5 tests) ====================
	print_str('--- 65. Enums ---')

	// 65.1 Basic enum values
	d1 := Direction.up
	print_int(int(d1)) // 0
	d2 := Direction.right
	print_int(int(d2)) // 3

	// 65.2 Enum in match
	match d1 {
		.up { print_int(10) }
		.down { print_int(20) }
		.left { print_int(30) }
		.right { print_int(40) }
	}

	// 65.3 Enum comparison
	if d2 == Direction.right {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 65.4 Enum in if-else chain
	d3 := Direction.left
	if d3 == Direction.up {
		print_int(100)
	} else if d3 == Direction.down {
		print_int(200)
	} else if d3 == Direction.left {
		print_int(300) // 300
	} else {
		print_int(400)
	}

	// ==================== 66. DEFER STATEMENTS (3 tests) ====================
	print_str('--- 66. Defer Statements ---')

	// 66.1 Defer executes before return
	g_acc = 0
	print_int(defer_return_test()) // 42
	print_int(g_acc) // 100

	// 66.2 Multiple defers in LIFO order
	g_acc = 0
	defer_order_test()
	print_int(g_acc) // 111 (100 + 10 + 1)

	// 66.3 Defer in main
	g_acc = 0
	defer {
		g_acc += 50
	}

	// ==================== 67. UNARY OPERATIONS ====================
	print_str('--- 67. Unary Operations ---')

	un1 := !false
	if un1 {
		print_int(1)
	} else {
		print_int(0)
	}
	un2 := !true
	if un2 {
		print_int(1)
	} else {
		print_int(0)
	}
	un3 := !!true
	if un3 {
		print_int(1)
	} else {
		print_int(0)
	}
	un4 := !(5 > 10)
	if un4 {
		print_int(1)
	} else {
		print_int(0)
	}
	un5 := false
	un6 := !un5
	if un6 {
		print_int(1)
	} else {
		print_int(0)
	}

	// ==================== 68. COMPLEX BOOLEAN ====================
	print_str('--- 68. Complex Boolean ---')

	if true && true && true {
		print_int(1)
	} else {
		print_int(0)
	}
	if false || false || true {
		print_int(1)
	} else {
		print_int(0)
	}
	if (true && false) || (true && true) {
		print_int(1)
	} else {
		print_int(0)
	}
	cb_a := 10
	cb_b := 20
	cb_c := 30
	if cb_a < cb_b && cb_b < cb_c {
		print_int(1)
	} else {
		print_int(0)
	}
	if cb_a < 15 && cb_b > 15 && cb_c == 30 {
		print_int(1)
	} else {
		print_int(0)
	}

	// ==================== 69. COMPARISON AS EXPRESSION ====================
	print_str('--- 69. Comparison as Expression ---')

	cmp1 := 10 > 5
	if cmp1 {
		print_int(1)
	} else {
		print_int(0)
	}
	cmp2 := 3 < 5
	cmp3 := 7 > 2
	if cmp2 && cmp3 {
		print_int(1)
	} else {
		print_int(0)
	}
	cmp4 := 42 == 42
	if cmp4 {
		print_int(1)
	} else {
		print_int(0)
	}
	cmp5 := 10 != 20
	if cmp5 {
		print_int(1)
	} else {
		print_int(0)
	}
	cmp6 := (5 + 5) == (2 * 5)
	if cmp6 {
		print_int(1)
	} else {
		print_int(0)
	}

	// ==================== 70. DEEPLY NESTED IF ====================
	print_str('--- 70. Deeply Nested If ---')

	dn1 := 5
	if dn1 > 0 {
		if dn1 > 3 {
			if dn1 > 4 {
				print_int(1)
			} else {
				print_int(0)
			}
		} else {
			print_int(0)
		}
	} else {
		print_int(0)
	}
	dn2 := 2
	if dn2 == 1 {
		print_int(10)
	} else {
		if dn2 == 2 {
			print_int(20)
		} else {
			if dn2 == 3 {
				print_int(30)
			} else {
				print_int(0)
			}
		}
	}
	dn3 := 3
	if dn3 > 0 {
		match dn3 {
			1 { print_int(100) }
			2 { print_int(200) }
			3 { print_int(300) }
			else { print_int(0) }
		}
	} else {
		print_int(0)
	}
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
	print_int(dn4_sum) // 3
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

	// ==================== 71. LARGE CONSTANTS ====================
	print_str('--- 71. Large Constants ---')

	big1 := 100000
	print_int(big1)
	big2 := 1000 * 1000
	print_int(big2)
	big3 := 50000 + 50000
	print_int(big3)
	big4 := 200000 - 100000
	print_int(big4)
	big5 := 1000000 / 100
	print_int(big5)

	// ==================== 72. MIXED OPERATIONS ====================
	print_str('--- 72. Mixed Operations ---')

	mix1 := (10 + 5) * 2
	if mix1 == 30 {
		print_int(1)
	} else {
		print_int(0)
	}
	mix2a := 10 > 5
	mix2b := 20 < 30
	if mix2a && mix2b {
		print_int(1)
	} else {
		print_int(0)
	}
	mix3 := 2 + 3 * 4 - 6 / 2
	print_int(mix3) // 11
	mix4 := (5 | 3) + (4 & 6)
	print_int(mix4) // 11
	mix5 := ((10 + 5) * 2 - 10) / 5
	print_int(mix5) // 4

	// ==================== 73. EDGE CASES ====================
	print_str('--- 73. Edge Cases ---')

	edge1 := 0 + 0
	print_int(edge1)
	edge2 := 100 * 0
	print_int(edge2)
	edge3 := 0 / 7
	print_int(edge3)
	edge4 := 42 + 0
	print_int(edge4)
	edge5 := 42 * 1
	print_int(edge5)
	edge6 := 42 / 1
	print_int(edge6)
	edge7 := 7 / 7
	print_int(edge7)
	edge8 := 100 / 10
	print_int(edge8)
	edge9 := 10 % 10
	print_int(edge9)
	edge10 := 5 % 7
	print_int(edge10)
	if 0 == 0 {
		print_int(1)
	} else {
		print_int(0)
	}
	if 0 < 1 {
		print_int(1)
	} else {
		print_int(0)
	}

	// ==================== 74. COMPLEX RECURSION ====================
	print_str('--- 74. Complex Recursion ---')

	print_int(sum_recursive(100)) // 5050
	print_int(gcd(252, 105)) // 21
	print_int(fib(15)) // 610
	print_int(power(3, 5)) // 243
	print_int(factorial(7)) // 5040

	// ==================== 75. STRUCT OPERATIONS ====================
	print_str('--- 75. Struct Operations ---')

	mut sp1 := Point{
		x: 0
		y: 0
	}
	scale_point(mut sp1, 10)
	print_int(sp1.x) // 0
	print_int(sp1.y) // 0
	mut sp2 := Point{
		x: 1
		y: 1
	}
	scale_point(mut sp2, 5)
	translate_point(mut sp2, 10, 20)
	print_int(sp2.x) // 15
	print_int(sp2.y) // 25
	sp3 := Point{
		x: 100
		y: 200
	}
	sp3_sum := sp3.x + sp3.y
	sp3_diff := sp3.y - sp3.x
	print_int(sp3_sum) // 300
	print_int(sp3_diff) // 100
	mut sp4 := Point{
		x: 10
		y: 20
	}
	swap_point(mut sp4)
	scale_point(mut sp4, 2)
	print_int(sp4.x) // 40
	print_int(sp4.y) // 20
	sp5 := Point{
		x: 5
		y: 10
	}
	if sp5.x < sp5.y {
		print_int(1)
	} else {
		print_int(0)
	}

	// ==================== 76. CONTROL FLOW EDGE CASES ====================
	print_str('--- 76. Control Flow Edge Cases ---')

	mut cf1 := 0
	if true {
		cf1 = 1
	}
	print_int(cf1) // 1
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
				2 { print_int(22) }
				else { print_int(20) }
			}
		}
		else {
			print_int(0)
		}
	}

	mut cf4 := 0
	mut cf4_i := 0
	for cf4_i < 20 && cf4 < 50 {
		cf4 += cf4_i
		cf4_i++
	}
	print_int(cf4) // 55
	print_int(nested_return(5)) // 100
	print_int(nested_return(15)) // 200
	print_int(nested_return(25)) // 300
	mut cf5 := 0
	mut cf5_i := 0
	for cf5_i < 3 || cf5 < 10 {
		cf5 += 5
		cf5_i++
	}
	print_int(cf5) // 15

	// ==================== 77. ARRAY INITIALIZATION ====================
	print_str('--- 77. Array Initialization ---')

	arr1 := [10, 20, 30]
	print_int(arr1[0]) // 10
	print_int(arr1[1]) // 20
	print_int(arr1[2]) // 30
	arr2 := [5, 10, 15]
	arr2_sum := arr2[0] + arr2[1] + arr2[2]
	print_int(arr2_sum) // 30
	base_val := 7
	arr3 := [base_val, base_val * 2, base_val * 3]
	print_int(arr3[0]) // 7
	print_int(arr3[1]) // 14
	print_int(arr3[2]) // 21
	arr4 := [100, 200, 300]
	result4 := arr4[0] * 2 + arr4[1]
	print_int(result4) // 400
	arr5 := [3, 4, 5]
	print_int(add(arr5[0], arr5[1])) // 7
	print_int(mul(arr5[1], arr5[2])) // 20

	// ==================== 78. FOR-IN ARRAY ====================
	print_str('--- 78. For-In Array ---')

	arr_iter1 := [10, 20, 30]
	mut sum_iter1 := 0
	for elem in arr_iter1 {
		sum_iter1 += elem
	}
	print_int(sum_iter1) // 60
	arr_iter2 := [5, 10, 15]
	mut weighted_sum2 := 0
	for i, elem in arr_iter2 {
		weighted_sum2 += (i + 1) * elem
	}
	print_int(weighted_sum2) // 70
	arr_iter3 := [1, 2, 3, 4, 5]
	mut sum_iter3 := 0
	for elem in arr_iter3 {
		if elem > 3 {
			break
		}
		sum_iter3 += elem
	}
	print_int(sum_iter3) // 6
	arr_iter4 := [1, 2, 3, 4, 5]
	mut sum_iter4 := 0
	for elem in arr_iter4 {
		if elem % 2 == 0 {
			continue
		}
		sum_iter4 += elem
	}
	print_int(sum_iter4) // 9
	arr_outer := [1, 2, 3]
	arr_inner := [10, 20]
	mut nested_sum := 0
	for outer in arr_outer {
		for inner in arr_inner {
			nested_sum += outer * inner
		}
	}
	print_int(nested_sum) // 180

	// ==================== 79. FIXED SIZE ARRAYS ====================
	print_str('--- 79. Fixed Size Arrays ---')

	fixed_arr1 := [5, 10, 15]
	print_int(fixed_arr1[0]) // 5
	print_int(fixed_arr1[1]) // 10
	print_int(fixed_arr1[2]) // 15
	fa_idx := 1
	print_int(fixed_arr1[fa_idx]) // 10
	mut fixed_sum := 0
	for elem in fixed_arr1 {
		fixed_sum += elem
	}
	print_int(fixed_sum) // 30
	fixed_arr2 := [1, 2, 3, 4, 5]
	mut fixed_product := 1
	for elem in fixed_arr2 {
		fixed_product *= elem
	}
	print_int(fixed_product) // 120
	fixed_outer := [100, 200, 300]
	fixed_inner := [1, 2, 3]
	print_int(fixed_outer[0] + fixed_inner[2]) // 103
	fixed_literal := [1, 2, 3, 4, 5]!
	print_int(fixed_literal[0]) // 1
	print_int(fixed_literal[4]) // 5
	print_int(fixed_literal.len) // 5
	mut fixed_literal_sum := 0
	for elem in fixed_literal {
		fixed_literal_sum += elem
	}
	print_int(fixed_literal_sum) // 15

	// ==================== 80. STRING STRUCT FIELDS ====================
	print_str('--- 80. String Struct Fields ---')

	s80_1 := 'Hello'
	print_str(s80_1) // Hello
	s80_2 := 'World'
	print_int(s80_2.len) // 5
	a80 := 'AB'
	b80 := 'CDE'
	print_int(a80.len + b80.len) // 5
	print_str('Passed directly')

	// ==================== 81. STRUCT FIELD OPERATIONS ====================
	print_str('--- 81. Struct Field Operations ---')

	mut sf1 := Point{
		x: 10
		y: 20
	}
	sf1.x = sf1.x + 5
	sf1.y = sf1.y - 3
	print_int(sf1.x) // 15
	print_int(sf1.y) // 17
	mut sf2 := Point{
		x: 6
		y: 100
	}
	sf2.x = sf2.x * 7
	sf2.y = sf2.y / 4
	print_int(sf2.x) // 42
	print_int(sf2.y) // 25
	mut sf3 := Point{
		x: 50
		y: 30
	}
	sf3.x += 25
	sf3.y -= 10
	print_int(sf3.x) // 75
	print_int(sf3.y) // 20
	mut sf4 := Point{
		x: 8
		y: 64
	}
	sf4.x *= 5
	sf4.y /= 8
	print_int(sf4.x) // 40
	print_int(sf4.y) // 8
	mut sf5 := Point{
		x: 3
		y: 4
	}
	sf5.x = sf5.x + sf5.y
	sf5.y = sf5.x * sf5.y
	print_int(sf5.x) // 7
	print_int(sf5.y) // 28
	mut sf6 := Point{
		x: 2
		y: 3
	}
	sf6.x = sf6.x * 2
	sf6.x = sf6.x + 1
	sf6.x = sf6.x * 3
	sf6.y = sf6.y + sf6.x
	print_int(sf6.x) // 15
	print_int(sf6.y) // 18
	mut sf7 := Point{
		x: 17
		y: 23
	}
	sf7.x = sf7.x % 5
	sf7.y = sf7.y % 7
	print_int(sf7.x) // 2
	print_int(sf7.y) // 2
	mut sf8 := Point{
		x: 0b1100
		y: 0b1010
	}
	sf8.x = sf8.x & sf8.y
	sf8.y = sf8.x | 0b0101
	print_int(sf8.x) // 8
	print_int(sf8.y) // 13
	mut sf9 := Point{
		x: 5
		y: 10
	}
	sf9.x = add(sf9.x, sf9.y)
	sf9.y = mul(sf9.x, 2)
	print_int(sf9.x) // 15
	print_int(sf9.y) // 30
	mut rect_mod := Rectangle{
		width:  10
		height: 20
		origin: Point{
			x: 0
			y: 0
		}
	}
	rect_mod.width = rect_mod.width * 2
	rect_mod.height += 5
	rect_mod.origin.x = 100
	rect_mod.origin.y = rect_mod.origin.x / 2
	print_int(rect_mod.width) // 20
	print_int(rect_mod.height) // 25
	print_int(rect_mod.origin.x) // 100
	print_int(rect_mod.origin.y) // 50

	// ==================== 82. PRINTLN ====================
	print_str('--- 82. Println ---')

	println('hello world')

	// ==================== 83. ALGEBRAIC OPTIMIZATIONS ====================
	print_str('--- 83. Algebraic Optimizations ---')

	opt_val := 42
	print_int(opt_val - opt_val) // 0
	opt_xor := 123
	print_int(opt_xor ^ opt_xor) // 0
	opt_and := 99
	print_int(opt_and & opt_and) // 99
	opt_or := 77
	print_int(opt_or | opt_or) // 77
	opt_mul2 := 25
	print_int(opt_mul2 * 2) // 50
	opt_a2 := 10
	opt_b2 := opt_a2 - opt_a2
	opt_c2 := opt_a2 | opt_a2
	print_int(opt_b2) // 0
	print_int(opt_c2) // 10
	opt_mul2_comm := 13
	print_int(2 * opt_mul2_comm) // 26
	opt_expr := 7
	print_int((opt_expr ^ opt_expr) + 5) // 5
	print_int((opt_expr & opt_expr) * 2) // 14
	opt_large := 12345
	print_int(opt_large - opt_large) // 0
	print_int(opt_large ^ opt_large) // 0
	print_int(opt_large & opt_large) // 12345
	print_int(opt_large | opt_large) // 12345
	mut opt_loop_sum := 0
	for i in 1 .. 5 {
		opt_loop_sum += i - i
		opt_loop_sum += i & i
	}
	print_int(opt_loop_sum) // 10

	// ==================== 84. DEAD STORE ELIMINATION ====================
	print_str('--- 84. Dead Store Elimination ---')

	{
		mut dead_var := 100
		dead_var = 200
		_ = dead_var
	}
	print_int(1)
	mut dse_var := 10
	dse_var = 20
	dse_var = 30
	print_int(dse_var) // 30
	mut dse_multi := 1
	dse_multi = 2
	dse_multi = 3
	dse_multi = 4
	dse_multi = 5
	print_int(dse_multi) // 5
	mut dse_branch := 100
	if false {
		dse_branch = 999
	}
	print_int(dse_branch) // 100
	mut dse_live := 50
	if true {
		dse_live = 75
	}
	print_int(dse_live) // 75

	// ==================== 85. GOTO STATEMENT ====================
	print_str('--- 85. Goto Statement ---')

	mut counter75 := 0
	mut iterations75 := 0
	start75:
	counter75++
	iterations75++
	if counter75 < 3 {
		unsafe {
			goto start75
		}
	}
	print_int(iterations75) // 3
	mut skipped75 := false
	unsafe {
		goto skip75
	}
	skipped75 = true
	skip75:
	if skipped75 {
		print_int(0)
	} else {
		print_int(1) // 1
	}

	// ==================== 86. STRING MATCH RETURN ====================
	print_str('--- 86. String Match Return ---')

	print_str(operator_to_name('+')) // __plus
	print_str(operator_to_name('-')) // __minus
	print_str(operator_to_name('*')) // __mul
	print_str(operator_to_name('?')) // ?

	// ==================== 87. RETURN IF EXPRESSION ====================
	print_str('--- 87. Return If Expression ---')

	test_return_if_expr()

	// ==================== 88. OR BLOCKS AND OPTIONAL ====================
	print_str('--- 88. Or Blocks and Optional ---')

	val1 := try_get_value(true) or { 0 }
	print_int(val1) // 42
	val2 := try_get_value(false) or { 99 }
	print_int(val2) // 99
	val3 := try_get_value(true) or { panic('should not happen') }
	print_int(val3) // 42
	try_get_value(true) or { panic('should not reach') }
	try_get_value(false) or {
		print_int(7) // 7
	}
	print_str('or blocks: ok')

	// ==================== 89. IF GUARD (OPTIONAL UNWRAP) ====================
	print_str('--- 89. If Guard (Optional Unwrap) ---')

	if val := try_get_value(true) {
		print_int(val) // 42
	} else {
		print_int(0)
	}
	if val := try_get_value(false) {
		print_int(0)
	} else {
		print_int(99) // 99
	}
	// nested if-guard
	if a := try_get_value(true) {
		if b := try_get_value(true) {
			print_int(a + b) // 84
		}
	}
	// if-guard without else
	if val := try_get_value(true) {
		print_int(val + 8) // 50
	}
	// if-guard with false, no else — should skip body
	mut guard_ran := false
	if val := try_get_value(false) {
		guard_ran = true
		print_int(val)
	}
	if !guard_ran {
		print_int(1) // 1
	}
	print_str('if guard: ok')

	// ==================== 90. MAPS ====================
	print_str('--- 90. Maps ---')

	mut m := map[string]int{}
	m['one'] = 1
	m['two'] = 2
	m['three'] = 3
	print_int(m['one']) // 1
	print_int(m['two']) // 2
	print_int(m['three']) // 3
	print_int(m.len) // 3
	// if-guard with map lookup
	if val := m['two'] {
		print_int(val) // 2
	} else {
		print_int(0)
	}
	if val := m['missing'] {
		print_int(val)
	} else {
		print_int(99) // 99
	}
	// map with int keys
	mut mi := map[int]int{}
	mi[10] = 100
	mi[20] = 200
	print_int(mi[10]) // 100
	print_int(mi[20]) // 200
	// map init with values
	mut ml := map[string]int{}
	ml['x'] = 10
	ml['y'] = 20
	print_int(ml['x']) // 10
	print_int(ml['y']) // 20
	print_str('maps: ok')

	// ==================== 91. STRING METHODS ====================
	print_str('--- 91. String Methods ---')

	s91 := 'hello world'
	if s91.starts_with('hello') {
		print_int(1) // 1
	}
	if s91.starts_with('world') == false {
		print_int(2) // 2
	}
	if s91.ends_with('world') {
		print_int(3) // 3
	}
	if s91.ends_with('hello') == false {
		print_int(4) // 4
	}
	if s91.contains('lo wo') {
		print_int(5) // 5
	}
	if s91.contains('xyz') == false {
		print_int(6) // 6
	}
	print_int(s91.index_u8(111)) // 4 (111 = 'o')
	print_int(s91.last_index_u8(111)) // 7
	print_str(s91.substr(0, 5)) // hello
	print_str(s91.substr(6, 11)) // world
	print_str(s91.all_before(' ')) // hello
	print_str(s91.all_after(' ')) // world
	path91 := '/Users/test/file.v'
	print_str(path91.all_before_last('/')) // /Users/test
	print_str(path91.all_after_last('/')) // file.v
	spaced := '  hello  '
	print_str(spaced.trim_space()) // hello
	print_str('  abc'.trim_left(' ')) // abc
	print_str('abc  '.trim_right(' ')) // abc
	numstr := '42'
	print_int(numstr.int()) // 42
	print_str('string methods: ok')

	// ==================== 92. DYNAMIC ARRAYS ====================
	print_str('--- 92. Dynamic Arrays ---')

	mut arr := []int{}
	arr << 10
	arr << 20
	arr << 30
	print_int(arr.len) // 3
	print_int(arr[0]) // 10
	print_int(arr[1]) // 20
	print_int(arr[2]) // 30
	// for-in dynamic array
	mut sum92 := 0
	for x in arr {
		sum92 = sum92 + x
	}
	print_int(sum92) // 60
	// for-in with index
	for i, x in arr {
		if i == 1 {
			print_int(x) // 20
		}
	}
	// string array
	mut names := []string{}
	names << 'hello'
	names << 'world'
	print_int(names.len) // 2
	print_str(names[0]) // hello
	print_str(names[1]) // world
	print_str('dynamic arrays: ok')

	print_str('--- 93. Array Methods, Slicing, Split ---')

	// array clone
	mut ac := []int{}
	ac << 1
	ac << 2
	ac << 3
	mut ac2 := ac.clone()
	ac2 << 4
	print_int(ac.len) // 3
	print_int(ac2.len) // 4

	// array last/first
	print_int(ac.first()) // 1
	print_int(ac.last()) // 3

	// array delete_last
	mut ad := []int{}
	ad << 10
	ad << 20
	ad << 30
	ad.delete_last()
	print_int(ad.len) // 2
	print_int(ad.last()) // 20

	// array delete (by index)
	mut ae := []int{}
	ae << 100
	ae << 200
	ae << 300
	ae.delete(0)
	print_int(ae.len) // 2
	print_int(ae[0]) // 200

	// array clear
	mut af := []int{}
	af << 1
	af << 2
	af.clear()
	print_int(af.len) // 0

	// array contains
	mut ag := []int{}
	ag << 5
	ag << 10
	ag << 15
	if ag.contains(10) {
		print_str('contains 10: yes')
	}
	if ag.contains(7) == false {
		print_str('contains 7: no')
	}

	// string slicing
	s93 := 'hello world'
	print_str(s93[0..5]) // hello
	print_str(s93[6..]) // world
	print_str(s93[..5]) // hello

	// array slicing
	mut as93 := []int{}
	as93 << 10
	as93 << 20
	as93 << 30
	as93 << 40
	as93 << 50
	sl := as93[1..4]
	print_int(sl.len) // 3
	print_int(sl[0]) // 20
	print_int(sl[1]) // 30

	// string split
	parts := 'a,b,c'.split(',')
	print_int(parts.len) // 3
	print_str(parts[0]) // a
	print_str(parts[1]) // b
	print_str(parts[2]) // c

	// string replace
	r93 := 'hello world'.replace('world', 'v3')
	print_str(r93) // hello v3

	// map delete
	mut md := map[string]int{}
	md['x'] = 10
	md['y'] = 20
	md.delete('x')
	print_int(md.len) // 1

	print_str('array methods, slicing, split: ok')

	print_str('--- 94. Map Iteration, Array Init with len ---')

	// for k, v in map
	mut m94 := map[string]int{}
	m94['a'] = 1
	m94['b'] = 2
	m94['c'] = 3
	mut sum94 := 0
	for _, v in m94 {
		sum94 = sum94 + v
	}
	print_int(sum94) // 6

	// for v in map (value only)
	mut count94 := 0
	for _, v in m94 {
		count94 = count94 + v
	}
	print_int(count94) // 6

	// array init with len
	a94 := []int{len: 5}
	print_int(a94.len) // 5
	print_int(a94[0]) // 0 (zero-initialized)

	// array init with len and init
	a94b := []int{len: 3, init: 42}
	print_int(a94b.len) // 3
	print_int(a94b[0]) // 42
	print_int(a94b[2]) // 42

	print_str('map iteration, array init: ok')

	print_str('--- 95. In Operator, Array Join ---')

	// in operator for maps
	mut m95 := map[string]int{}
	m95['hello'] = 1
	m95['world'] = 2
	if 'hello' in m95 {
		print_str('hello in map: yes')
	}
	if 'missing' !in m95 {
		print_str('missing not in map: yes')
	}

	// in operator for arrays
	mut a95 := []int{}
	a95 << 10
	a95 << 20
	a95 << 30
	if 20 in a95 {
		print_str('20 in array: yes')
	}
	if 99 !in a95 {
		print_str('99 not in array: yes')
	}

	// array join
	mut words := []string{}
	words << 'hello'
	words << 'world'
	words << 'v3'
	print_str(words.join(', ')) // hello, world, v3
	print_str(words.join(' ')) // hello world v3

	print_str('in operator, array join: ok')

	// 96. strings.Builder
	mut sb := strings.new_builder(100)
	sb.write_string('hello')
	sb.write_string(' ')
	sb.write_string('world')
	result96 := sb.str()
	print_str(result96) // hello world

	mut sb2 := strings.new_builder(50)
	sb2.writeln('line1')
	sb2.writeln('line2')
	sb2.write_string('line3')
	print_str(sb2.str()) // line1\nline2\nline3

	print_str('strings.Builder: ok')

	// 97. Static methods
	f97 := Foo97.new(10, 20)
	assert f97.x == 10
	assert f97.y == 20

	f97b := Foo97.with_name('test', 42)
	assert f97b.name == 'test'
	assert f97b.val == 42

	print_str('static methods: ok')

	// 98. @FILE compile-time constant
	file_path := @FILE
	assert file_path.len > 0
	assert file_path.contains('test_all_lang_features.v')
	print_str('@FILE: ok')

	// 99. Unsafe blocks
	mut ux := 10
	unsafe {
		ux = 42
	}
	assert ux == 42

	uval := unsafe { 100 }
	assert uval == 100

	print_str('unsafe blocks: ok')

	// 100. Function pointers
	r100 := apply_op(add, 3, 4)
	assert r100 == 7

	r100b := apply_op(multiply, 5, 6)
	assert r100b == 30

	print_str('function pointers: ok')

	// 101. Sum type smartcasting (ident and selector)
	cat101 := Cat{
		name: 'Whiskers'
		age:  5
	}
	dog101 := Dog{
		name:   'Rex'
		tricks: 3
	}
	a101 := Animal(cat101)
	if a101 is Cat {
		assert a101.name == 'Whiskers'
		assert a101.age == 5
	}

	h101 := Holder{
		pet: Animal(cat101)
	}
	assert describe_holder(h101) == 'Whiskers'
	assert holder_detail(h101) == 5

	h102 := Holder{
		pet: Animal(dog101)
	}
	assert describe_holder(h102) == 'Rex'
	assert holder_detail(h102) == 3

	// selector smartcast via if
	if h101.pet is Cat {
		assert h101.pet.name == 'Whiskers'
		assert h101.pet.age == 5
	}

	// selector smartcast via match
	match h102.pet {
		Dog {
			assert h102.pet.tricks == 3
		}
		Cat {
			assert false
		}
	}

	print_str('sum type smartcast: ok')

	print_str('--- 102. In Operator LHS Evaluation ---')
	g_count = 0
	if next_in_value() in 0..10 {
		print_str('range side effect: yes')
	}
	print_int(g_count) // 1

	g_count = 0
	if next_in_value() in [2, 3, 4] {
		print_str('inline side effect: unexpected')
	} else {
		print_str('inline side effect: no')
	}
	print_int(g_count) // 1

	g_count = 0
	if next_in_value() !in [2, 3, 4] {
		print_str('not in side effect: yes')
	}
	print_int(g_count) // 1
	print_str('in operator lhs eval: ok')

	print_str('--- 103. Transformer Semantic Lowering ---')
	print_str('interp ${7} ${true}')

	mut range_sum103 := 0
	for i in 1 .. 4 {
		range_sum103 = range_sum103 + i
	}
	print_int(range_sum103) // 6

	mut str_sum103 := 0
	for i, ch in 'abc' {
		str_sum103 = str_sum103 + i + int(ch)
	}
	print_int(str_sum103) // 297

	fixed103 := [3, 4, 5]
	mut fixed_sum103 := 0
	for x in fixed103 {
		fixed_sum103 = fixed_sum103 + x
	}
	print_int(fixed_sum103) // 12

	or_ok103 := maybe_review_value(true) or { 0 }
	or_else103 := maybe_review_value(false) or { 9 }
	print_int(or_ok103) // 42
	print_int(or_else103) // 9
	if guard103 := maybe_review_value(true) {
		print_int(guard103) // 42
	} else {
		print_int(0)
	}

	mut map103 := map[string]int{}
	map103['one'] = 1
	map103['two'] = 2
	map103['three'] = 3
	print_int(map103['one'] + map103['three']) // 4
	if 'two' in map103 {
		print_str('map membership lowered: yes')
	}

	arr103 := []int{len: 3, init: 5}
	print_int(arr103[0] + arr103[2]) // 10

	animal103 := make_review_animal()
	if animal103 is Cat {
		cat103 := animal103 as Cat
		print_str(cat103.name) // Milo
	}
	print_str('transformer semantic lowering: ok')

	print_str('--- 104. Review Regression Lowering ---')

	mut map104 := map[string]int{}
	map104['a'] = 1
	map104['a'] += 2
	map104['a']++
	map104['a'] -= 1
	map104['a']--
	print_int(map104['a']) // 2

	mut string_map104 := map[string]string{}
	string_map104['name'] = 'v'
	string_map104['name'] += '3'
	print_str(string_map104['name']) // v3

	mut array_map104 := map[string][]int{}
	array_map104['nums'] << 4
	array_map104['nums'] << 5
	print_int(array_map104['nums'].len) // 2

	mut int_map104 := map[int]int{}
	int_map104[2] = 40
	int_map104[3] = 60
	print_int(int_map104[2] + int_map104[3]) // 100

	mut holder104 := Holder{
		pet: Animal(Cat{
			name: 'Kit'
			age:  6
		})
	}
	holder104.pet = Animal(Dog{
		name:   'Bolt'
		tricks: 8
	})
	print_str(describe_holder(holder104)) // Bolt
	print_int(holder_detail(holder104)) // 8

	print_int(optional_arg_int104(7)) // 7
	print_int(optional_arg_int104(none)) // -1
	print_int(optional_arg_point104(Point{
		x: 3
		y: 4
	})) // 7
	print_int(optional_arg_point104(none)) // -1

	id104 := UserId104(41)
	print_int(id104.next()) // 42
	print_str('review regression lowering: ok')

	print_str('--- 105. Self-Hosting Features ---')

	// 105.1 Multi-smartcast in && chains: both sides smartcast
	a105 := Animal(Cat{
		name: 'Luna'
		age:  3
	})
	b105 := Animal(Dog{
		name:   'Max'
		tricks: 7
	})
	if a105 is Cat && b105 is Dog {
		assert a105.name == 'Luna'
		assert a105.age == 3
		assert b105.name == 'Max'
		assert b105.tricks == 7
		print_int(a105.age + b105.tricks) // 10
	} else {
		assert false
	}

	// 105.2 Multi-smartcast: same type on both sides
	c105 := Animal(Cat{
		name: 'Mimi'
		age:  2
	})
	d105 := Animal(Cat{
		name: 'Neko'
		age:  9
	})
	if c105 is Cat && d105 is Cat {
		print_int(c105.age + d105.age) // 11
	}

	// 105.3 Multi-smartcast: first matches, second doesn't
	e105 := Animal(Cat{
		name: 'Cleo'
		age:  4
	})
	f105 := Animal(Cat{
		name: 'Felix'
		age:  6
	})
	mut took_else105 := false
	if e105 is Cat && f105 is Dog {
		assert false
	} else {
		took_else105 = true
	}
	assert took_else105

	// 105.4 Map value append via << (map[key] << value) with type inference
	mut suffix_map105 := map[string][]string{}
	suffix_map105['greet'] << 'hello'
	suffix_map105['greet'] << 'hi'
	suffix_map105['part'] << 'world'
	print_int(suffix_map105['greet'].len) // 2
	print_int(suffix_map105['part'].len) // 1
	print_str(suffix_map105['greet'][0]) // hello
	print_str(suffix_map105['part'][0]) // world

	// 105.5 Map value append in a loop (mirrors markused suffix_map pattern)
	mut tag_map105 := map[string][]int{}
	for i in 0 .. 5 {
		key := if i < 3 { 'low' } else { 'high' }
		tag_map105[key] << i
	}
	print_int(tag_map105['low'].len) // 3
	print_int(tag_map105['high'].len) // 2

	// 105.6 Multi-smartcast with field access in expression
	g105 := Animal(Cat{
		name: 'Socks'
		age:  8
	})
	h105 := Animal(Dog{
		name:   'Buddy'
		tricks: 5
	})
	mut desc105 := ''
	if g105 is Cat && h105 is Dog {
		desc105 = g105.name + ' and ' + h105.name
	}
	print_str(desc105) // Socks and Buddy

	print_str('self-hosting features: ok')

	print_str('--- 106. Multi-Return Functions ---')

	// 106.1 Basic multi-return (int, string)
	a106, b106 := two_vals()
	assert a106 == 42
	assert b106 == 'hello'
	print_int(a106) // 42
	print_str(b106) // hello

	// 106.2 Multi-return swap
	c106, d106 := swap_ints(10, 20)
	assert c106 == 20
	assert d106 == 10
	print_int(c106) // 20
	print_int(d106) // 10

	// 106.3 Three-value multi-return
	x106, y106, z106 := three_ints(5)
	assert x106 == 5
	assert y106 == 10
	assert z106 == 15
	print_int(x106 + y106 + z106) // 30

	// 106.4 Multi-return in loop
	mut sum106 := 0
	for i in 0 .. 3 {
		lo, hi := swap_ints(i, i * 10)
		sum106 += lo + hi
	}
	print_int(sum106) // (0+0)+(10+1)+(20+2) = 33

	// 106.5 Multi-return used directly in expression
	a106b, _ := swap_ints(7, 3)
	print_int(a106b) // 3

	print_str('multi-return: ok')

	print_str('--- 107. Struct Update Syntax (Assoc) ---')

	// 107.1 Basic struct update
	p107 := Point{
		x: 10
		y: 20
	}
	p107b := Point{
		...p107
		x: 99
	}
	assert p107b.x == 99
	assert p107b.y == 20
	print_int(p107b.x) // 99
	print_int(p107b.y) // 20

	// 107.2 Update y only
	p107c := Point{
		...p107
		y: 55
	}
	assert p107c.x == 10
	assert p107c.y == 55
	print_int(p107c.x) // 10
	print_int(p107c.y) // 55

	// 107.3 Update both fields (identity check)
	p107d := Point{
		...p107
		x: 1
		y: 2
	}
	assert p107d.x == 1
	assert p107d.y == 2
	print_int(p107d.x + p107d.y) // 3

	// 107.4 Struct update via function return
	p107e := return_assoc_point(Point{ x: 5, y: 15 }, 42)
	assert p107e.x == 42
	assert p107e.y == 15
	print_int(p107e.x) // 42

	// 107.5 Chained struct updates
	base107 := Point{
		x: 100
		y: 200
	}
	step1 := Point{
		...base107
		x: 1
	}
	step2 := Point{
		...step1
		y: 2
	}
	assert step2.x == 1
	assert step2.y == 2
	print_int(step2.x + step2.y) // 3

	print_str('struct update (assoc): ok')

	print_str('--- 108. Array Push Many ---')

	// 108.1 Basic push_many
	mut arr108 := [1, 2]
	arr108b := [3, 4, 5]
	arr108 << arr108b
	assert arr108.len == 5
	assert arr108[3] == 4
	assert arr108[4] == 5
	print_int(arr108.len) // 5

	// 108.2 Push empty array
	mut arr108c := [10, 20]
	empty108 := []int{}
	arr108c << empty108
	assert arr108c.len == 2
	print_int(arr108c.len) // 2

	// 108.3 Push into empty
	mut arr108d := []int{}
	arr108d << [7, 8, 9]
	assert arr108d.len == 3
	assert arr108d[0] == 7
	print_int(arr108d[2]) // 9

	// 108.4 Push many in loop
	mut arr108e := []int{}
	for i in 0 .. 3 {
		arr108e << [i * 10, i * 10 + 1]
	}
	assert arr108e.len == 6
	print_int(arr108e.len) // 6
	print_int(arr108e[4]) // 20

	// 108.5 Push many strings
	mut sarr108 := []string{}
	sarr108 << 'first'
	sarr108 << ['second', 'third']
	assert sarr108.len == 3
	print_str(sarr108[2]) // third

	print_str('array push_many: ok')

	print_str('--- 109. String Comparison Operators ---')

	// 109.1 String less-than
	assert 'apple' < 'banana'
	if 'apple' < 'banana' {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 109.2 String greater-than
	assert 'z' > 'a'
	if 'zoo' > 'abc' {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 109.3 String less-equal
	assert 'abc' <= 'abd'
	assert 'abc' <= 'abc'
	if 'abc' <= 'abc' {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 109.4 String greater-equal
	assert 'xyz' >= 'xyz'
	assert 'b' >= 'a'
	if 'hello' >= 'hello' {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 109.5 String comparison in sorting logic
	s109a := 'cat'
	s109b := 'dog'
	smaller109 := if s109a < s109b { s109a } else { s109b }
	assert smaller109 == 'cat'
	print_str(smaller109) // cat

	print_str('string comparison: ok')

	print_str('--- 110. Struct Operator Overloading ---')

	// 110.1 Struct addition
	pa110 := Point{
		x: 1
		y: 2
	}
	pb110 := Point{
		x: 3
		y: 4
	}
	pc110 := pa110 + pb110
	assert pc110.x == 4
	assert pc110.y == 6
	print_int(pc110.x) // 4
	print_int(pc110.y) // 6

	// 110.2 Struct equality
	assert pc110 == Point{
		x: 4
		y: 6
	}
	assert pa110 != pb110
	if pa110 == Point{
		x: 1
		y: 2
	} {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 110.3 Struct less-than
	assert pa110 < pb110
	if pa110 < pb110 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 110.4 Chained struct addition
	pd110 := pa110 + pb110 + Point{
		x: 10
		y: 20
	}
	assert pd110.x == 14
	assert pd110.y == 26
	print_int(pd110.x) // 14

	// 110.5 Struct ops in loop
	mut acc110 := Point{
		x: 0
		y: 0
	}
	for i in 1 .. 4 {
		acc110 = acc110 + Point{
			x: i
			y: i * 2
		}
	}
	assert acc110.x == 6
	assert acc110.y == 12
	print_int(acc110.x + acc110.y) // 18

	print_str('struct operators: ok')

	print_str('--- 111. Return Match / Match as Expression ---')

	// 111.1 Return match with string
	assert classify_str(0) == 'zero'
	assert classify_str(1) == 'one'
	assert classify_str(2) == 'two'
	assert classify_str(99) == 'other'
	print_str(classify_str(1)) // one

	// 111.2 Match as expression value (string)
	v111 := 3
	label111 := match v111 {
		1 { 'one' }
		2 { 'two' }
		3 { 'three' }
		else { 'unknown' }
	}

	assert label111 == 'three'
	print_str(label111) // three

	// 111.3 Match expression with int
	score111 := 85
	grade111 := match true {
		score111 >= 90 { 'A' }
		score111 >= 80 { 'B' }
		score111 >= 70 { 'C' }
		else { 'F' }
	}

	assert grade111 == 'B'
	print_str(grade111) // B

	// 111.4 Match expression in function call
	print_str(classify_str(0)) // zero

	// 111.5 Nested match expression
	outer111 := 1
	inner111 := 2
	r111 := match outer111 {
		1 {
			match inner111 {
				2 { 'one-two' }
				else { 'one-other' }
			}
		}
		else {
			'other'
		}
	}

	assert r111 == 'one-two'
	print_str(r111) // one-two

	print_str('match expression: ok')

	print_str('--- 112. If Expression Returning String ---')

	// 112.1 Basic if-expr string
	s112a := if true { 'yes' } else { 'no' }
	assert s112a == 'yes'
	print_str(s112a) // yes

	// 112.2 If-expr string with condition
	val112 := 42
	s112b := if val112 > 20 { 'big' } else { 'small' }
	assert s112b == 'big'
	print_str(s112b) // big

	// 112.3 Return if-expr string via function
	assert describe_sign(5) == 'positive'
	assert describe_sign(-3) == 'negative'
	assert describe_sign(0) == 'zero'
	print_str(describe_sign(7)) // positive

	// 112.4 If-expr string used in concatenation
	prefix112 := 'Result: '
	s112c := prefix112 + if true { 'pass' } else { 'fail' }
	print_str(s112c) // Result: pass

	// 112.5 If-expr string in loop
	mut results112 := []string{}
	for i in 0 .. 3 {
		results112 << if i % 2 == 0 { 'even' } else { 'odd' }
	}
	assert results112[0] == 'even'
	assert results112[1] == 'odd'
	assert results112[2] == 'even'
	print_str(results112[1]) // odd

	print_str('if expr string: ok')

	print_str('--- 113. @FN Comptime ---')

	// 113.1 @FN returns current function name
	fn_name113 := @FN
	assert fn_name113.len > 0
	print_str(fn_name113) // main

	// 113.2 @FILE still works
	file113 := @FILE
	assert file113.contains('test_all_lang_features.v')
	print_str('@FN and @FILE: ok')

	print_str('--- 114. String in Expressions ---')

	// 114.1 String equality in variable
	eq114 := 'hello' == 'hello'
	assert eq114
	neq114 := 'hello' != 'world'
	assert neq114
	if eq114 && neq114 {
		print_int(1) // 1
	}

	// 114.2 String comparison result used in match
	cmp114 := 'abc' < 'def'
	r114 := if cmp114 { 'less' } else { 'not less' }
	assert r114 == 'less'
	print_str(r114) // less

	// 114.3 String method results in conditions
	s114 := 'hello world'
	if s114.starts_with('hello') && s114.ends_with('world') && s114.contains(' ') {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 114.4 String concatenation with if-expr
	greeting114 := 'Hello' + ', ' + 'World'
	assert greeting114 == 'Hello, World'
	print_str(greeting114) // Hello, World

	// 114.5 String length comparison
	short114 := 'ab'
	long114 := 'abcde'
	assert short114.len < long114.len
	print_int(long114.len - short114.len) // 3

	print_str('string expressions: ok')

	print_str('--- 115. Self-Host Regression Features ---')

	// 115.1 Map returned from a call keeps map type through for-in with keys.
	scores115 := make_scores115()
	mut key_len115 := 0
	mut value_sum115 := 0
	mut beta_value115 := 0
	for name, score in scores115 {
		key_len115 += name.len
		value_sum115 += score
		if name == 'beta' {
			beta_value115 = score
		}
	}
	print_int(key_len115) // alpha + beta + gamma = 14
	print_int(value_sum115) // 9
	print_int(beta_value115) // 3

	// 115.2 Optional struct guard payload supports selector reads.
	mut info_score115 := 0
	if info115 := maybe_call_info115(true) {
		print_str(info115.name) // resolved
		info_score115 = info115.score
	} else {
		info_score115 = -1
	}
	print_int(info_score115) // 7

	// 115.3 Optional guard selector can feed a map assignment.
	mut resolved_calls115 := map[int]string{}
	if info115b := maybe_call_info115(true) {
		resolved_calls115[42] = info115b.name
	}
	print_str(resolved_calls115[42]) // resolved

	// 115.4 Optional map lookup payload can be an array and then be iterated.
	mut suffix_map115 := map[string][]string{}
	suffix_map115['call'] << 'resolve'
	suffix_map115['call'] << 'emit'
	mut suffix_len115 := 0
	if suffixes115 := suffix_map115['call'] {
		for suffix115 in suffixes115 {
			suffix_len115 += suffix115.len
		}
	}
	print_int(suffix_len115) // 11

	// 115.5 Dynamic array clear and reuse in a loop.
	mut reusable115 := []string{}
	mut reuse_len115 := 0
	for i in 0 .. 3 {
		reusable115.clear()
		reusable115 << 'x'
		if i > 0 {
			reusable115 << 'yy'
		}
		for item115 in reusable115 {
			reuse_len115 += item115.len
		}
	}
	print_int(reuse_len115) // 7

	print_str('self-host regression features: ok')

	print_str('--- 116. Additional Self-Host Feature Coverage ---')

	// 116.1 Struct field defaults, including a lowered string expression.
	default116a := Defaults116{
		count: 7
	}
	print_str(default116a.name) // v3
	print_int(default116a.count) // 7
	default116b := Defaults116{}
	print_str(default116b.name) // v3
	print_int(default116b.count) // 42

	// 116.2 Map index or-block fallback and present value.
	map_or_present116 := scores115['alpha'] or { 0 }
	map_or_missing116 := scores115['missing'] or { 11 }
	print_int(map_or_present116) // 2
	print_int(map_or_missing116) // 11

	// 116.3 typeof expression lowering.
	type_name116 := typeof(123)
	print_str(type_name116) // int literal
	assert type_name116 == 'int literal'

	// 116.4 @VMODROOT compile-time path exists.
	vmod_root116 := @VMODROOT
	if vmod_root116.len > 0 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 116.5 If-expression lowered before a call argument.
	print_int(use_int116(if false { 1 } else { 2 })) // 2

	// 116.6 More than eight call arguments.
	print_int(sum_nine116(1, 2, 3, 4, 5, 6, 7, 8, 9)) // 45

	print_str('additional self-host feature coverage: ok')

	print_str('--- 117. ARM64 Self-Host Regression Coverage ---')

	// 117.1 Recursive sumtype values survive string-keyed map storage and lookup.
	mut registry117 := Registry117{
		prefix: 'types'
		items:  map[string]MetaType117{}
	}
	registry117.items['types.Type'] = MetaType117(MetaMap117{
		key: MetaType117(MetaNamed117{
			name: 'string'
		})
		val: MetaType117(MetaArray117{
			elem: MetaType117(MetaNamed117{
				name: 'Type'
			})
		})
	})
	type_key117 := registry117.qualify('Type')
	if type_key117 in registry117.items {
		type_meta117 := registry117.items[type_key117] or {
			MetaType117(MetaNamed117{
				name: 'missing'
			})
		}
		print_str(meta_name117(type_meta117)) // map[string][]Type
	} else {
		print_str('missing')
	}
	print_str(meta_name117(meta_array_elem117(MetaType117(MetaArray117{
		elem: MetaType117(MetaNamed117{
			name: 'Type'
		})
	})))) // Type
	print_str(meta_fn_signature117(MetaType117(MetaFn117{
		params: [MetaType117(MetaNamed117{
			name: 'ArrayDataHeader'
		})]
		ret:    MetaType117(MetaNamed117{
			name: 'base_data'
		})
	}))) // fn(ArrayDataHeader) base_data

	// 117.2 Smartcasted sumtype selector uses the active variant when fields share names.
	print_str(meta_iface_name117(MetaType117(MetaIface117{
		name: 'Writer'
	}))) // Writer
	print_str(meta_iface_call117(MetaType117(MetaIface117{
		name: 'Reader'
	}))) // Reader

	// 117.3 Option-returned array can be unwrapped with `or` and then iterated.
	strings117 := maybe_strings117(true) or { []string{} }
	mut strings_len117 := 0
	for item117 in strings117 {
		strings_len117 += item117.len
	}
	print_int(strings_len117) // 9

	// 117.4 Dynamic array index method lowers to the SSA array lookup helper.
	print_int(strings117.index('arm64')) // 1
	print_int(strings117.index('missing')) // -1

	// 117.5 strings.Builder.write lowers its internal push_many call.
	mut bytes117 := []u8{}
	bytes117 << 83
	bytes117 << 83
	bytes117 << 65
	mut builder117 := strings.new_builder(8)
	written117 := builder117.write(bytes117) or { -1 }
	print_int(written117) // 3
	print_str(builder117.str()) // SSA

	// 117.6 delete_last works through a mut dynamic-array parameter.
	mut trim_values117 := []int{}
	trim_values117 << 10
	trim_values117 << 20
	trim_values117 << 30
	trim_last117(mut trim_values117)
	print_int(trim_values117.len) // 2
	print_int(trim_values117.last()) // 20

	// 117.7 delete_last works on a dynamic-array field selector.
	mut stack117 := Stack117{
		values: []int{}
	}
	stack117.values << 4
	stack117.values << 5
	stack117.values << 6
	trim_stack117(mut stack117)
	print_int(stack117.values.len) // 2
	print_int(stack117.values.last()) // 5

	// 117.8 Smartcasts do not leak to later functions with the same variable name.
	print_str(smartcast_name117(CastLeak117(CastNamed117{
		name: 'scoped'
	}))) // scoped
	if Marker117.on.is_on() {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 117.9 Alias scalar if-expression lowers to a tcc-compatible zero initializer.
	print_int(int(scalar_alias_if117(false))) // 12

	// 117.10 Fixed-array lengths can come from const expressions in type declarations.
	fixed_holder117 := FixedHolder117{
		values: [7, 8, 9]!
	}
	print_int(fixed_holder_sum117(fixed_holder117)) // 24

	// 117.11 Function pointer fields use generated typedefs before struct declarations.
	fn_holder117 := FnHolder117{
		op: add117
	}
	print_int(fn_holder_call117(fn_holder117)) // 9

	// 117.12 Pointer-receiver methods on array index expressions receive element addresses.
	mut instrs117 := []Instr117{}
	instrs117 << Instr117{
		op:       7
		operands: [11]
	}
	print_int(instrs117[0].operand_total117(5)) // 23

	// 117.13 Locals inferred from array indexing keep their struct field types.
	store117 := TypeStore117{
		types: [FieldBag117{
			values: [31]
		}]
	}
	print_int(indexed_struct_field117(store117, 0)) // 31

	// 117.14 Direct smartcast `as` uses the narrowed local inside the branch.
	print_str(smartcast_as_name117(CastLeak117(CastNamed117{
		name: 'as-cast'
	}))) // as-cast

	// 117.15 Sum fields copied to locals can be smartcast before concrete assignment.
	print_str(pointer_array_elem117(MetaType117(MetaPointer117{
		base: MetaType117(MetaArray117{
			elem: MetaType117(MetaNamed117{
				name: 'Elem'
			})
		})
	}))) // Elem

	// 117.16 String short-name extraction uses mutable assignment instead of if-value.
	print_str(short_variant117('types.Array')) // Array

	// 117.17 Option-returning match branches return none explicitly.
	print_int(optional_match_arith117('div', 8, 2) or { -1 }) // 4
	print_int(optional_match_arith117('div', 8, 0) or { -5 }) // -5

	// 117.18 Address-of zero-valued struct constants returns a stable pointer.
	print_int(const_holder_value117()) // 0

	// 117.19 Range membership lowers without leaving raw range expressions in SSA.
	if 5 in 3..7 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 117.20 Compound assignment mutates fields through dynamic-array indexes.
	mut sym_store117 := SymbolStore117{
		symbols: [Symbol117{
			sect:  2
			value: 5
		}, Symbol117{
			sect:  3
			value: 11
		}]
	}
	print_int(adjust_symbol_values117(mut sym_store117, 100)) // 116

	// 117.21 Single-variable for-in over []string preserves element type.
	print_int(sum_string_for_in117(strings117)) // 9

	// 117.22 For-in over structs with sumtype fields keeps field types.
	field_types117 := [
		ForField117{
			name: 'elem'
			typ:  MetaType117(MetaArray117{
				elem: MetaType117(MetaNamed117{
					name: 'Type'
				})
			})
		},
	]
	print_str(find_field_type117(field_types117, 'elem')) // []Type

	// 117.23 Signed truncation keeps negative values negative for comparisons.
	mut wide_neg117 := i64(0 - 1)
	if int(wide_neg117) >= 0 {
		print_int(0)
	} else {
		print_int(1) // 1
	}
	if int(wide_neg117) < 0 {
		print_int(1) // 1
	} else {
		print_int(0)
	}

	// 117.24 Map clone owns its storage instead of aliasing the original map.
	print_int(map_clone_pair117()) // 27

	// 117.25 Optional struct payloads preserve string fields from array indexes.
	print_int(optional_context_score117()) // 1

	// 117.26 Signed narrow loads keep sign bits for comparisons.
	print_int(signed_narrow_load_score117()) // 15

	// 117.27 Mut parameters can update array fields through selector bases.
	print_int(mut_selector_base_score117()) // 38

	// 117.28 Chained else-if expressions keep a stable branch tail type.
	print_int(chained_if_expr_score117(7)) // 80

	// 117.29 Same-named sum-type methods resolve from the actual receiver.
	print_int(sum_type_method_same_name_score117()) // 33

	// 117.30 Dynamic-array pop returns the removed value and shrinks the array.
	print_int(array_pop_score117()) // 92

	// 117.31 Large recursive sum-type method receivers keep later variant payloads.
	print_int(large_sumtype_method_receiver_score117()) // 41

	// 117.32 Qualified imported calls do not fall back to same-short local functions.
	print_int(qualified_import_same_short_score117()) // 18

	// 117.33 Dynamic-array repeat preserves the repeated array length.
	print_int(array_repeat_score117()) // 6

	// 117.34 Discard assignment still evaluates RHS side effects.
	print_int(discard_assignment_side_effect_score117()) // 1

	// 118 Array DSL calls, enum .str(), callbacks, and result err returns compile.
	print_int(array_callback_enum_score118()) // 35

	// 119 v1 compiler feature regressions added while making v3 compile cmd/v.
	// 119.1 Return-if expressions with an early return branch.
	print_int(return_if_branch119(true)) // 12
	print_int(return_if_branch119(false)) // 7

	// 119.2 Match smartcasts survive early returns in non-matching branches.
	print_str(match_smartcast_return119(NestedExpr119(NestedIdent119{
		name: 'match-id'
	}))) // match-id
	print_str(match_smartcast_return119(NestedExpr119(NestedInfixExpr119{
		op: '+'
	}))) // not-ident

	// 119.3 Indexed sumtype smartcasts keep the selected variant field type.
	print_str(indexed_smartcast119([NestedExpr119(NestedIdent119{
		name: 'idx'
	})])) // idx

	// 119.4 Sumtype membership checks work with nested sumtype variants.
	print_int(nested_type_membership119(NestedNode119(NestedExpr119(NestedIdent119{
		name: 'plain'
	})))) // 1
	print_int(nested_type_membership119(NestedNode119(NestedExpr119(NestedInfixExpr119{
		op: '+'
	})))) // 0

	// 119.5 Const option-or fallback expressions are lowered.
	print_str(const_or119) // fallback

	// 119.6 Addressing matched sumtype values can initialize sumtype pointers.
	print_int(sum_pointer_target119(NestedNode119(NestedIfBranch119{}))) // 6

	// 119.7 Array sort callbacks and named compare functions are supported.
	print_int(sort_with_compare119()) // 125

	// 119.8 User-defined == and < operators drive derived comparisons.
	print_int(operator_compare119()) // 27

	// 119.9 Interface fields are readable through value and pointer interfaces.
	print_int(interface_field119()) // 38

	// 119.10 Generic math helpers used by cmd/v are available.
	print_int(math_generic119()) // 11

	// 119.11 Imported module constants can call receiver methods.
	print_int(module_const_method119()) // 83

	// 119.12 Module-qualified static methods resolve before loose .new fallbacks.
	print_int(qualified_static_new119()) // 71

	// 119.13 Static .new calls resolve by explicit type before loose fallbacks.
	print_int(local_static_new119()) // 75

	// 119.14 Imported module new() functions resolve after flattening.
	print_int(module_function_new119()) // 101

	// 119.15 Newline-separated nil assignment does not break pointer lowering.
	print_int(nil_newline_pointer_assignment119()) // 21

	// 119.16 IError fields accept none in defaults and explicit initializers.
	print_int(ierror_none_field119()) // 29

	// 119.17 Pointer-to-sumtype fields can default to nil.
	print_int(sum_pointer_nil_default119()) // 31

	// 119.18 Optional string interpolation prints none and present values.
	print_str(optional_string_interp119()) // Option(none)|Option('halt')

	// 119.19 Optional casts in struct fields produce present option values.
	print_str(optional_cast_field119('tail')) // Option('tail')

	// 119.20 Optional values assigned to optional fields are not double-wrapped.
	print_str(optional_passthrough_field119(maybe_const119(true))) // Option('set')

	// 119.21 flag_default_value calls are lowered through the transformer.
	print_str(flag_default_value_lowering119()) // "abc"

	// 119.22 Methods with map receivers resolve and return optional values.
	print_int(map_receiver_method119()) // 41

	// 119.23 Zero-value map fields can be queried without a runtime crash.
	print_int(zero_map_lookup119()) // 79

	// 119.24 Flag enum static zero() calls work with bit operations.
	print_int(flag_enum_zero119()) // 43

	// 119.25 Pointer casts to sumtype aliases keep valid pointer values.
	print_int(sum_alias_pointer_cast119(RecursiveExpr119(RecursiveIf119{}))) // 9

	// 119.26 @LOCATION expands to a non-empty compile-time string.
	print_int(at_location119()) // 4

	// 119.27 Atomic and wyhash C helper shims are available.
	print_int(c_atomic_wymix119()) // 13

	// 119.28 More atomic C helpers used by channel/runtime code are available.
	print_int(c_atomic_channel_helpers119()) // 38

	// 119.29 File lock C helper shims are available.
	print_int(c_filelock_helpers119()) // 47

	// 119.30 Returning &local struct values promotes them to heap storage.
	print_int(heap_local_return119()) // 53

	// 119.31 Rune array string conversion is available to the ARM64 backend.
	print_int(rune_array_string119()) // 61

	// 119.32 Prealloc atomic helpers are available to the ARM64 backend.
	print_int(prealloc_atomic_helpers119()) // 16

	// 119.33 Signal handler cast helper is available to the ARM64 backend.
	print_int(signal_handler_cast119()) // 67

	// 119.34 Print/eprint/eprintln builtins are available to the SSA runtime.
	print_int(printing_builtin_helpers119()) // 89

	// 119.35 String all_before_last/all_after_last helpers are available.
	print_int(string_last_part_helpers119()) // 97

	// 119.36 Channels can be initialized, sent, received, closed, and compared.
	print_int(channel_runtime119()) // 59

	// 119.37 Empty struct init uses typed zero values for missing fields.
	print_int(zero_default_struct119()) // 103

	// 119.38 os.join_path variadic arguments lower to chained join_path_single calls.
	print_int(join_path_variadic119()) // 107

	// 119.39 SSA sum literals preserve struct payload pointers.
	print_int(ssa_sum_payload_store119()) // 109

	// 119.40 Function-field calls on call results avoid checker recursion.
	print_int(selector_fn_call_base119()) // 113

	// 119.41 Map index `or { none }` and `!` propagate optional returns.
	print_int(map_index_optional_return119()) // 94

	// 119.42 Markused keeps receiver-peer methods needed by live methods.
	print_int(receiver_method_liveness119()) // 117

	print_str('arm64 self-host regression coverage: ok')

	print_str('=== ALL 119 TESTS PASSED ===')
}
