struct ModValue {
	value int
}

fn (value ModValue) mod() int {
	return value.value
}

fn (left ModValue) % (right ModValue) ModValue {
	return ModValue{
		value: left.value % right.value
	}
}

fn test_operator_and_method_with_same_c_name() {
	left := ModValue{
		value: 7
	}
	right := ModValue{
		value: 4
	}
	assert left.mod() == 7
	assert (left % right).value == 3
}

// Num has every overloadable operator next to an ordinary method named the way C
// would spell that operator. The methods take an `int`, so calling the wrong one
// does not compile, and they add 1000, so the results tell the two apart.
struct Num {
	v int
}

fn (a Num) + (b Num) Num {
	return Num{a.v + b.v}
}

fn (a Num) - (b Num) Num {
	return Num{a.v - b.v}
}

fn (a Num) * (b Num) Num {
	return Num{a.v * b.v}
}

fn (a Num) / (b Num) Num {
	return Num{a.v / b.v}
}

fn (a Num) % (b Num) Num {
	return Num{a.v % b.v}
}

fn (a Num) ** (b Num) Num {
	mut r := 1
	for _ in 0 .. b.v {
		r *= a.v
	}
	return Num{r}
}

fn (a Num) == (b Num) bool {
	return a.v == b.v
}

fn (a Num) < (b Num) bool {
	return a.v < b.v
}

fn (a Num) plus(n int) int {
	return a.v + n + 1000
}

fn (a Num) minus(n int) int {
	return a.v - n + 1000
}

fn (a Num) mul(n int) int {
	return a.v * n + 1000
}

fn (a Num) mul_(n int) int {
	return a.v * n * n + 1000
}

fn (a Num) pow(n int) int {
	return a.v + n * 2 + 1000
}

fn (a Num) div(n int) int {
	return a.v / n + 1000
}

fn (a Num) mod(n int) int {
	return a.v % n + 1000
}

fn (a Num) eq(n int) bool {
	return a.v == n + 1000
}

fn (a Num) ne(n int) bool {
	return a.v != n + 1000
}

fn (a Num) lt(n int) bool {
	return a.v < n + 1000
}

fn (a Num) gt(n int) bool {
	return a.v > n + 1000
}

fn (a Num) le(n int) bool {
	return a.v <= n + 1000
}

fn (a Num) ge(n int) bool {
	return a.v >= n + 1000
}

fn test_operators_and_methods_named_like_their_c_spelling() {
	a := Num{6}
	b := Num{2}
	assert (a + b).v == 8
	assert (a - b).v == 4
	assert (a * b).v == 12
	assert (a / b).v == 3
	assert (a % Num{4}).v == 2
	assert (a ** b).v == 36
	assert a == Num{6}
	assert a != b
	assert b < a
	assert a > b
	assert b <= a
	assert a >= b
	assert a.plus(2) == 1008
	assert a.minus(2) == 1004
	assert a.mul(2) == 1012
	assert a.mul_(2) == 1024
	assert a.pow(2) == 1010
	assert a.div(2) == 1003
	assert a.mod(4) == 1002
	assert !a.eq(6)
	assert a.ne(6)
	assert a.lt(6)
	assert !a.gt(6)
	assert a.le(6)
	assert !a.ge(6)
}

fn test_compound_assignments_next_to_methods_named_like_their_c_spelling() {
	mut x := Num{3}
	x **= Num{2}
	assert x.v == 9
	x += Num{1}
	assert x.v == 10
	x *= Num{3}
	assert x.v == 30
	mut arr := [Num{2}, Num{3}]
	arr[1] **= Num{3}
	assert arr[1].v == 27
	arr[0] -= Num{5}
	assert arr[0].v == -3
	mut m := {
		'k': Num{4}
	}
	m['k'] **= Num{2}
	assert m['k'].v == 16
	assert x.mul_(2) == 1120
	assert arr[1].plus(1) == 1028
}

struct Grid {
mut:
	cells []int
}

fn (g Grid) [] (i int) int {
	return g.cells[i]
}

fn (mut g Grid) []= (i int, value int) {
	g.cells[i] = value
}

fn (g Grid) op_index(i int) string {
	return 'op_index ${i}'
}

fn (mut g Grid) op_index_set(i int) string {
	return 'op_index_set ${i}'
}

fn test_index_operators_and_methods_named_like_their_c_spelling() {
	mut g := Grid{
		cells: [1, 2, 3]
	}
	g[1] = 20
	assert g[1] == 20
	assert g[2] == 3
	assert g.op_index(1) == 'op_index 1'
	assert g.op_index_set(2) == 'op_index_set 2'
}

// GNum is Num as a generic struct: its operators are specialized per instance.
struct GNum[T] {
	v T
}

fn (a GNum[T]) + (b GNum[T]) GNum[T] {
	return GNum[T]{a.v + b.v}
}

fn (a GNum[T]) - (b GNum[T]) GNum[T] {
	return GNum[T]{a.v - b.v}
}

fn (a GNum[T]) * (b GNum[T]) GNum[T] {
	return GNum[T]{a.v * b.v}
}

fn (a GNum[T]) / (b GNum[T]) GNum[T] {
	return GNum[T]{a.v / b.v}
}

fn (a GNum[T]) % (b GNum[T]) GNum[T] {
	return GNum[T]{a.v % b.v}
}

fn (a GNum[T]) ** (b GNum[T]) GNum[T] {
	mut r := T(1)
	for _ in 0 .. b.v {
		r *= a.v
	}
	return GNum[T]{r}
}

fn (a GNum[T]) == (b GNum[T]) bool {
	return a.v == b.v
}

fn (a GNum[T]) < (b GNum[T]) bool {
	return a.v < b.v
}

fn (a GNum[T]) plus(n T) T {
	return a.v + n + 1000
}

fn (a GNum[T]) minus(n T) T {
	return a.v - n + 1000
}

fn (a GNum[T]) mul(n T) T {
	return a.v * n + 1000
}

fn (a GNum[T]) mul_(n T) T {
	return a.v * n * n + 1000
}

fn (a GNum[T]) pow(n T) T {
	return a.v + n * 2 + 1000
}

fn (a GNum[T]) div(n T) T {
	return a.v / n + 1000
}

fn (a GNum[T]) mod(n T) T {
	return a.v % n + 1000
}

fn (a GNum[T]) eq(n T) bool {
	return a.v == n + 1000
}

fn (a GNum[T]) lt(n T) bool {
	return a.v < n + 1000
}

fn test_generic_operators_and_methods_named_like_their_c_spelling() {
	a := GNum[int]{6}
	b := GNum[int]{2}
	assert (a + b).v == 8
	assert (a - b).v == 4
	assert (a * b).v == 12
	assert (a / b).v == 3
	assert (a % GNum[int]{4}).v == 2
	assert (a ** b).v == 36
	assert a == GNum[int]{6}
	assert a != b
	assert b < a
	assert a > b
	assert a.plus(2) == 1008
	assert a.minus(2) == 1004
	assert a.mul(2) == 1012
	assert a.mul_(2) == 1024
	assert a.pow(2) == 1010
	assert a.div(2) == 1003
	assert a.mod(4) == 1002
	assert !a.eq(6)
	assert a.lt(6)
	c := GNum[i64]{3}
	assert (c ** GNum[i64]{3}).v == 27
	assert c.mul_(2) == 1012
}

fn test_generic_compound_assignments_next_to_methods_named_like_their_c_spelling() {
	mut x := GNum[int]{3}
	x **= GNum[int]{2}
	assert x.v == 9
	x += GNum[int]{1}
	assert x.v == 10
	mut arr := [GNum[u32]{2}, GNum[u32]{3}]
	arr[1] **= GNum[u32]{3}
	assert arr[1].v == 27
	assert arr[1].mul_(2) == 1108
	mut m := {
		'k': GNum[i16]{4}
	}
	m['k'] **= GNum[i16]{2}
	assert m['k'].v == 16
	assert m['k'].plus(1) == 1017
}
