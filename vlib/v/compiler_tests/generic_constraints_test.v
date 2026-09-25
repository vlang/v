module main

import os

// A type parameter can name an interface it must satisfy, `fn f[T Named]`: a
// call is checked against the interface, where V checks a value passed to an
// interface parameter, and the body can use on a `T` only what the interface
// declares. A type parameter without a constraint works as before.

const constraint_prelude = 'module main

interface Named {
	name string
}

interface Greeter {
	greet() string
}

interface NamedGreeter {
	Named
	Greeter
}

struct User {
	name string
	age  int
}

fn (u User) greet() string {
	return u.name
}

struct Pet {
	name string
}
'

// The prelude above is 27 lines: a program's own lines start at 28.
const first_line = 28

fn check_program(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, constraint_prelude + source) or { panic(err) }
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler -check -nocolor ${os.quoted_path(path)}')
}

// error_lines returns `line:col: message` for every error of a check output,
// with the line counted from the program's own first line.
fn error_lines(output string) []string {
	mut lines := []string{}
	for line in output.split_into_lines() {
		if !line.contains(': error: ') {
			continue
		}
		location := line.all_before(': error: ').split(':')
		if location.len < 3 {
			continue
		}
		program_line := location[location.len - 2].int() - first_line + 1
		lines << '${program_line}:${location[location.len - 1]}: ${line.all_after(': error: ')}'
	}
	return lines
}

fn test_a_constrained_call_with_types_that_implement_the_interface_is_valid() {
	res := check_program('valid', "fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn main() {
	u := longest(User{ name: 'Alex', age: 30 }, User{ name: 'Bob', age: 25 })
	println(u.age)
	println(longest(Pet{ name: 'Rex' }, Pet{ name: 'Tom' }).name)
}
")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

fn test_a_call_with_a_type_that_does_not_implement_the_constraint_is_reported_at_the_call() {
	res := check_program('int_call', 'fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn main() {
	println(longest(1, 2))
}
')
	assert res.exit_code == 1, res.output
	// Once, at the call: not again in the body of the instance for `int`.
	assert error_lines(res.output) == ["6:18: `int` doesn't implement field `name` of interface `Named`"], res.output
}

fn test_an_explicit_type_argument_is_checked_against_the_constraint() {
	res := check_program('explicit', "fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn main() {
	println(longest[User](User{ name: 'a' }, User{ name: 'b' }).age)
	println(longest[int](1, 2))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["7:18: `int` doesn't implement field `name` of interface `Named`"], res.output
}

fn test_a_missing_method_of_the_constraint_is_reported_at_the_call() {
	res := check_program('pet_call', "fn welcome[T NamedGreeter](x T) string {
	return x.greet() + x.name
}

fn main() {
	println(welcome(User{ name: 'a' }))
	println(welcome(Pet{ name: 'b' }))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["7:18: `Pet` doesn't implement method `greet` of interface `NamedGreeter`"], res.output
}

fn test_the_body_can_use_only_what_the_constraint_declares() {
	// Reported even though nothing calls `oldest`.
	res := check_program('body', 'fn oldest[T Named](a T, b T) T {
	if a.age > b.age {
		return a
	}
	println(a.nme)
	return b
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'2:7: type `T` has no field named `age`: its constraint `Named` does not declare it',
		'2:15: type `T` has no field named `age`: its constraint `Named` does not declare it',
		'5:12: type `T` has no field named `nme`: its constraint `Named` does not declare it',
	], res.output
}

fn test_a_method_the_constraint_does_not_declare_is_reported_in_the_body() {
	res := check_program('body_method', 'fn label[T Named](x T) string {
	return x.greet()
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'2:11: type `T` has no method `greet`: its constraint `Named` does not declare it',
	], res.output
}

fn test_a_local_that_gets_its_value_from_a_t_is_a_t() {
	// Every way a local can take a value of type `T`: each `.age` below is on a `T`.
	res := check_program('derived', "fn derived[T Named](a T, b T, items []T, m map[string]T, c bool, n int) int {
	x := a
	mut total := x.age
	mut y := a
	y = b
	total += y.age
	z := if c { a } else { b }
	total += z.age
	for w in items {
		total += w.age
	}
	for i, w2 in items {
		total += i + w2.age
	}
	first := items[0]
	total += first.age
	p, q := a, b
	total += p.name.len + q.age
	r := (a)
	total += r.age
	s := &a
	total += s.age
	t := match n {
		1 { a }
		else { b }
	}
	total += t.age
	u := m['k'] or { a }
	total += u.age
	v := x
	total += v.age
	return total + x.greet().len
}

struct Box[T Named] {
	item T
}

fn from_field[T Named](b Box[T]) int {
	x := b.item
	return x.age
}

constraint Number = int | f64

fn from_set[T Number](x T) int {
	y := x
	return y.len
}

fn main() {}
")
	assert res.exit_code == 1, res.output
	no_age := 'type `T` has no field named `age`: its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'3:17: ${no_age}',
		'6:13: ${no_age}',
		'8:13: ${no_age}',
		'10:14: ${no_age}',
		'13:19: ${no_age}',
		'16:17: ${no_age}',
		'18:26: ${no_age}',
		'20:13: ${no_age}',
		'22:13: ${no_age}',
		'27:13: ${no_age}',
		'29:13: ${no_age}',
		'31:13: ${no_age}',
		'32:19: type `T` has no method `greet`: its constraint `Named` does not declare it',
		'41:11: ${no_age}',
		'48:11: type `T` has no field named `len`: `int`, in its constraint `Number`, does not have it',
	], res.output
}

fn test_a_local_from_a_guard_a_call_or_an_array_method_is_a_t() {
	// `U`, not `T`: the type of a call must be the caller's parameter, not the
	// callee's `T`, which has the same name in `find` and `pair`.
	res := check_program('derived_calls', "fn find[T Named](items []T) ?T {
	return none
}

fn pair[T Named](a T) (T, int) {
	return a, 1
}

fn derived[U Named](a U, items []U, m map[string]U) int {
	mut total := 0
	if v := m['k'] {
		total += v.age
	}
	if w := find(items) {
		total += w.age
	}
	p, n := pair(a)
	total += n + p.age
	arr := [a]
	total += arr[0].age
	f := items.first()
	total += f.age
	for g in items.filter(it.name.len > 0) {
		total += g.age
	}
	for h in items.reverse() {
		total += h.age
	}
	return total
}

fn main() {}
")
	assert res.exit_code == 1, res.output
	no_age := 'type `U` has no field named `age`: its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'12:14: ${no_age}',
		'15:14: ${no_age}',
		'18:17: ${no_age}',
		'20:18: ${no_age}',
		'22:13: ${no_age}',
		'24:14: ${no_age}',
		'27:14: ${no_age}',
	], res.output
}

fn test_a_generic_call_in_the_body_has_the_type_of_the_caller() {
	// `pick(n)` is a `U`: its own `T` is bound to the caller's `U`, and the
	// caller's `T` is another type parameter, with another constraint.
	res := check_program('call_type', 'fn pick[T Named](a T) T {
	return a
}

struct Box[T Named] {
	item T
}

fn (b Box[T]) get() T {
	return b.item
}

fn mixed[T Greeter, U Named](g T, n U, b Box[U]) int {
	x := pick(n)
	println(x.name)
	println(g.greet())
	y := b.get()
	println(y.name)
	return x.age + y.age + pick(n).age
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	no_age := 'type `U` has no field named `age`: its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'19:11: ${no_age}',
		'19:19: ${no_age}',
		'19:33: ${no_age}',
	], res.output
}

fn test_an_operator_needs_every_type_of_the_constraint() {
	// An interface declares no operators: a `T` it constrains has `==`, `!=` and
	// `in`, as a value of the interface. A set takes what all of its types take.
	res := check_program('operators', 'constraint Number = int | f64

constraint Word = string | int

constraint Flag = bool | int

struct Vec {
	x int
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x}
}

constraint Addable = int | Vec

type IntArr = []int

type StrMap = map[string]int

constraint Indexable = string | IntArr

constraint Keyed = string | StrMap

fn iface[T Named](a T, b T, items []T) bool {
	println(a == b)
	println(a in items)
	println(a + b)
	mut c := a
	c += b
	c++
	println(-a)
	d := items[0]
	println(d * b)
	return a < b
}

fn numbers[T Number](x T, y T) T {
	println(x < y)
	println(x + y * x - y / x)
	println(x + 1)
	println(x % y)
	println(x << 1)
	println(-x)
	mut z := x
	z += y
	z++
	return z
}

fn words[T Word](x T) T {
	println(x + x)
	println(x < x)
	println(x + 1)
	println(1 + x)
	return x - x
}

fn flags[T Flag](x T) bool {
	println(x == x)
	println(!x)
	return x && x
}

fn addables[T Addable](x T) T {
	println(x + x)
	return x - x
}

fn indexes[T Indexable, K Keyed](x T, k K) {
	println(x[0])
	println(k[0])
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	named := 'its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'28:12: operator `+` is not defined on type `T`: ${named}',
		'30:4: operator `+=` is not defined on type `T`: ${named}',
		'31:3: operator `++` is not defined on type `T`: ${named}',
		'32:10: operator `-` is not defined on type `T`: ${named}',
		'34:12: operator `*` is not defined on type `T`: ${named}',
		'35:11: operator `<` is not defined on type `T`: ${named}',
		'42:12: operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it',
		'43:12: operator `<<` is not defined on type `T` and `int literal`: `f64`, in its constraint `Number`, does not have it',
		'54:12: operator `+` is not defined on type `T` and `int literal`: `string`, in its constraint `Word`, does not have it',
		'55:12: operator `+` is not defined on `int literal` and type `T`: `string`, in its constraint `Word`, does not have it',
		'56:11: operator `-` is not defined on type `T`: `string`, in its constraint `Word`, does not have it',
		'61:10: operator `!` is not defined on type `T`: `int`, in its constraint `Flag`, does not have it',
		'62:11: operator `&&` is not defined on type `T`: `int`, in its constraint `Flag`, does not have it',
		'67:11: operator `-` is not defined on type `T`: `Vec`, in its constraint `Addable`, does not have it',
		'72:11: type `K` cannot be indexed with `int literal`: `StrMap`, in its constraint `Keyed`, does not have it',
	], res.output
}

fn test_an_append_to_a_constrained_array_is_left_to_its_elements() {
	// `items << x` as a statement is an append, which `[]int` takes; as a value,
	// `_ = items << x`, it is a shift, which no array takes.
	res := check_program('append', 'type IntArr = []int

type IntArr2 = []i64

constraint Ints = IntArr | IntArr2

fn push[T Ints](mut items T) {
	items << 1
	_ = items << 1
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'9:12: operator `<<` is not defined on type `T` and `int literal`: `IntArr`, in its constraint `Ints`, does not have it',
	], res.output
}

fn test_a_compile_time_is_narrows_a_constrained_type() {
	// In `$if T is f64 {`, `T` is `f64`; in its `$else`, the rest of its set. A
	// branch that no type of the set reaches, `T is string`, is not checked, and
	// a condition that names no type parameter leaves the set as it is.
	res := check_program('comptime_is', 'constraint Number = int | f64

fn narrow[T Number](x T, y T) T {
	$if T is f64 {
		println(x % y)
	} $else {
		println(x % y)
	}
	$if T is int {
		println(x.len)
	}
	$if T is string {
		println(x.len)
	}
	$if T in [int, f64] {
		println(x + y)
	}
	$if T is $int {
		println(x << 1)
	} $else {
		println(x << 1)
	}
	$if debug {
		println(x % y)
	}
	$if !debug {
		println(x % y)
	}
	$if T !is f64 {
		println(x % y)
	}
	$if T !in [int] {
		println(x % y)
	}
	$if T is int || T is f64 {
		println(x % y)
	}
	return x
}

fn narrow_iface[T Named](a T) {
	$if T is User {
		println(a.age)
	} $else {
		println(a.age)
	}
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'5:13: operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it',
		'10:13: type `T` has no field named `len`: `int`, in its constraint `Number`, does not have it',
		'21:13: operator `<<` is not defined on type `T` and `int literal`: `f64`, in its constraint `Number`, does not have it',
		'27:13: operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it',
		'33:13: operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it',
		'36:13: operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it',
		'45:13: type `T` has no field named `age`: its constraint `Named` does not declare it',
	], res.output
}

fn test_a_compile_time_is_on_a_value_narrows_its_type_parameter() {
	// `$if x is f64 {` with `x T` asks what `$if T is f64 {` asks, for a local
	// that gets its value from a `T` too, and with `!is`, `in`, `!in`, the groups
	// and `||`. An `is` outside `$if` is no compile-time test: V rejects it.
	res := check_program('comptime_value_is', 'constraint Number = int | f64

fn narrow[T Number](x T, y T) T {
	$if x is f64 {
		println(x % y)
	} $else {
		println(x % y)
	}
	$if y is int {
		println(x % y)
	}
	$if x !is f64 {
		println(x % y)
	}
	$if x in [int] {
		println(x % y)
	}
	$if x !in [int] {
		println(x % y)
	}
	$if x is $int {
		println(x << 1)
	} $else {
		println(x << 1)
	}
	z := x
	$if z is int {
		println(z % y)
	}
	$if x is f64 || y is f64 {
		println(x % y)
	}
	return x
}

fn narrow_iface[T Named](a T) {
	$if a is User {
		println(a.age)
	} $else {
		println(a.age)
	}
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'5:13: operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it',
		'19:13: operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it',
		'24:13: operator `<<` is not defined on type `T` and `int literal`: `f64`, in its constraint `Number`, does not have it',
		'31:13: operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it',
		'40:13: type `T` has no field named `age`: its constraint `Named` does not declare it',
	], res.output
}

fn test_a_runtime_is_on_a_value_of_a_constrained_type_is_reported() {
	// V takes `is` on a sum type or an interface value only: `value is f64` with
	// `value T` does not build for a type of the constraint that is neither, so
	// the check says so where it is written; `$if value is f64 {` tests `T`. A
	// set of sum types takes it.
	res := check_program('runtime_is', 'constraint Number = int | f64

type Shape = Square | Circle

struct Square {}

struct Circle {}

constraint Shapes = Shape

fn numbers[T Number](value T) string {
	if value is f64 || value is int {
		return value.str()
	}
	if value !is f64 {
		return "x"
	}
	$if value is f64 {
		return "f"
	}
	return ""
}

fn named[T Named](a T) {
	if a is User {
		println(a.age)
	}
}

fn shapes[T Shapes](s T) bool {
	return s is Circle
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'12:5: `is` can only be used with sum type or interface values, not `T`: `int`, in its constraint `Number`, is neither; test `T` with `\$if value is f64`',
		'12:21: `is` can only be used with sum type or interface values, not `T`: `int`, in its constraint `Number`, is neither; test `T` with `\$if value is int`',
		'15:5: `is` can only be used with sum type or interface values, not `T`: `int`, in its constraint `Number`, is neither; test `T` with `\$if value !is f64`',
		'25:5: `is` can only be used with sum type or interface values, not `T`: a type that implements `Named` does not have to be one; test `T` with `\$if a is User`',
	], res.output
}

fn test_a_constrained_generic_needs_the_constraint_of_a_type_parameter_it_takes() {
	// `Box[T]` or `pick(a, b)` with a type parameter of the declaration around
	// them: its constraint has to satisfy theirs, where they are written, as in
	// Go and Rust. A `T` without one is told which to take; an interface that
	// embeds `Named` satisfies `Named`, and so does `T` in `$if T is int`.
	res := check_program('propagation', 'struct Box[T Named] {
	item T
}

fn wrap[T](x T) Box[T] {
	return Box[T]{
		item: x
	}
}

fn wrap_ok[T Named](x T) Box[T] {
	return Box[T]{
		item: x
	}
}

interface Other {
	id int
}

fn wrap_other[T Other](x T) Box[T] {
	return Box{
		item: x
	}
}

fn pick[T Named](a T, b T) T {
	return a
}

fn call_unconstrained[U](a U, b U) U {
	return pick(a, b)
}

fn call_ok[U NamedGreeter](a U, b U) U {
	return pick(a, b)
}

fn call_explicit[U](a U) U {
	return pick[U](a, a)
}

constraint Number = int | f64

constraint Word = string | int

fn double[T Number](x T) T {
	return x + x
}

fn call_set[W Word](x W) W {
	return double(x)
}

fn call_set_ok[W Number](x W) W {
	return double(x)
}

fn call_literal[U](x U) {
	println(pick(1, 2))
}

struct Outer[T] {
	inner Box[T]
}

struct OuterOk[T Named] {
	inner Box[T]
}

fn (b Box[T]) twice() Box[T] {
	return b
}

fn narrowed_call[T Number](x T) {
	$if T is int {
		println(double(x))
	}
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'6:9: `Box[T]` needs `T` to implement `Named`: `T` has no constraint: give it one, `[T Named]`',
		'5:17: `Box[T]` needs `T` to implement `Named`: `T` has no constraint: give it one, `[T Named]`',
		'22:9: `Box[T]` needs `T` to implement `Named`: its constraint `Other` does not',
		'21:29: `Box[T]` needs `T` to implement `Named`: its constraint `Other` does not',
		'32:14: `pick` needs `U` to implement `Named`: `U` has no constraint: give it one, `[U Named]`',
		'40:14: `pick` needs `U` to implement `Named`: `U` has no constraint: give it one, `[U Named]`',
		'52:16: `double` needs `W` to be in `Number`: `string`, in its constraint `Word`, is not',
		"60:15: `int` doesn't implement field `name` of interface `Named`",
		'64:8: `Box[T]` needs `T` to implement `Named`: `T` has no constraint: give it one, `[T Named]`',
	], res.output
}

fn test_a_generic_interface_is_a_constraint_bound_to_the_type_argument() {
	// `[T Comparable[T]]`: the interface of each type argument itself, so `Odd`,
	// whose `less` takes an `int`, is not `Comparable[Odd]`; `[T Comparable[int]]`
	// is bound as it is written. The same for a generic struct, a function value
	// and a type parameter passed on.
	res := check_program('generic_iface', 'interface Comparable[T] {
	less(other T) bool
}

struct Num {
	v int
}

fn (a Num) less(b Num) bool {
	return a.v < b.v
}

struct Odd {
	v int
}

fn (a Odd) less(b int) bool {
	return a.v < b
}

fn smallest[T Comparable[T]](a T, b T) T {
	if a.less(b) {
		return a
	}
	println(a.v)
	return b
}

fn below[T Comparable[int]](a T, n int) bool {
	return a.less(n)
}

struct Sorted[T Comparable[T]] {
	items []T
}

struct Shelves {
	good Sorted[Num]
	bad  Sorted[Odd]
}

fn relay[U Comparable[U]](a U, b U) U {
	return smallest(a, b)
}

fn relay_bad[U](a U, b U) U {
	return smallest(a, b)
}

fn main() {
	println(smallest(Num{1}, Num{2}).v)
	println(smallest(Odd{1}, Odd{2}).v)
	println(smallest(User{}, User{}).name)
	println(smallest(1, 2))
	println(below(Odd{1}, 5))
	println(below(Num{1}, 5))
	f := smallest[Odd]
	println(f)
}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'25:12: type `T` has no field named `v`: its constraint `Comparable` does not declare it',
		'39:7: `Odd` incorrectly implements method `less` of interface `Comparable`: expected `Odd`, not `int` for parameter 1',
		'47:18: `smallest` needs `U` to implement `Comparable[U]`: `U` has no constraint: give it one, `[U Comparable[U]]`',
		'52:19: `Odd` incorrectly implements method `less` of interface `Comparable`: expected `Odd`, not `int` for parameter 1',
		"53:19: `User` doesn't implement method `less` of interface `Comparable`",
		"54:19: `int` doesn't implement method `less` of interface `Comparable`",
		'56:16: `Num` incorrectly implements method `less` of interface `Comparable`: expected `int`, not `Num` for parameter 1',
		'57:16: `Odd` incorrectly implements method `less` of interface `Comparable`: expected `Odd`, not `int` for parameter 1',
	], res.output
}

fn test_a_local_of_type_t_can_use_what_the_constraint_declares() {
	res := check_program('derived_valid', "fn total[T Named](a T, items []T) int {
	x := a
	mut sum := x.name.len
	for y in items {
		sum += y.name.len
	}
	n := 5
	sum += n
	s := 'abc'
	sum += s.len
	first := items[0]
	return sum + first.name.len
}

fn main() {
	println(total(User{ name: 'a' }, [User{
		name: 'b'
	}]))
}
")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

fn test_a_generic_struct_checks_its_type_arguments() {
	res := check_program('struct', "struct Box[T Named] {
	item T
}

fn (b Box[T]) label() string {
	return b.item.name
}

fn main() {
	println(Box[User]{ item: User{ name: 'a' } }.label())
	println(Box[int]{ item: 1 }.label())
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["11:10: `int` doesn't implement field `name` of interface `Named`"], res.output
}

fn test_a_generic_struct_type_written_in_a_declaration_is_checked() {
	res := check_program('declared', "struct Box[T Named] {
	item T
}

struct Shelf {
	good Box[User]
	bad  Box[int]
}

fn show(b Box[int]) string {
	return ''
}

fn keep(b Box[User]) Box[User] {
	return b
}

fn main() {}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		"7:7: `int` doesn't implement field `name` of interface `Named`",
		"10:11: `int` doesn't implement field `name` of interface `Named`",
	], res.output
}

fn test_an_inferred_generic_struct_init_is_checked() {
	res := check_program('inferred_init', "struct Box[T Named] {
	item T
}

fn main() {
	good := Box{
		item: User{ name: 'a' }
	}
	bad := Box{
		item: 1
	}
	println('\${good.item.name} \${bad}')
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["9:9: `int` doesn't implement field `name` of interface `Named`"], res.output
}

fn test_a_generic_type_written_anywhere_in_a_declaration_is_checked() {
	// A type alias of a function, the methods of an interface, an embedded struct,
	// the key of a map, a nested application, an anonymous struct, a sum type and
	// an alias: each at the application that fails, not at the start of the type.
	res := check_program('written_decls', 'struct Box[T Named] {
	item T
}

type FnAlias = fn (Box[int]) int

type GoodFn = fn (Box[User]) int

interface Shelf {
	put(b Box[int])
	get() Box[int]
	keep(b Box[User]) Box[User]
}

struct Holder {
	Box[int]
	keyed map[Box[int]]string
	nested []Box[Box[User]]
	good Box[User]
	anon struct {
		inner Box[int]
	}
}

type SumBad = Box[int] | string

type AliasBad = Box[int]

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		"5:20: `int` doesn't implement field `name` of interface `Named`",
		"10:8: `int` doesn't implement field `name` of interface `Named`",
		"11:8: `int` doesn't implement field `name` of interface `Named`",
		"16:2: `int` doesn't implement field `name` of interface `Named`",
		"17:12: `int` doesn't implement field `name` of interface `Named`",
		"18:11: `Box[User]` doesn't implement field `name` of interface `Named`",
		"21:9: `int` doesn't implement field `name` of interface `Named`",
		"25:15: `int` doesn't implement field `name` of interface `Named`",
		"27:17: `int` doesn't implement field `name` of interface `Named`",
	], res.output
}

fn test_a_generic_type_written_in_an_expression_is_checked() {
	// Explicit type arguments of a call, map, array and channel literals, `sizeof`,
	// `typeof`, `isreftype`, casts to a pointer or an option, and a function literal's
	// parameter and return type. The same with `Box[User]` is valid.
	res := check_program('written_exprs', 'struct Box[T Named] {
	item T
}

fn make[T]() T {
	return T{}
}

fn sites() {
	made := make[Box[int]]()
	_ = made
	x1 := map[string]Box[int]{}
	x2 := sizeof(Box[int])
	x3 := sizeof[Box[int]]()
	x4 := typeof[Box[int]]().name
	x5 := isreftype(Box[int])
	x6 := unsafe { &Box[int](nil) }
	x7 := ?Box[int](none)
	x8 := []Box[int]{len: 2}
	x9 := chan Box[int]{}
	x10 := fn (b Box[int]) Box[int] {
		return b
	}
	x11 := []&Box[int]{}
	x12 := map[string][]Box[Box[User]]{}
	good1 := map[string]Box[User]{}
	good2 := sizeof(Box[User])
	good3 := fn (b Box[User]) Box[User] {
		return b
	}
	_ = [x1.len, int(x2), int(x3), x4.len]
	_ = [x5, x6 == unsafe { nil }, x7 == none, x8.len == 0, x9.len == 0, x11.len == 0, x12.len == 0]
	_ = [good1.len, int(good2)]
	_ = x10
	_ = good3
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		"10:15: `int` doesn't implement field `name` of interface `Named`",
		"12:19: `int` doesn't implement field `name` of interface `Named`",
		"13:15: `int` doesn't implement field `name` of interface `Named`",
		"14:15: `int` doesn't implement field `name` of interface `Named`",
		"15:15: `int` doesn't implement field `name` of interface `Named`",
		"16:18: `int` doesn't implement field `name` of interface `Named`",
		"17:18: `int` doesn't implement field `name` of interface `Named`",
		"18:9: `int` doesn't implement field `name` of interface `Named`",
		"19:10: `int` doesn't implement field `name` of interface `Named`",
		"20:13: `int` doesn't implement field `name` of interface `Named`",
		"21:15: `int` doesn't implement field `name` of interface `Named`",
		"21:25: `int` doesn't implement field `name` of interface `Named`",
		"24:12: `int` doesn't implement field `name` of interface `Named`",
		"25:22: `Box[User]` doesn't implement field `name` of interface `Named`",
	], res.output
}

fn test_a_generic_function_value_is_checked() {
	res := check_program('fn_value', "fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn main() {
	good := longest[User]
	bad := longest[int]
	println(good(User{ name: 'a' }, User{ name: 'b' }).age)
	println(bad(1, 2))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["7:17: `int` doesn't implement field `name` of interface `Named`"], res.output
}

fn test_a_constraint_must_be_an_interface() {
	res := check_program('not_interface', 'fn twice[T User](x T) T {
	return x
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'1:12: the constraint of `T` must be an interface or a constraint, not `User`',
	], res.output
}

fn test_a_constraint_is_a_type_name_alone() {
	// V3 used to skip what follows the name of a type parameter: this compiled.
	res := check_program('not_a_type', 'fn id[T Foo + 42](x T) T {
	return x
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['1:13: unexpected token `+`, expecting `,` or `]`'], res.output
}

fn test_an_unconstrained_generic_works_as_before() {
	res := check_program('unconstrained', "fn show[T](x T) string {
	return x.name
}

fn main() {
	println(show(User{ name: 'a' }))
	println(show(1))
}
")
	assert res.exit_code == 1, res.output
	// The error of the instance for `int`, in the body, as without constraints.
	assert error_lines(res.output) == ['2:11: `int` has no property `name`'], res.output
}

fn test_a_constraint_from_another_module_is_checked_at_the_call() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_modules_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'shapes')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'shapes', 'shapes.v'), 'module shapes

pub interface Named {
	name string
}

pub fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'main.v'), "module main

import shapes

struct City {
	name string
}

fn main() {
	println(shapes.longest(City{ name: 'Lima' }, City{ name: 'Oslo' }).name)
	println(shapes.longest(1, 2))
}
") or {
		panic(err)
	}
	// Several parser workers, as a machine with more cores runs it.
	res := os.execute('cd ${os.quoted_path(dir)} && VJOBS=4 ${os.quoted_path(@VEXE)} -new-compiler -check -nocolor .')
	assert res.exit_code == 1, res.output
	errors := res.output.split_into_lines().filter(it.contains(': error: '))
	assert errors == ["main.v:11:25: error: `int` doesn't implement field `name` of interface `Named`"], res.output
}

// A constraint can also be a set of types, declared with `constraint`: the
// type argument must be one of them, and the body can use on a value of type
// `T` what every type of the set has.

fn test_a_call_with_a_type_of_the_constraint_set_is_valid() {
	res := check_program('set_valid', 'constraint Number = int | i64 | f64

fn double[T Number](x T) T {
	return x + x
}

fn describe[T Number](x T) string {
	return x.str()
}

fn main() {
	println(double(2))
	println(double(2.5))
	println(describe(i64(3)))
}
')
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

fn test_a_call_with_a_type_outside_the_constraint_set_is_reported_at_the_call() {
	res := check_program('set_call', "constraint Number = int | i64 | f64

fn double[T Number](x T) T {
	return x + x
}

fn main() {
	println(double('a'))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['8:17: cannot use `string` as `T`: it is not in its constraint `Number`'], res.output
}

fn test_the_body_can_use_only_what_every_type_of_the_set_has() {
	res := check_program('set_body', 'constraint Animal = User | Pet

fn label[T Animal](x T) string {
	return x.name + x.greet()
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'4:20: type `T` has no method `greet`: `Pet`, in its constraint `Animal`, does not have it',
	], res.output
}

fn test_the_body_can_use_what_every_type_of_a_set_of_any_types_has() {
	// `len` of arrays, maps and strings, the fields and methods builtin declares
	// for them, and what an interface of the set declares.
	res := check_program('set_members_any', 'constraint Sized = []int | []string | map[string]int | string

constraint Listy = []int | []string

constraint Value = Named

fn size[T Sized](x T) int {
	return x.len
}

fn caps[T Sized](x T) int {
	return x.cap
}

fn copies[T Sized](x T) T {
	return x.clone()
}

fn reversed[T Listy](x T) T {
	return x.reverse()
}

fn show[T Value](x T) string {
	return x.name
}

fn ages[T Value](x T) int {
	return x.age
}

fn texts[T Sized](x T) string {
	return x.str()
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'12:11: type `T` has no field named `cap`: `map[string]int`, in its constraint `Sized`, does not have it',
		'28:11: type `T` has no field named `age`: `Named`, in its constraint `Value`, does not have it',
	], res.output
}

fn test_a_constraint_set_of_unknown_types_is_reported() {
	res := check_program('set_unknown', 'constraint Bad = int | Foo

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['1:24: unknown type `Foo`'], res.output
}

// half_written_errors checks a program with `main` and `decl`, before `main` or,
// `at_end`, after it, and returns `line:col: message` for each of its errors.
fn half_written_errors(name string, decl string, at_end bool) []string {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_half_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_fn := 'fn main() {\n\tprintln(1)\n}\n'
	source := if at_end {
		'module main\n\n${main_fn}\n${decl}\n'
	} else {
		'module main\n\n${decl}\n\n${main_fn}'
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source) or { panic(err) }
	res := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check -nocolor ${os.quoted_path(path)}')
	mut lines := []string{}
	for line in res.output.split_into_lines() {
		if line.contains(': error: ') {
			lines << line.all_after('main.v:')
		}
	}
	return lines
}

// shifted_columns moves each error of `errors` on line `decl_line` that is past
// column `keyword_col` right by `shift`: what a longer keyword moves.
fn shifted_columns(errors []string, decl_line int, keyword_col int, shift int) []string {
	mut result := []string{}
	for e in errors {
		parts := e.split_nth(':', 3)
		if parts.len == 3 && parts[0].int() == decl_line && parts[1].int() > keyword_col {
			result << '${parts[0]}:${parts[1].int() + shift}:${parts[2]}'
		} else {
			result << e
		}
	}
	return result
}

// without_eof_column is `e` without its column when it is about the end of the
// file: there, the error is where the scanner was last, on the keyword `type`
// and past the word `constraint`, where the name is missing.
fn without_eof_column(e string) string {
	parts := e.split_nth(':', 3)
	if !e.contains('eof') || parts.len != 3 {
		return e
	}
	return '${parts[0]}:${parts[2]}'
}

fn test_a_half_written_constraint_is_reported_as_a_half_written_type() {
	// While a `constraint` declaration is being written, V reports what it reports
	// for a `type` declaration at the same point, and not a statement of a script
	// such as "all definitions must occur before code in script mode". `type` at
	// the end of a file also says "a type alias can not refer to itself: " about a
	// name it did not get. Neither says anything about `Foo =` or `Foo = int |` at
	// the end of a file.
	mut compared := 0
	for tail in ['', ' ', ' Foo', ' Foo ', ' Foo =', ' Foo = int |'] {
		for prefix in ['', 'pub '] {
			for at_end in [false, true] {
				type_errors := half_written_errors('type', '${prefix}type${tail}', at_end)
				constraint_errors := half_written_errors('constraint', '${prefix}constraint${tail}',
					at_end)
				decl_line := if at_end { 7 } else { 3 }
				want := shifted_columns(type_errors.filter(!it.contains('a type alias can not refer to itself')),
					decl_line, prefix.len + 1, 'constraint'.len - 'type'.len)
				compared += want.len
				assert constraint_errors.map(without_eof_column(it)) == want.map(without_eof_column(it)), '`${prefix}constraint${tail}`, at the end: ${at_end}'
			}
		}
	}
	assert compared > 20
}

fn test_a_constraint_declaration_follows_the_syntax_rules_of_a_type_declaration() {
	// As `type T = int | f64` and `type Xy = none | int` are reported. A constraint
	// has no type parameters: its types are the ones it lists.
	res := check_program('constraint_syntax', 'constraint T = int | f64

constraint WithNone = int | none

constraint Pair[T] = int | f64

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'1:12: single letter capital names are reserved for generic template types',
		'3:29: a constraint cannot have none as one of its types',
		'5:16: a constraint cannot have type parameters',
	], res.output
}

fn test_a_constraint_declaration_follows_the_rules_of_a_type_declaration() {
	// Its name, the types it lists, and another type or constraint of the same
	// name, as a sum type is checked. Any type can be in the set: an array, a
	// map, an option, a pointer, a function, a channel, a fixed array.
	res := check_program('constraint_rules', 'constraint lower = int | f64

constraint Twice = int | int

constraint Itself = int | Itself

constraint Composite = []int | map[string]int | ?int | &int | fn () | chan int | [2]u8 | User

constraint Unknown = []Nope | map[string]Nope

constraint Named2 = int | f64

struct Named2 {}

struct Clash {}

constraint Clash = int

constraint Twin = int

constraint Twin = f64

constraint Res = !int | int

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'1:1: constraint `lower` must begin with capital letter',
		'3:26: constraint Twice cannot hold the type `int` more than once',
		'5:27: constraint cannot hold itself',
		// Once per line, at the type that holds it, as for a sum type.
		'9:22: unknown type `Nope`',
		'13:8: cannot register struct `Named2`, another type with this name exists',
		'17:1: cannot register constraint `Clash`, another type with this name exists',
		'21:1: cannot register constraint `Twin`, another type with this name exists',
		'23:18: a constraint cannot hold a Result type',
	], res.output
}

fn test_constraint_is_still_a_name() {
	res := check_program('name', "struct Rule {
	constraint string
}

fn main() {
	constraint := Rule{
		constraint: 'x'
	}
	println(constraint.constraint)
}
")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

fn test_a_program_with_constraints_builds_and_runs() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_run_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, constraint_prelude + "struct Box[T Named] {
	item T
}

fn (b Box[T]) label() string {
	return 'box of \${b.item.name}'
}

constraint Number = int | i64 | f64

fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn double[T Number](x T) T {
	return x + x
}

fn main() {
	u := longest(User{ name: 'Alex', age: 30 }, User{ name: 'Bo', age: 25 })
	println(u.age)
	println(double(21))
	println(double(1.25))
	println(Box[User]{ item: u }.label())
}
") or {
		panic(err)
	}
	res := os.execute('${os.quoted_path(@VEXE)} -new-compiler run ${os.quoted_path(path)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['30', '42', '2.5', 'box of Alex'], res.output
}
