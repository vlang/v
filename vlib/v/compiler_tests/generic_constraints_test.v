module main

import os
import v.cmdexec
import v.compiler_tests.method_form

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

// check_program checks `source` after the prelude, and its method form too (see
// same_with_methods).
fn check_program(name string, source string, flags ...string) os.Result {
	check := fn [flags] (name string, source string) os.Result {
		return check_program_form(name, source, flags)
	}
	res := check(name, source)
	same_with_methods(name, source, first_line, res, check)
	return res
}

fn check_program_form(name string, source string, flags []string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, constraint_prelude + source) or { panic(err) }
	return os.exec([@VEXE, '-check', '-nocolor', ...flags, path])
}

// check_fixture checks `source` after the prelude as the checker fixtures are
// checked, `-checker-fixture`, where each concrete instance of a generic
// function is checked in full.
fn check_fixture(name string, source string) os.Result {
	res := check_fixture_form(name, source)
	same_with_methods(name, source, first_line, res, check_fixture_form)
	return res
}

fn check_fixture_form(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_fixture_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, constraint_prelude + source) or { panic(err) }
	return os.exec([@VEXE, '-checker-fixture', '-check', '-nocolor', path])
}

// same_with_methods checks, with `check`, the method form of `source`: its generic
// functions written as methods of a struct without type parameters, whose own
// type parameters have to behave as those of the functions, with their
// constraints. It has to report the errors that `res` reports, where it moves
// them (see method_form.MethodForm). `first` is the line of the checked file
// where `source` starts. It returns false when `source` has no method form.
fn same_with_methods(name string, source string, first int, res os.Result, check fn (string, string) os.Result) bool {
	form := method_form.of(source) or { return false }
	method_res := check('${name}_methods', form.source)
	assert method_res.exit_code == res.exit_code, 'the method form of `${name}`:\n${method_res.output}'
	difference := form.differences(error_lines_from(res.output, first), error_lines_from(method_res.output,
		first))
	assert difference == '', 'the method form of `${name}`: ${difference}\n${method_res.output}'
	return true
}

// check_whole checks `source`, a whole program without the prelude, and its
// method form too (see same_with_methods).
fn check_whole(name string, source string) os.Result {
	res := check_whole_form(name, source)
	checked := same_with_methods(name, source, 1, res, check_whole_form)
	assert checked, '`${name}` declares no generic function'
	return res
}

fn check_whole_form(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_whole_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source) or { panic(err) }
	return os.exec([@VEXE, '-check', '-nocolor', path])
}

// run_program builds and runs `source`, a whole program without the prelude,
// and its method form too, which has to print what the program prints.
fn run_program(name string, source string) os.Result {
	res := run_program_form(name, source)
	form := method_form.of(source) or { panic('`${name}` declares no generic function') }
	method_res := run_program_form('${name}_methods', form.source)
	assert method_res.exit_code == res.exit_code, 'the method form of `${name}`:\n${method_res.output}'
	assert method_res.output == res.output, 'the method form of `${name}`:\n${method_res.output}'
	return res
}

fn run_program_form(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_run_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source) or { panic(err) }
	return os.exec([@VEXE, 'run', path])
}

// error_lines returns `line:col: message` for every error of a check output,
// with the line counted from the program's own first line.
fn error_lines(output string) []string {
	return error_lines_from(output, first_line)
}

// error_lines_from returns `line:col: message` for every error of a check
// output, with the line counted from the line `first` of the checked file.
fn error_lines_from(output string, first int) []string {
	mut lines := []string{}
	for line in output.split_into_lines() {
		if !line.contains(': error: ') {
			continue
		}
		location := line.all_before(': error: ').split(':')
		if location.len < 3 {
			continue
		}
		program_line := location[location.len - 2].int() - first + 1
		lines << '${program_line}:${location[location.len - 1]}: ${line.all_after(': error: ')}'
	}
	return lines
}

// A generic method can pass its type parameter to another method constrained
// by the same generic interface. smallest infers T from relay's U.
const constraint_method_relay = 'interface Comparable[T] {
	less(other T) bool
}

struct Num {
	v int
}

fn (a Num) less(b Num) bool {
	return a.v < b.v
}

struct Sorter {}

fn (s Sorter) smallest[T Comparable[T]](a T, b T) T {
	if a.less(b) {
		return a
	}
	return b
}

fn (s Sorter) relay[U Comparable[U]](a U, b U) U {
	return s.smallest(a, b)
}

fn check_method_relay() {
	println(Sorter{}.relay(Num{2}, Num{1}).v)
}
'

fn test_valid_constraint_calls_and_identifier_uses() {
	res := check_program('valid_constraint_calls', constraint_method_relay + '
fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn total[T Named](a T, items []T) int {
	x := a
	mut sum := x.name.len
	for y in items {
		sum += y.name.len
	}
	n := 5
	sum += n
	s := \'abc\'
	sum += s.len
	first := items[0]
	return sum + first.name.len
}

type Number = int | i64 | f64

fn double[T Number](x T) T {
	return x + x
}

fn describe[T Number](x T) string {
	return x.str()
}

type Part[T] = T | bool

type First = Part[int]

type Nested = Part[Part[int]] | Part[string]

type Aliased = Part[First] | Part[string]

fn accept[T Nested](value T) T {
	return value
}

fn check_finite_nested_constraints() {
	println(accept(1))
	println(accept("ok"))
	println(accept(true))
	println(accept_alias(1))
	println(accept_alias("ok"))
	println(accept_alias(true))
}

fn accept_alias[T Aliased](value T) T {
	return value
}

struct Rule {
	constraint string
}

fn check_constraint_identifier() {
	constraint := Rule{
		constraint: \'x\'
	}
	println(constraint.constraint)
}

fn main() {
	u := longest(User{ name: \'Alex\', age: 30 }, User{ name: \'Bob\', age: 25 })
	println(u.age)
	println(longest(Pet{ name: \'Rex\' }, Pet{ name: \'Tom\' }).name)

	println(total(User{ name: \'a\' }, [User{
		name: \'b\'
	}]))

	println(double(2))
	println(double(2.5))
	println(describe(i64(3)))
	check_method_relay()
	check_finite_nested_constraints()
	check_constraint_identifier()
}
')
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

fn test_constrained_bodies_reject_undeclared_members() {
	// Check values derived from constrained parameters in uncalled generic
	// functions. Values derived from U keep its Named constraint through
	// guards, calls and array methods, even when the callee uses T.
	// Raise the message limit so all 29 errors are printed.
	res := check_program('body_members', "fn derived[T Named](a T, b T, items []T, m map[string]T, c bool, n int) int {
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

type Number = int | f64

fn from_set[T Number](x T) int {
	y := x
	return y.len
}

fn oldest[T Named](a T, b T) T {
	if a.age > b.age {
		return a
	}
	println(a.nme)
	return b
}

fn label[T Named](x T) string {
	return x.greet()
}

fn find[T Named](items []T) ?T {
	return none
}

fn pair[T Named](a T) (T, int) {
	return a, 1
}

fn derived_from_calls[U Named](a U, items []U, m map[string]U) int {
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

fn pick[T Named](a T) T {
	return a
}

fn (b Box[T]) get() T {
	return b.item
}

// The caller has its own parameter T with a different constraint.
fn mixed[T Greeter, U Named](g T, n U, b Box[U]) int {
	x := pick(n)
	println(x.name)
	println(g.greet())
	y := b.get()
	println(y.name)
	return x.age + y.age + pick(n).age
}

fn main() {}
", '-message-limit', '100')
	assert res.exit_code == 1, res.output
	no_age := 'type `T` has no field named `age`: its constraint `Named` does not declare it'
	no_caller_age := 'type `U` has no field named `age`: its constraint `Named` does not declare it'
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
		'52:7: ${no_age}',
		'52:15: ${no_age}',
		'55:12: type `T` has no field named `nme`: its constraint `Named` does not declare it',
		'60:11: type `T` has no method `greet`: its constraint `Named` does not declare it',
		'74:14: ${no_caller_age}',
		'77:14: ${no_caller_age}',
		'80:17: ${no_caller_age}',
		'82:18: ${no_caller_age}',
		'84:13: ${no_caller_age}',
		'86:14: ${no_caller_age}',
		'89:14: ${no_caller_age}',
		'109:11: ${no_caller_age}',
		'109:19: ${no_caller_age}',
		'109:33: ${no_caller_age}',
	], res.output
}

fn test_operations_on_constrained_values_require_support_from_all_types() {
	// An interface declares no operators. Equality and membership are available
	// as for a value of the interface. A constraint set permits only operations
	// supported by every type in it, including types in an embedding family.
	// On arrays, items << x appends as a statement and shifts in an expression.
	// Leave room to report unexpected errors beyond the 20 expected here.
	res := check_program('constraint_operations', "type Number = int | f64

type Word = string | int

type Flag = bool | int

struct Vec {
	x int
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x}
}

type Addable = int | Vec

type IntArr = []int

type StrMap = map[string]int

type Indexable = string | IntArr

type Keyed = string | StrMap

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

type IntArr2 = []i64

type Ints = IntArr | IntArr2

fn push[T Ints](mut items T) {
	items << 1
	_ = items << 1
}

// User and types that embed it at any depth satisfy a User constraint.
struct Admin {
	User
	level int
}

struct Root {
	Admin
}

fn (a User) + (b User) User {
	return User{
		name: a.name + b.name
	}
}

fn show[T User](x T) string {
	return x.name + x.greet()
}

fn levels[T User](x T) int {
	return x.level
}

fn sum[T User](a T, b T) T {
	println(a == b)
	return a + b
}

type People = User | Pet

fn greet_named[T Named](x T) string {
	return x.name
}

fn only_people[T People](x T) string {
	return x.name
}

fn relay[T User](x T) string {
	return greet_named(x) + only_people(x)
}

fn narrowed[T User](a T, b T) bool {
	\$if T is Admin {
		return a == b
	} \$else {
		return a != b
	}
}

fn main() {
	println(show(User{ name: 'u' }))
	println(show(Admin{ User: User{ name: 'a' } }))
	println(show(Root{}))
	println(show(Pet{ name: 'p' }))
}
", '-message-limit', '100')
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
		'81:12: operator `<<` is not defined on type `T` and `int literal`: `IntArr`, in its constraint `Ints`, does not have it',
		'105:11: type `T` has no field named `level`: `User`, in its constraint `User`, does not have it',
		'110:11: operator `+` is not defined on type `T`: a struct that embeds `User` does not have it',
		'124:38: `only_people` needs `T` to be in `People`: a struct that embeds `User`, in its constraint `User`, is not',
		'139:15: cannot use `Pet` as `T`: it is not `User` and does not embed it',
	], res.output
}

fn test_comptime_and_runtime_type_checks_on_constrained_values() {
	// Compile-time conditions can narrow types and values. A condition such
	// as debug leaves the type set intact. Runtime type tests require a sum
	// type or interface value, and a match reports its first invalid type pattern.
	// Raise the message limit so all 21 errors are printed.
	res := check_program('constraint_type_checks', 'type Number = int | f64

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

fn narrow_value[T Number](x T, y T) T {
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

fn narrow_iface_value[T Named](a T) {
	$if a is User {
		println(a.age)
	} $else {
		println(a.age)
	}
}

type Shape = Square | Circle

struct Square {}

struct Circle {
	r f64
}

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

fn shapes[T Shape](s T) bool {
	return s is Circle
}

type Two = Named | Greeter

fn match_named[T Named](a T) {
	match a {
		User { println(a.age) }
		else {}
	}
}

fn match_shapes[T Shape](s T) f64 {
	return match s {
		Circle { s.r }
		else { 0.0 }
	}
}

fn two[T Two](x T) bool {
	return x is User
}

fn values[T Number](value T) string {
	return match value {
		1 { value.nme() }
		else { "" }
	}
}

fn main() {}
', '-message-limit', '100')
	assert res.exit_code == 1, res.output
	modulo_error := 'operator `%` is not defined on type `T`: `f64`, in its constraint `Number`, does not have it'
	shift_error := 'operator `<<` is not defined on type `T` and `int literal`: `f64`, in its constraint `Number`, does not have it'
	age_error := 'type `T` has no field named `age`: its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'5:13: ${modulo_error}',
		'10:13: type `T` has no field named `len`: `int`, in its constraint `Number`, does not have it',
		'21:13: ${shift_error}',
		'27:13: ${modulo_error}',
		'33:13: ${modulo_error}',
		'36:13: ${modulo_error}',
		'45:13: ${age_error}',
		'51:13: ${modulo_error}',
		'65:13: ${modulo_error}',
		'70:13: ${shift_error}',
		'77:13: ${modulo_error}',
		'86:13: ${age_error}',
		'99:5: `is` can only be used with sum type or interface values, not `T`: `int`, in its constraint `Number`, is neither; test `T` with `\$if value is f64`',
		'99:21: `is` can only be used with sum type or interface values, not `T`: `int`, in its constraint `Number`, is neither; test `T` with `\$if value is int`',
		'102:5: `is` can only be used with sum type or interface values, not `T`: `int`, in its constraint `Number`, is neither; test `T` with `\$if value !is f64`',
		'112:5: `is` can only be used with sum type or interface values, not `T`: a type that implements `Named` does not have to be one; test `T` with `\$if a is User`',
		'118:9: `is` can only be used with sum type or interface values, not `T`: `Square`, in its constraint `Shape`, is neither; test `T` with `\$if s is Circle`',
		'125:3: matching by type can only be done on sum type or interface values, not `T`: a type that implements `Named` does not have to be one; test `T` with `\$if a is User`',
		'132:3: matching by type can only be done on sum type or interface values, not `T`: `Square`, in its constraint `Shape`, is neither; test `T` with `\$if s is Circle`',
		'138:9: `is` can only be used with sum type or interface values, not `T`: a type that implements `Named`, in its constraint `Two`, does not have to be one; test `T` with `\$if x is User`',
		'143:13: type `T` has no method `nme`: `int`, in its constraint `Number`, does not have it',
	], res.output
}

fn test_generic_types_and_methods_propagate_required_constraints() {
	// A type argument's constraint must satisfy the constraint at its use.
	// When all type parameters are constrained, a method body is checked like
	// a function with both the receiver and method parameters.
	// Raise the message limit so all 29 errors are printed.
	res := check_program('constraint_propagation', "struct Box[T Named] {
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

type Number = int | f64

type Word = string | int

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

interface Shelf[T Named] {
	get() T
}

type Picker[T Named] = fn (T) T

struct Leaf[T] {
	v T
}

type Tree[T Named] = Leaf[T] | string

fn take[T](s Shelf[T]) T {
	return s.get()
}

fn take_ok[T Named](s Shelf[T]) T {
	return s.get()
}

interface BigShelf[T] {
	Shelf[T]
	count() int
}

interface BigShelfOk[T Named] {
	Shelf[T]
	count() int
}

struct Keeper[T] {
	p ?Picker[T]
}

struct KeeperOk[T NamedGreeter] {
	p ?Picker[T]
}

type Forest[T] = Tree[T] | int

type ForestOk[T Named] = Tree[T] | int

fn (s Shelf[T]) label[T]() string {
	return s.get().name
}

fn (s Shelf[T]) years[T]() int {
	return s.get().age
}

fn (t Tree[T]) describe[T](x T) string {
	return x.name
}

fn (t Tree[T]) years[T](x T) int {
	return x.age
}

// PairShelf constrains its receiver parameter. Query leaves it open.
fn takes_int(n int) int {
	return n
}

struct PairShelf[T Named] {
	items []T
}

fn (s PairShelf[T]) pair[U Named](u U) string {
	n := takes_int('s')
	return s.items[0].name + u.name + u.greet() + n.str()
}

fn pair_fn[T Named, U Named](s PairShelf[T], u U) string {
	n := takes_int('s')
	return s.items[0].name + u.name + u.greet() + n.str()
}

struct Query[T] {
	items []T
}

fn (q Query[T]) map[U Named](f fn (T) U) []U {
	n := takes_int('s')
	println(n)
	println(q.items.map(f(it).age))
	return q.items.map(f(it))
}

fn map_fn[T, U Named](q Query[T], f fn (T) U) []U {
	n := takes_int('s')
	println(n)
	println(q.items.map(f(it).age))
	return q.items.map(f(it))
}

fn to_user(x int) User {
	return User{
		name: 'u'
		age:  x
	}
}

fn to_int(x int) int {
	return x
}

fn main() {
	s := PairShelf[User]{
		items: [User{
			name: 'a'
		}]
	}
	println(s.pair(User{ name: 'b' }))
	println(pair_fn(s, User{ name: 'b' }))
	println(s.pair(3))
	println(pair_fn(s, 3))
	println(s.pair[int](3))
	println(pair_fn[User, int](s, 3))
	q := Query[int]{
		items: [1, 2]
	}
	println(q.map(to_user))
	println(map_fn(q, to_user))
	println(q.map(to_int))
	println(map_fn(q, to_int))
	println(q.map[int](to_int))
	println(map_fn[int, int](q, to_int))
}
", '-message-limit', '100')
	assert res.exit_code == 1, res.output
	no_age := 'type `T` has no field named `age`: its constraint `Named` does not declare it'
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
		'93:14: `Shelf[T]` needs `T` to implement `Named`: `T` has no constraint: give it one, `[T Named]`',
		'102:2: `Shelf[T]` needs `T` to implement `Named`: `T` has no constraint: give it one, `[T Named]`',
		'112:5: `Picker[T]` needs `T` to implement `Named`: `T` has no constraint: give it one, `[T Named]`',
		'119:18: `Tree[T]` needs `T` to implement `Named`: `T` has no constraint: give it one, `[T Named]`',
		'128:17: ${no_age}',
		'136:11: ${no_age}',
		'149:17: cannot use `string` as `int` in argument 1 to `takes_int`',
		'150:38: type `U` has no method `greet`: its constraint `Named` does not declare it',
		'154:17: cannot use `string` as `int` in argument 1 to `takes_int`',
		'155:38: type `U` has no method `greet`: its constraint `Named` does not declare it',
		'165:28: type `U` has no field named `age`: its constraint `Named` does not declare it',
		'172:28: type `U` has no field named `age`: its constraint `Named` does not declare it',
		"195:17: `int` doesn't implement field `name` of interface `Named`",
		"196:21: `int` doesn't implement field `name` of interface `Named`",
		"197:17: `int` doesn't implement field `name` of interface `Named`",
		"198:24: `int` doesn't implement field `name` of interface `Named`",
		"204:10: `int` doesn't implement field `name` of interface `Named`",
		"205:10: `int` doesn't implement field `name` of interface `Named`",
		"206:16: `int` doesn't implement field `name` of interface `Named`",
		"207:22: `int` doesn't implement field `name` of interface `Named`",
	], res.output
}

fn test_interface_constraints_check_calls_and_function_values() {
	// Comparable[T] is bound to the type argument itself, so Odd's less(int)
	// satisfies Comparable[int] and fails Comparable[Odd].
	res := check_program('interface_calls', "fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn welcome[T NamedGreeter](x T) string {
	return x.greet() + x.name
}

interface Comparable[T] {
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
	println(longest(1, 2))
	println(longest[User](User{ name: 'a' }, User{ name: 'b' }).age)
	println(longest[int](1, 2))
	println(welcome(User{ name: 'a' }))
	println(welcome(Pet{ name: 'b' }))

	good := longest[User]
	bad := longest[int]
	println(good(User{ name: 'a' }, User{ name: 'b' }).age)
	println(bad(1, 2))

	println(smallest(Num{1}, Num{2}).v)
	println(smallest(Odd{1}, Odd{2}).v)
	println(smallest(User{}, User{}).name)
	println(smallest(1, 2))
	println(below(Odd{1}, 5))
	println(below(Num{1}, 5))
	f := smallest[Odd]
	println(f)
}
")
	assert res.exit_code == 1, res.output
	no_name := "`int` doesn't implement field `name` of interface `Named`"
	odd_mismatch := '`Odd` incorrectly implements method `less` of interface `Comparable`: expected `Odd`, not `int` for parameter 1'
	assert error_lines(res.output) == [
		'33:12: type `T` has no field named `v`: its constraint `Comparable[T]` does not declare it',
		'47:7: ${odd_mismatch}',
		'55:18: `smallest` needs `U` to implement `Comparable[U]`: `U` has no constraint: give it one, `[U Comparable[U]]`',
		'59:18: ${no_name}',
		'61:18: ${no_name}',
		"63:18: `Pet` doesn't implement method `greet` of interface `NamedGreeter`",
		'66:17: ${no_name}',
		'71:19: ${odd_mismatch}',
		"72:19: `User` doesn't implement method `less` of interface `Comparable`",
		"73:19: `int` doesn't implement method `less` of interface `Comparable`",
		'75:16: `Num` incorrectly implements method `less` of interface `Comparable`: expected `int`, not `Num` for parameter 1',
		'76:16: ${odd_mismatch}',
	], res.output
}

// unwrap_name infers T from the return type of the container's get method.
const constraint_inference_declarations = 'interface Container[T] {
	get() T
}

struct InferenceUser {
	name string
}

struct InferenceBox[T] {
	item T
}

fn (b InferenceBox[T]) get() T {
	return b.item
}

fn unwrap_name[C Container[T], T Named](c C) string {
	return c.get().name
}
'

fn test_generic_constraints_on_functions_and_receiver_methods_build_and_run() {
	res := run_program('constrained_functions_and_methods', constraint_prelude +
		constraint_method_relay + constraint_inference_declarations + "
struct Shelf[T Named] {
	items []T
}

fn (s Shelf[T]) pair[U Named](u U) string {
	return s.items[0].name + '+' + u.name
}

fn pair_fn[T Named, U Named](s Shelf[T], u U) string {
	return s.items[0].name + '+' + u.name
}

struct Query[T] {
	items []T
}

fn (q Query[T]) map[U Named](f fn (T) U) []U {
	return q.items.map(f(it))
}

fn map_fn[T, U Named](q Query[T], f fn (T) U) []U {
	return q.items.map(f(it))
}

fn to_user(x int) User {
	return User{
		name: 'u\${x}'
		age:  x
	}
}

struct Box[T Named] {
	item T
}

fn (b Box[T]) label() string {
	return 'box of \${b.item.name}'
}

type Number = int | i64 | f64

fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn double[T Number](x T) T {
	return x + x
}

fn check_constraint_calls() {
	u := longest(User{ name: 'Alex', age: 30 }, User{ name: 'Bo', age: 25 })
	println(u.age)
	println(double(21))
	println(double(1.25))
	println(Box[User]{ item: u }.label())
}

interface ValueShelf[T Named] {
	get() T
}

struct UserShelf {
	u InferenceUser
}

fn (s UserShelf) get() InferenceUser {
	return s.u
}

fn take_now[T Named](s ValueShelf[T]) T {
	return s.get()
}

fn check_inferred_constraints() {
	println(unwrap_name(InferenceBox[InferenceUser]{InferenceUser{'ana'}}))
	shelf := UserShelf{InferenceUser{'bea'}}
	println(take_now(shelf).name)
	println(take_now[InferenceUser](shelf).name)
	println(take_now(ValueShelf[InferenceUser](shelf)).name)
}

fn main() {
	s := Shelf[User]{
		items: [User{
			name: 'a'
		}]
	}
	println(s.pair(Pet{ name: 'p' }))
	println(pair_fn(s, Pet{ name: 'p' }))
	println(s.pair[User](User{ name: 'b' }))
	println(pair_fn[User, User](s, User{ name: 'b' }))
	q := Query[int]{
		items: [1, 2]
	}
	println(q.map(to_user).map(it.name))
	println(map_fn(q, to_user).map(it.name))
	println(q.map[User](to_user).map(it.age))
	println(map_fn[int, User](q, to_user).map(it.age))
	check_constraint_calls()
	check_method_relay()
	check_inferred_constraints()
}
")
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == [
		'a+p',
		'a+p',
		'a+b',
		'a+b',
		"['u1', 'u2']",
		"['u1', 'u2']",
		'[1, 2]',
		'[1, 2]',
		'30',
		'42',
		'2.5',
		'box of Alex',
		'1',
		'ana',
		'bea',
		'bea',
		'bea',
	], res.output
}

fn test_constrained_generic_type_arguments_are_checked() {
	// Structs, interfaces, aliases, and sum types check their type arguments
	// where they are written or inferred. Using PetTree as a constraint also
	// checks its type argument and admits only the variants of that instance.
	res := check_program('generic_type_arguments', "struct Box[T Named] {
	item T
}

fn (b Box[T]) label() string {
	return b.item.name
}

struct BoxShelf {
	good Box[User]
	bad  Box[int]
}

fn show(b Box[int]) string {
	return ''
}

fn keep(b Box[User]) Box[User] {
	return b
}

interface Shelf[T Named] {
	get() T
}

type Picker[T Named] = fn (T) T

struct Leaf[T] {
	v T
}

struct Twig[T] {
	l T
}

type Tree[T Named] = Leaf[T] | Twig[T]

struct IntShelf {}

fn (s IntShelf) get() int {
	return 1
}

struct Holder {
	shelf  Shelf[int]
	picker ?Picker[int]
	tree   Tree[int]
	good   Tree[User]
}

type Forest = Tree[int] | string

fn use_shelf(s Shelf[int]) int {
	return s.get()
}

fn make_picker() Picker[int] {
	return fn (x int) int {
		return x
	}
}

fn good_types(s Shelf[User], p Picker[User], t Tree[User]) {}

type PetTree[T Named] = Leaf[T] | Pet

fn reject_member[U PetTree[int]](x U) U {
	return x
}

fn accept_member[U PetTree[User]](x U) U {
	return x
}

fn main() {
	println(Box[User]{ item: User{ name: 'a' } }.label())
	println(Box[int]{ item: 1 }.label())

	good := Box{
		item: User{ name: 'a' }
	}
	bad := Box{
		item: 1
	}
	println('\${good.item.name} \${bad}')

	println(use_shelf(Shelf[int](IntShelf{})))
	println(sizeof(Picker[int]))
	trees := []Tree[int]{}
	shelves := map[string]Shelf[int]{}
	println(trees.len + shelves.len)

	println(accept_member(Leaf[User]{}).v.name)
	println(accept_member(Pet{}).name)
	println(accept_member(1))
}
")
	assert res.exit_code == 1, res.output
	no_name := "`int` doesn't implement field `name` of interface `Named`"
	assert error_lines(res.output) == [
		'11:7: ${no_name}',
		'14:11: ${no_name}',
		'45:9: ${no_name}',
		'46:10: ${no_name}',
		'47:9: ${no_name}',
		'51:15: ${no_name}',
		'53:16: ${no_name}',
		'57:18: ${no_name}',
		'67:20: ${no_name}',
		'77:10: ${no_name}',
		'82:9: ${no_name}',
		'87:20: ${no_name}',
		'88:17: ${no_name}',
		'89:13: ${no_name}',
		'90:24: ${no_name}',
		'95:24: cannot use `int` as `U`: it is not in its constraint `PetTree[User]`',
	], res.output
}

fn test_closures_and_array_methods_respect_generic_constraints() {
	// Closure parameters and array callbacks use the constraint of their input.
	// Nested array methods each bind `it` to their own element type.
	res := check_program('constraint_closures', 'fn labels[T Named](xs []T) []string {
	f := fn (x T) string {
		return x.name + x.age.str()
	}
	return xs.map(f)
}

fn ages[T Named](xs []T) []int {
	return xs.map(fn (x T) int {
		return x.age
	})
}

fn its[T Named](xs []T) []int {
	println(xs.filter(it.name.len > 0).len)
	return xs.map(it.age)
}

fn sorted_ages[T Named](mut xs []T) {
	xs.sort(a.age < b.age)
}

fn lambdas[T Named](xs []T) []int {
	println(xs.map(|x| x.name))
	return xs.map(|x| x.age)
}

fn apply[U](xs []U, f fn (U) int) []int {
	mut out := []int{}
	for x in xs {
		out << f(x)
	}
	return out
}

fn twice(f fn (int) int) int {
	return f(2)
}

fn nested[T Named](xs []T) int {
	return xs.filter(it.name.len > 0 && [1, 2].any(it > 0) && it.age > 0).len
}

fn sorted_by_age[T Named](xs []T) []T {
	return xs.sorted(|p, q| p.age < q.age)
}

fn tests[T Named](xs []T) {
	println(xs.any(it.age > 1))
	println(xs.all(it.age > 1))
	println(xs.count(it.age > 1))
}

fn pointers[T Named](x &T) int {
	f := fn (y &T) int {
		return y.age
	}
	return f(x)
}

fn inside[T Named](x T) int {
	outer := fn (y T) int {
		inner := fn (z T) int {
			return z.age
		}
		return inner(y)
	}
	return outer(x)
}

fn fine[T Named](xs []T, users []User) {
	println([1, 2].map(it * 2))
	println(users.map(it.age))
	g := fn (u User) int {
		return u.age
	}
	println(users.map(g))
	println(twice(|n| n * 2))
	println(apply(xs, |x| x.name.len))
}

fn given[T Named](xs []T) []int {
	return apply(xs, |x| x.age)
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	no_age := 'type `T` has no field named `age`: its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'3:21: ${no_age}',
		'10:12: ${no_age}',
		'16:19: ${no_age}',
		'20:12: ${no_age}',
		'20:20: ${no_age}',
		'25:22: ${no_age}',
		'41:63: ${no_age}',
		'45:28: ${no_age}',
		'45:36: ${no_age}',
		'49:20: ${no_age}',
		'50:20: ${no_age}',
		'51:22: ${no_age}',
		'56:12: ${no_age}',
		'64:13: ${no_age}',
		'83:25: ${no_age}',
	], res.output
}

fn test_an_instance_the_constraint_rejects_is_reported_at_its_call_only() {
	// Where each instance is checked in full, as in the checker fixtures, the
	// body of one whose type argument does not satisfy the constraint repeats
	// the error of the call in errors of its own: those are left out.
	res := check_fixture('rejected_instances', "struct Rock {
	weight int
}

type Number = int | f64

fn say[T Greeter](x T) string {
	return x.greet()
}

fn relay[T Greeter](x T) string {
	return say(x)
}

fn label[T Named](x T) string {
	return 'n: \${x.name}'
}

struct Box[T Named] {
	item T
}

fn wrap[T Named](x T) Box[T] {
	return Box[T]{
		item: x
	}
}

fn twice[T Number](x T) T {
	return x + x
}

fn main() {
	println(say(Pet{}))
	println(relay(Rock{}))
	println(label(Rock{}))
	println(wrap(Rock{}))
	println(twice('a'))
	println(say(User{}))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		"34:14: `Pet` doesn't implement method `greet` of interface `Greeter`",
		"35:16: `Rock` doesn't implement method `greet` of interface `Greeter`",
		"36:16: `Rock` doesn't implement field `name` of interface `Named`",
		"37:15: `Rock` doesn't implement field `name` of interface `Named`",
		'38:16: cannot use `string` as `T`: it is not in its constraint `Number`',
	], res.output
}

fn test_generic_type_arguments_in_declarations_and_expressions_are_checked() {
	// Each invalid application is reported where it is written.
	// The corresponding uses of Box[User] are valid.
	// Raise the message limit so all 23 errors are printed.
	res := check_program('written_type_arguments', 'struct Box[T Named] {
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
', '-message-limit', '100')
	assert res.exit_code == 1, res.output
	int_without_name := "`int` doesn't implement field `name` of interface `Named`"
	assert error_lines(res.output) == [
		'5:20: ${int_without_name}',
		'10:8: ${int_without_name}',
		'11:8: ${int_without_name}',
		'16:2: ${int_without_name}',
		'17:12: ${int_without_name}',
		"18:11: `Box[User]` doesn't implement field `name` of interface `Named`",
		'21:9: ${int_without_name}',
		'25:15: ${int_without_name}',
		'27:17: ${int_without_name}',
		'34:15: ${int_without_name}',
		'36:19: ${int_without_name}',
		'37:15: ${int_without_name}',
		'38:15: ${int_without_name}',
		'39:15: ${int_without_name}',
		'40:18: ${int_without_name}',
		'41:18: ${int_without_name}',
		'42:9: ${int_without_name}',
		'43:10: ${int_without_name}',
		'44:13: ${int_without_name}',
		'45:15: ${int_without_name}',
		'45:25: ${int_without_name}',
		'48:12: ${int_without_name}',
		"49:22: `Box[User]` doesn't implement field `name` of interface `Named`",
	], res.output
}

fn test_type_aliases_and_sum_types_as_constraints() {
	// Ints requires the alias itself. Number, Real, and Both admit their sum
	// variants, including variants reached through nested and generic sum types.
	res := check_program('alias_constraints', "type Ints = []int

fn ints[T Ints](x T) int {
	return x.len
}

fn nope[T Nope](x T) T {
	return x
}

type Number = int | f64

type Integer = int | i64

type Float = f32 | f64

type Real = Integer | Float

fn double[T Number](x T) T {
	return x + x
}

fn describe[T Number](x T) string {
	return x.str() + x.hex()
}

fn keep(n Number) Number {
	return n
}

fn half[T Real](x T) T {
	return x / 2
}

type Bad = int | Foo

fn keep_bad[T Bad](x T) T {
	return x
}

type Part[T] = T | bool

type Both = Part[int] | Part[string]

fn accept[T Both](value T) T {
	return value
}

fn main() {
	println(ints(Ints([1, 2])))
	println(ints([1, 2]))
	println(double(2))
	println(double(1.5))
	println(double('a'))
	println(keep(Number(1)))
	println(half(i64(4)))
	println(half(f32(1)))
	println(half(u8(1)))
	println(accept(1.5))
}
")
	assert res.exit_code == 1, res.output
	// The unknown sum variant is reported once, where Bad is declared.
	assert error_lines(res.output) == [
		'7:11: unknown type `Nope`',
		'24:21: type `T` has no method `hex`: `f64`, in its constraint `Number`, does not have it',
		'35:18: unknown type `Foo`',
		'51:15: cannot use `[]int` as `T`: it is not in its constraint `Ints`',
		'54:17: cannot use `string` as `T`: it is not in its constraint `Number`',
		'58:15: cannot use `u8` as `T`: it is not in its constraint `Real`',
		'59:17: cannot use `f64` as `T`: it is not in its constraint `Both`',
	], res.output
}

fn test_a_nested_sum_constraint_keeps_distinct_generic_instances() {
	// Each variant order starts with fresh generic instances.
	for name, variants in {
		'int_first':    'Part[int] | Part[string]'
		'string_first': 'Part[string] | Part[int]'
	} {
		res := check_whole('sum_instances_${name}', "module main

type Part[T] = T | bool

type Both = ${variants}

fn accept[T Both](value T) T {
	return value
}

fn main() {
	println(accept(1))
	println(accept('ok'))
	println(accept(true))
}
")
		assert res.exit_code == 0, res.output
	}
}

fn test_generic_sum_constraints_reject_recursive_expansion() {
	res := check_whole('sum_instances_recursive', 'module main

type Expanding[T] = T | Expanding[[]T]

fn accept_direct[T Expanding[int]](value T) T {
	return value
}

type MutualFirst[T] = T | MutualSecond[[]T]
type MutualSecond[T] = T | MutualFirst[[]T]

fn accept_mutual[T MutualFirst[int]](value T) T {
	return value
}

type Part[T] = T | bool

type ArgumentGrowing[T] = T | Part[ArgumentGrowing[[]T]]

fn accept_argument[T ArgumentGrowing[int]](value T) T {
	return value
}

fn main() {
	println(accept_direct(1))
	println(accept_mutual(1))
	println(accept_argument(1))
}
')
	assert res.exit_code == 1, res.output
	// The layout paths are incidental to the recursion and constraint checks.
	errors := error_lines_from(res.output, 1).map(it.all_before('; inline cycle:'))
	assert errors == [
		'3:25: sum type cannot hold itself',
		'5:20: constraint `Expanding[int]` cannot expand a recursive sum type',
		'9:27: sum type `MutualFirst` cannot be defined recursively',
		'10:28: sum type `MutualSecond` cannot be defined recursively',
		'12:20: constraint `MutualFirst[int]` cannot expand a recursive sum type',
		'18:31: sum type `ArgumentGrowing` cannot be defined recursively',
		'20:22: constraint `ArgumentGrowing[int]` cannot expand a recursive sum type',
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

// check_shapes checks a program of a module `shapes`, with its interface `Named`
// and then `decls`, and a `main` of `body` that imports it.
fn check_shapes(name string, decls string, body string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'shapes')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'shapes', 'shapes.v'), 'module shapes

pub interface Named {
	name string
}

' + decls) or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import shapes

struct City {
	name string
}

fn main() {
' + body + '}
') or {
		panic(err)
	}
	// Several parser workers, as a machine with more cores runs it.
	return cmdexec.run_in('env', ['VJOBS=4', @VEXE, '-check', '-nocolor', '.'], dir)
}

fn test_a_constraint_from_another_module_is_checked_at_the_call() {
	res := check_shapes('modules', 'pub fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

pub struct Host {}

pub fn (h Host) longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}
',
		"	println(shapes.longest(City{ name: 'Lima' }, City{ name: 'Oslo' }).name)
	println(shapes.longest(1, 2))
	h := shapes.Host{}
	println(h.longest(City{ name: 'Lima' }, City{ name: 'Oslo' }).name)
	println(h.longest(1, 2))
")
	assert res.exit_code == 1, res.output
	errors := res.output.split_into_lines().filter(it.contains(': error: '))
	no_name := "`int` doesn't implement field `name` of interface `Named`"
	assert errors == ['main.v:11:25: error: ${no_name}', 'main.v:14:20: error: ${no_name}'], res.output
}

fn test_type_sets_and_interface_aliases_check_calls_and_members() {
	// A generic body can use only the members available on every type in
	// its constraint. An interface alias accepts implementations of the interface.
	res := check_program('set_rejections', "type Sized = []int | []string | map[string]int | string

type Listy = []int | []string

type Value = Named

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

type Animal = User | Pet

fn label[T Animal](x T) string {
	return x.name + x.greet()
}

type Number = int | i64 | f64

fn double[T Number](x T) T {
	return x + x
}

type NamedOrInt = Named | int

fn describe[T NamedOrInt](x T) string {
	\$if T is User {
		return x.name + x.greet()
	} \$else {
		return ''
	}
}

interface ThingThatHasName {
	name string
}

type NameAlias = ThingThatHasName

fn show_aliased[T NameAlias](x T) string {
	return x.name
}

fn main() {
	println(double('a'))

	println(describe(User{ name: 'u' }))
	println(describe(Pet{ name: 'p' }))
	println(describe(3))
	println(describe(1.5))
	println(show_aliased(1))
	println(show_aliased(User{ name: 'Alex' }))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'12:11: type `T` has no field named `cap`: `map[string]int`, in its constraint `Sized`, does not have it',
		'28:11: type `T` has no field named `age`: its constraint `Value` does not declare it',
		'38:20: type `T` has no method `greet`: `Pet`, in its constraint `Animal`, does not have it',
		'68:17: cannot use `string` as `T`: it is not in its constraint `Number`',
		'73:19: cannot use `f64` as `T`: it is not in its constraint `NamedOrInt`',
		"74:23: `int` doesn't implement field `name` of interface `ThingThatHasName`",
	], res.output
}

fn read_format_thousands_fixture() string {
	return os.read_file(os.join_path(os.dir(@FILE), 'testdata', 'format_thousands_28797.v')) or {
		panic(err)
	}
}

fn test_the_thousands_formatting_of_28797_takes_a_constraint() {
	// vlang/v#28797 rejects the types that are not numbers with `$if T !in
	// [...] { $compile_error(...) }`. With `[T Number]` in its place, its code
	// builds and runs, and its examples and tests hold.
	res := run_program('28797', read_format_thousands_fixture())
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().ends_with('1.234.567.891.42'), res.output
}

fn test_inferred_type_arguments_must_satisfy_their_constraints() {
	declarations := constraint_prelude.all_after('module main\n\n') + '\n' +
		constraint_inference_declarations
	source := read_format_thousands_fixture().replace('\nfn main() {\n', '\n${declarations}\nfn main() {\n')
	last := "\tprintln(format_thousands(1234567891.42, '.'))"
	format_line := source.split_into_lines().index(last) + 2
	assert format_line > 1
	bad_format := "\tprintln(format_thousands('1234', ','))"
	// InferenceBox[int] binds T to int, so the call fails the Named constraint.
	bad_inference := '\tprintln(unwrap_name(InferenceBox[int]{1}))'
	check := check_whole('inferred_rejections', source.replace(last, '${last}\n${bad_format}\n${bad_inference}'))
	assert check.exit_code == 1, check.output
	assert error_lines_from(check.output, 1) == [
		'${format_line}:27: cannot use `string` as `T`: it is not in its constraint `Number`',
		"${format_line + 1}:10: `int` doesn't implement field `name` of interface `Named`",
	], check.output
}
