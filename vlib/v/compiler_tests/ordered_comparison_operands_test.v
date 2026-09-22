// Tests for the operands of `<`, `>`, `<=` and `>=`. They need an order in
// common: without one, the C backend emits a comparison that the C compiler
// rejects (structs, maps, sum types, thread handles...) or one that compiles
// and means nothing (bools, enums, fn values or channels against numbers).
// The expected messages are the ones V1 reports for the same code, except for
// the comparisons marked below that V1 let through to the C compiler.
import os
import rand

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')
const ordered_v3_bin = os.join_path(os.temp_dir(), 'v3_ordered_comparison_test_${os.getpid()}')

const ordered_ops = ['<', '>', '<=', '>=']

const prelude = "module main

import time

struct Foo {
	x int
}

struct Op {
	x int
}

fn (a Op) < (b Op) bool {
	return a.x < b.x
}

type OpAlias = Op

type Sum = int | string

interface Speaker {
	speak() string
}

struct Dog {}

fn (d Dog) speak() string {
	return 'woof'
}

enum Color {
	red
	green
}

@[flag]
enum Perm {
	read
	write
}

type MyInt = int
type MyStr = string
type FooAlias = Foo

type Money = int

fn (a Money) < (b Money) bool {
	return int(a) < int(b)
}

fn epoch() time.Time {
	return time.unix(0)
}
"

// Operand is one side of a generated comparison: a parameter of type `decl`,
// or the literal `text` when `decl` is empty. Two operands of the same
// non-empty `group` can be ordered. An `integer` also orders with a pointer,
// as pointer arithmetic, and a `c_scalar` with a `voidptr`, which the C
// compiler compares with pointers and scalars; any other pair cannot.
struct Operand {
	key      string
	decl     string
	text     string
	group    string
	integer  bool
	c_scalar bool
}

fn (o Operand) expr(name string) string {
	return if o.decl == '' { o.text } else { name }
}

fn (o Operand) is_pointer() bool {
	return o.key in ['int_pointer', 'voidptr']
}

fn comparison_is_valid(lhs Operand, rhs Operand) bool {
	if lhs.group != '' && lhs.group == rhs.group {
		return true
	}
	if lhs.key == 'voidptr' {
		return rhs.is_pointer() || rhs.c_scalar
	}
	if rhs.key == 'voidptr' {
		// `bool < x` is rejected for any `x`.
		return lhs.is_pointer() || (lhs.c_scalar && lhs.key !in ['bool', 'bool_literal'])
	}
	return (lhs.is_pointer() && rhs.integer) || (rhs.is_pointer() && lhs.integer)
}

const operands = [
	Operand{
		key:      'int'
		decl:     'int'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:      'i64'
		decl:     'i64'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:      'u8'
		decl:     'u8'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:   'f64'
		decl:  'f64'
		group: 'number'
	},
	Operand{
		key:      'rune'
		decl:     'rune'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:      'int_alias'
		decl:     'MyInt'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:      'int_literal'
		text:     '1'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:      'zero_literal'
		text:     '0'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:   'float_literal'
		text:  '1.5'
		group: 'number'
	},
	Operand{
		key:      'rune_literal'
		text:     '`a`'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:      'char'
		decl:     'char'
		group:    'number'
		integer:  true
		c_scalar: true
	},
	Operand{
		key:   'int_pointer'
		decl:  '&int'
		group: 'int_pointer'
	},
	Operand{
		key:   'voidptr'
		decl:  'voidptr'
		group: 'voidptr'
	},
	Operand{
		key:   'string'
		decl:  'string'
		group: 'string'
	},
	Operand{
		key:   'string_alias'
		decl:  'MyStr'
		group: 'string'
	},
	Operand{
		key:   'string_literal'
		text:  "'a'"
		group: 'string'
	},
	Operand{
		key:   'struct_with_lt'
		decl:  'Op'
		group: 'op'
	},
	Operand{
		key:   'struct_with_lt_alias'
		decl:  'OpAlias'
		group: 'op'
	},
	Operand{
		key:   'time'
		decl:  'time.Time'
		group: 'time'
	},
	Operand{
		key:      'chan'
		decl:     'chan int'
		group:    'chan'
		c_scalar: true
	},
	Operand{
		key:      'bool'
		decl:     'bool'
		c_scalar: true
	},
	Operand{
		key:      'bool_literal'
		text:     'true'
		c_scalar: true
	},
	Operand{
		key:  'thread'
		decl: 'thread int'
	},
	Operand{
		key:  'struct'
		decl: 'Foo'
	},
	Operand{
		key:  'struct_alias'
		decl: 'FooAlias'
	},
	Operand{
		key:  'map'
		decl: 'map[string]int'
	},
	Operand{
		key:  'array'
		decl: '[]int'
	},
	Operand{
		key:  'fixed_array'
		decl: '[3]int'
	},
	Operand{
		key:  'sum'
		decl: 'Sum'
	},
	Operand{
		key:  'interface'
		decl: 'Speaker'
	},
	Operand{
		key:      'enum'
		decl:     'Color'
		c_scalar: true
	},
	Operand{
		key:      'flag_enum'
		decl:     'Perm'
		c_scalar: true
	},
	Operand{
		key:  'fn'
		decl: 'fn () int'
	},
]

struct MatrixCase {
	line  int
	name  string
	valid bool
}

// ErrorAt is an error the checker reports: its file, line, column and message.
struct ErrorAt {
	file string
	line int
	col  int
	msg  string
}

fn setup_v3_cache() {
	cache_dir := os.join_path(os.temp_dir(), 'v3_ordered_comparison_cache_${os.getpid()}')
	if os.getenv('V3CACHE') == cache_dir {
		return
	}
	os.rmdir_all(cache_dir) or {}
	os.rm(ordered_v3_bin) or {}
	os.setenv('V3CACHE', cache_dir, true)
}

// build_v3 builds the compiler under test once per test process.
fn build_v3() string {
	setup_v3_cache()
	if os.is_executable(ordered_v3_bin) {
		return ordered_v3_bin
	}
	build :=
		os.execute('${os.quoted_path(vexe)} -gc none -path ${os.quoted_path('${vlib_dir}|@vlib|@vmodules')} -o ${os.quoted_path(ordered_v3_bin)} ${os.quoted_path(v3_src)}')
	assert build.exit_code == 0, build.output
	return ordered_v3_bin
}

fn unique_temp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_ordered_${name}_${os.getpid()}_${rand.ulid()}')
}

fn check_errors(name string, src string) []ErrorAt {
	path := unique_temp_path(name) + '.v'
	os.write_file(path, src) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	errors := check_file_errors(path)
	for err in errors {
		assert err.file == path, 'error in another file: ${err}'
	}
	return errors
}

// check_file_errors runs the checker alone over the file at `path` and returns
// every error it reports, uncapped, in whichever file it is.
fn check_file_errors(path string) []ErrorAt {
	v3_bin := build_v3()
	result :=
		os.execute('${os.quoted_path(v3_bin)} -nocache -check -nocolor -checker-fixture ${os.quoted_path(path)}')
	mut errors := []ErrorAt{}
	for line in result.output.split_into_lines() {
		if !line.contains(': error: ') {
			continue
		}
		location := line.all_before(': error: ').split(':')
		if location.len < 3 {
			continue
		}
		errors << ErrorAt{
			file: location[..location.len - 2].join(':')
			line: location[location.len - 2].int()
			col:  location[location.len - 1].int()
			msg:  line.all_after(': error: ')
		}
	}
	assert errors.len > 0 || result.exit_code == 0, 'no error lines in a failed check:\n${result.output}'
	return errors
}

fn run_good(name string, src string) string {
	v3_bin := build_v3()
	out := unique_temp_path(name)
	good_src := out + '.v'
	os.write_file(good_src, src) or { panic(err) }
	defer {
		os.rm(good_src) or {}
		os.rm(out) or {}
	}
	compile :=
		os.execute('${os.quoted_path(v3_bin)} -nocache ${os.quoted_path(good_src)} -b c -o ${os.quoted_path(out)}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

// comparison_matrix returns a program with one `return lhs op rhs` per pair of
// operands and operator, and where each of them is.
fn comparison_matrix(ops []string) (string, []MatrixCase) {
	mut lines := prelude.split_into_lines()
	mut cases := []MatrixCase{}
	for li, lhs in operands {
		for ri, rhs in operands {
			for oi, op in ops {
				mut params := []string{}
				if lhs.decl != '' {
					params << 'a ${lhs.decl}'
				}
				if rhs.decl != '' {
					params << 'b ${rhs.decl}'
				}
				lines << 'fn case_${li}_${ri}_${oi}(${params.join(', ')}) bool {'
				lines << '\treturn ${lhs.expr('a')} ${op} ${rhs.expr('b')}'
				cases << MatrixCase{
					line:  lines.len
					name:  '${lhs.key} ${op} ${rhs.key}'
					valid: comparison_is_valid(lhs, rhs)
				}
				lines << '}'
				lines << ''
			}
		}
	}
	lines << 'fn main() {}'
	return lines.join('\n') + '\n', cases
}

fn test_ordered_comparisons_need_operands_with_a_common_order() {
	src, cases := comparison_matrix(ordered_ops)
	errors := check_errors('matrix', src)
	mut lines_with_errors := map[int][]string{}
	for err in errors {
		mut msgs := lines_with_errors[err.line] or { []string{} }
		msgs << err.msg
		lines_with_errors[err.line] = msgs
	}
	mut case_lines := map[int]bool{}
	mut wrong := []string{}
	for c in cases {
		case_lines[c.line] = true
		msgs := lines_with_errors[c.line] or { []string{} }
		if c.valid && msgs.len > 0 {
			wrong << '${c.name}: rejected: ${msgs}'
		} else if !c.valid && msgs.len == 0 {
			wrong << '${c.name}: accepted'
		}
	}
	for err in errors {
		assert err.line in case_lines, 'error outside the generated comparisons: ${err}'
	}
	assert wrong.len == 0, '${wrong.len} of ${cases.len} comparisons got the wrong verdict:\n${wrong.join('\n')}'
}

// MessageCase is a function whose body is `return <expr>`, and the errors the
// checker has to report on that line, as `col: message`.
struct MessageCase {
	params   string
	expr     string
	expected []string
}

const infix_bool_msg = 'bool types only have the following operators defined: `==`, `!=`, `||`, and `&&`'

const message_cases = [
	MessageCase{'t thread int', 't > 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `thread int`',
	]},
	MessageCase{'t thread int', '0 < t', [
		'9: infix expr: cannot use `thread int` (right expression) as `int literal`',
	]},
	MessageCase{'t thread int, n int', 't <= n', [
		'9: infix expr: cannot use `int` (right expression) as `thread int`',
	]},
	MessageCase{'c chan int', 'c >= 1', [
		'9: infix expr: cannot use `int literal` (right expression) as `chan int`',
	]},
	MessageCase{'a Foo', 'a <= 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `Foo`',
	]},
	// An alias without an ordering of its own is named after the type it stands for.
	MessageCase{'a FooAlias', 'a > 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `Foo`',
	]},
	MessageCase{'n int, s MyStr', 'n < s', [
		'9: infix expr: cannot use `string` (right expression) as `int`',
	]},
	MessageCase{'o OpAlias', 'o >= 1', [
		'9: infix expr: cannot use `int literal` (right expression) as `Op`',
	]},
	MessageCase{'m Money, s string', 'm < s', [
		'9: infix expr: cannot use `string` (right expression) as `Money`',
	]},
	MessageCase{'m map[string]int', 'm < 1', [
		'9: infix expr: cannot use `int literal` (right expression) as `map[string]int`',
	]},
	MessageCase{'a []int', 'a > 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `[]int`',
	]},
	MessageCase{'a [3]int', 'a > 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `[3]int`',
	]},
	MessageCase{'s Sum', 's > 0', [
		'11: cannot use operator `>` with `Sum`',
		'9: infix expr: cannot use `int literal` (right expression) as `Sum`',
	]},
	MessageCase{'n int, s Sum', 'n < s', [
		'11: cannot use operator `<` with `Sum`',
		'9: infix expr: cannot use `Sum` (right expression) as `int`',
	]},
	MessageCase{'s Sum, t Sum', 's < t', [
		'11: cannot use operator `<` with `Sum`',
	]},
	MessageCase{'i Speaker', 'i < 1', [
		'9: infix expr: cannot use `int literal` (right expression) as `Speaker`',
	]},
	MessageCase{'f fn () int', 'f > 0', [
		'9: mismatched types `fn () int` and `int literal`',
		'9: infix expr: cannot use `int literal` (right expression) as `fn () int`',
	]},
	MessageCase{'b bool', 'b > false', [
		'11: ${infix_bool_msg}',
	]},
	MessageCase{'b bool', 'b < 1', [
		'11: ${infix_bool_msg}',
		'9: infix expr: cannot use `int literal` (right expression) as `bool`',
	]},
	MessageCase{'n int, b bool', 'n >= b', [
		'9: infix expr: cannot use `bool` (right expression) as `int`',
	]},
	MessageCase{'a int, b int, c int', 'a < b < c', [
		'15: ${infix_bool_msg}',
		'9: infix expr: cannot use `int` (right expression) as `bool`',
	]},
	MessageCase{'c Color', 'c > 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `Color`',
	]},
	MessageCase{'s string', 's > 1', [
		'9: infix expr: cannot use `int literal` (right expression) as `string`',
	]},
	MessageCase{'r rune', "r > 'a'", [
		'9: infix expr: cannot use `string` (right expression) as `rune`',
	]},
	MessageCase{'o Op', 'o >= 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `Op`',
	]},
	MessageCase{'t time.Time', 't < 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `time.Time`',
	]},
	// A struct with its own `<` still takes an operand of its own type.
	MessageCase{'o Op, f Foo', 'o < f', [
		'9: mismatched types `Op` and `Foo`',
		'9: infix expr: cannot use `Foo` (right expression) as `Op`',
	]},
	MessageCase{'o Op, f Foo', 'o > f', [
		'9: infix expr: cannot use `Foo` (right expression) as `Op`',
	]},
	MessageCase{'o Op, f FooAlias', 'o < f', [
		'9: mismatched types `Op` and `FooAlias`',
		'9: infix expr: cannot use `Foo` (right expression) as `Op`',
	]},
	MessageCase{'o Op, t thread int', 'o < t', [
		'9: infix expr: cannot use `thread int` (right expression) as `Op`',
	]},
	MessageCase{'o Op, t time.Time', 'o < t', [
		'9: infix expr: cannot use `time.Time` (right expression) as `Op`',
	]},
	MessageCase{'t time.Time, o Op', 't >= o', [
		'9: infix expr: cannot use `Op` (right expression) as `time.Time`',
	]},
	MessageCase{'p Perm', 'p < 1', [
		'9: infix expr: cannot use `int literal` (right expression) as `Perm`',
	]},
	MessageCase{'n int, p Perm', 'n < p', [
		'9: infix expr: cannot use `Perm` (right expression) as `int`',
	]},
	MessageCase{'r rune, f f64', 'r < f', []string{}},
	MessageCase{'n int, c char', 'n > c', []string{}},
	MessageCase{'c char', 'c <= 1.5', []string{}},
	MessageCase{'p voidptr, c chan int', 'p < c', []string{}},
	MessageCase{'p voidptr, b bool', 'p >= b', []string{}},
	// V1 took a `voidptr` for anything; the C compiler orders it only against
	// pointers and scalars.
	MessageCase{'p voidptr, f Foo', 'p < f', [
		'9: infix expr: cannot use `Foo` (right expression) as `voidptr`',
	]},
	MessageCase{'p voidptr', 'p > 1.5', [
		'9: infix expr: cannot use `float literal` (right expression) as `voidptr`',
	]},
	MessageCase{'s string, p voidptr', 's <= p', [
		'9: infix expr: cannot use `voidptr` (right expression) as `string`',
	]},
	MessageCase{'p &int, n i8', 'p < n', []string{}},
	MessageCase{'o Op, p OpAlias', 'o <= p', []string{}},
	MessageCase{'a [3]int, b [3]int', 'a < b', [
		'11: only `==` and `!=` are defined on arrays',
	]},
	MessageCase{'a []int, b [3]int', 'a <= b', [
		'11: only `==` and `!=` are defined on arrays',
		'9: infix expr: cannot use `[3]int` (right expression) as `[]int`',
	]},
	// V1 let the next three through, and the C compiler rejected them.
	MessageCase{'a map[string]int, b map[string]int', 'a < b', [
		'11: only `==` and `!=` are defined on maps',
	]},
	MessageCase{'a Speaker, b Speaker', 'a > b', [
		'9: undefined operation `Speaker` > `Speaker`',
	]},
	MessageCase{'a ?int', 'a < none', [
		'9: invalid operator `<` to `?int` and `none`',
	]},
	// Only a pointer compares with zero; `unsafe` does not change a struct value.
	MessageCase{'a Foo', 'a == 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `Foo`',
	]},
	MessageCase{'a Foo', 'unsafe { a == 0 }', [
		'18: infix expr: cannot use `int literal` (right expression) as `Foo`',
	]},
	MessageCase{'a Foo', 'unsafe { 0 != a }', [
		'18: infix expr: cannot use `Foo` (right expression) as `int literal`',
	]},
	MessageCase{'t thread int', 'unsafe { t == 0 }', [
		'18: infix expr: cannot use `int literal` (right expression) as `thread int`',
	]},
	MessageCase{'mut node Foo', 'node == 0', [
		'9: infix expr: cannot use `int literal` (right expression) as `Foo`  (you can use it inside an `unsafe` block)',
	]},
	MessageCase{'mut node Foo', 'unsafe { node == 0 }', []string{}},
	MessageCase{'mut node Foo', 'unsafe { 0 != node }', []string{}},
]

fn test_ordered_comparison_messages_match_v1() {
	mut lines := prelude.split_into_lines()
	mut expr_lines := []int{}
	for i, c in message_cases {
		lines << 'fn message_case_${i}(${c.params}) bool {'
		lines << '\treturn ${c.expr}'
		expr_lines << lines.len
		lines << '}'
		lines << ''
	}
	lines << 'fn main() {}'
	errors := check_errors('messages', lines.join('\n') + '\n')
	mut wrong := []string{}
	for i, c in message_cases {
		mut got := []string{}
		for err in errors {
			if err.line == expr_lines[i] {
				got << '${err.col}: ${err.msg}'
			}
		}
		mut want := c.expected.clone()
		got.sort()
		want.sort()
		if got != want {
			wrong << '`${c.expr}` with (${c.params}):\n  want ${want}\n  got  ${got}'
		}
	}
	for err in errors {
		assert err.line in expr_lines, 'error outside the checked expressions: ${err}'
	}
	assert wrong.len == 0, wrong.join('\n')
}

fn test_ordered_comparisons_are_checked_in_every_context() {
	// Each line marked `// bad` compares a thread handle with a number.
	src := prelude + '
fn check_threads(ts []thread int, t thread int) {
	_ := ts.filter(fn (x thread int) bool {
		return x > 0 // bad
	})
	_ := ts.filter(it < 0) // bad
	_ := ts.any(it <= 0) // bad
	if t >= 0 { // bad
	}
	for t < 0 { // bad
	}
	assert t > 0 // bad
	ok := t < 1 // bad
	_ = ok
	f := fn [t] () bool {
		return t > 0 // bad
	}
	_ = f
	_ := match true {
		t > 1 { 1 } // bad
		!(t > 1) { 2 } // bad
		(t < 0) { 3 } // bad
		is_true(t >= 0) { 4 } // bad
		else { 0 }
	}
}

fn is_true(b bool) bool {
	return b
}

fn (f Foo) above(limit int) bool {
	return f > limit // bad
}

fn main() {}
'
	mut bad_lines := []int{}
	for i, line in src.split_into_lines() {
		if line.ends_with('// bad') {
			bad_lines << i + 1
		}
	}
	errors := check_errors('contexts', src)
	mut lines_with_errors := map[int]bool{}
	for err in errors {
		assert err.line in bad_lines, 'unexpected error: ${err}'
		lines_with_errors[err.line] = true
	}
	for line in bad_lines {
		assert line in lines_with_errors, 'no error on line ${line}:\n${src.split_into_lines()[line - 1]}'
	}
}

fn test_ordered_comparisons_between_ordered_operands_compile_and_run() {
	src := prelude + "
struct Point {
	x int
	y int
}

fn (p Point) far(limit int) bool {
	return p.x > limit || p.y >= limit
}

fn lt[T](a T, b T) bool {
	return a < b
}

fn positive[T](a T) bool {
	\$if T is \$int {
		return a > 0
	} \$else \$if T is \$float {
		return a > 0.0
	} \$else {
		return false
	}
}

fn grow_below(mut o Op, limit Op) bool {
	o = Op{o.x + 1}
	return o < limit
}

fn is_null(mut node Foo) bool {
	return unsafe { node == 0 }
}

fn (mut f Foo) is_null_receiver() bool {
	return unsafe { 0 == f }
}

fn main() {
	println(lt(1, 2))
	println(lt(2.5, 1.5))
	println(lt('a', 'b'))
	println(lt(Op{1}, Op{2}))
	println(lt(true, false))
	println(positive(3))
	println(positive(-1.5))
	println(positive('x'))
	mut nums := [3, 1, 2]
	nums.sort(a < b)
	println(nums)
	nums.sort(a > b)
	println(nums)
	mut points := [Point{2, 1}, Point{1, 2}]
	points.sort(a.x < b.x)
	println(points.map(it.x))
	println(nums.filter(it > 1))
	println(nums.any(it >= 3))
	println(nums.all(it <= 3))
	s := Sum(5)
	if s is int {
		println(s > 4)
	}
	o := ?int(7)
	if v := o {
		println(v >= 7)
	}
	println((o or { 0 }) < 8)
	mut op := Op{1}
	println(grow_below(mut op, Op{3}))
	mut foo := Foo{}
	println(is_null(mut foo))
	println(foo.is_null_receiver())
	x := 3
	p := &x
	println(p > 0)
	c1 := chan int{}
	c2 := c1
	println(c1 <= c2)
	println(Point{3, 4}.far(3))
	println(MyInt(3) > 2)
	println(`a` < `b`)
	println(i64(-1) < 0)
	println(u8(200) >= 100)
	println(1.5 > 1)
	println(f32(1.5) < 2.5)
	println('abc' < 'abd')
	println(MyStr('a') < 'b')
	println(epoch() < time.unix(2))
	println(Op{3} >= Op{2})
	println(OpAlias(Op{1}) <= Op{1})
	m := {'a': 1}
	println(m['a'] > 0)
	println(nums.len > 2)
	\$for f in Point.fields {
		\$if f.typ is int {
			println(Point{5, 6}.\$(f.name) > 5)
		}
	}
	a, b := 1, 2
	println(a < b && b < 3)
	println(match a {
		1 { 10 }
		else { 20 }
	} > 5)
	unsafe {
		q := &x
		println(p <= q)
	}
	n := 5
	println(match true {
		!(n > 10) { 'small' }
		else { 'big' }
	})
	println(match true {
		(n >= 5) { 'five or more' }
		else { 'less' }
	})
}
"
	assert run_good('valid', src).split_into_lines() == [
		'true',
		'false',
		'true',
		'true',
		'false',
		'true',
		'false',
		'false',
		'[1, 2, 3]',
		'[3, 2, 1]',
		'[1, 2]',
		'[3, 2]',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'false',
		'false',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'true',
		'false',
		'true',
		'true',
		'true',
		'true',
		'small',
		'five or more',
	]
}

fn test_sort_comparators_report_each_mismatch_once() {
	src := prelude + "
struct Named {
	x    int
	name string
}

fn sort_threads(mut lines []string) {
	lines.sort((go a.split('/').last()) < b.split('/').last())
}

fn sort_named(mut items []Named) {
	items.sort(a.x < b.name)
}

fn main() {}
"
	lines := src.split_into_lines()
	threads_line := lines.index("\tlines.sort((go a.split('/').last()) < b.split('/').last())") + 1
	named_line := lines.index('\titems.sort(a.x < b.name)') + 1
	errors := check_errors('sort', src)
	// `.sort()` reports a thread handle on the left of its comparison itself.
	thread_mismatches := errors.filter(it.line == threads_line && it.msg.starts_with('infix expr'))
	assert thread_mismatches.len == 1, errors.str()
	assert thread_mismatches[0].msg == 'infix expr: cannot use `string` (right expression) as `thread string`'
	assert errors.any(it.line == named_line
		&& it.msg == 'infix expr: cannot use `string` (right expression) as `int`'), errors.str()
}

fn test_ordered_comparisons_in_templates() {
	// The code of a template is checked as part of the function that renders
	// it, with positions that belong to the template rather than to the V file.
	dir := unique_temp_path('templates')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'good.html'), "@if n > 0\npositive\n@end\n@if name < 'm'\nearly\n@end\n") or {
		panic(err)
	}
	good_src := os.join_path(dir, 'good.v')
	os.write_file(good_src, "module main\n\nfn page(n int, name string) string {\n\treturn \$tmpl('good.html')\n}\n\nfn main() {\n\tprint(page(1, 'a'))\n}\n") or {
		panic(err)
	}
	good_bin := os.join_path(dir, 'good')
	compile :=
		os.execute('${os.quoted_path(build_v3())} -nocache ${os.quoted_path(good_src)} -b c -o ${os.quoted_path(good_bin)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(good_bin))
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines().map(it.trim_space()).filter(it != '') == [
		'positive',
		'early',
	]

	os.write_file(os.join_path(dir, 'bad.html'), '@if name > 1\nlate\n@end\n') or { panic(err) }
	bad_src := os.join_path(dir, 'bad.v')
	os.write_file(bad_src, "module main\n\nfn page(name string) string {\n\treturn \$tmpl('bad.html')\n}\n\nfn main() {\n\tprint(page('a'))\n}\n") or {
		panic(err)
	}
	errors := check_file_errors(bad_src)
	assert errors.len == 1, errors.str()
	assert errors[0].file.ends_with('bad.html') && errors[0].line == 1 && errors[0].col == 5, errors.str()
	assert errors[0].msg.starts_with('infix expr: cannot use `int literal` (right expression) as `string`'), errors.str()
}

fn test_translated_files_keep_c_comparisons() {
	errors := check_errors('translated', '@[translated]
module main

enum Level {
	low
	high
}

fn above(l Level, n int) bool {
	return l > n
}

fn main() {}
')
	assert errors.len == 0, errors.str()
}
