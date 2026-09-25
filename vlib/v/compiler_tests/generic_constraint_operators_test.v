module main

import os

// An operator on a value of a constrained type parameter takes what every type
// of its constraint takes. This test holds the rules to the checker itself: for
// each type below, a `T` whose constraint is that type alone must be reported
// on exactly the lines where a value of that type is, for every operator, every
// kind of operand on either side, the assignment operators, `++`, `--` and
// indexing. The types cover every group of the rules and every way a type can
// declare its operators (docs.vlang.io/limited-operator-overloading.html): one
// of `+ - * / % ** < ==` alone, `<` with `==`, all of them, `[]` and `[]=`, an
// alias of a primitive that declares one, and an alias of a struct that does.

const operator_prelude = 'module main

interface Named {
	name string
}

struct User {
	name string
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

type Sum = int | string

type IntArr = []int

type IntArr2 = [2]int

type StrMap = map[string]int

type Fn0 = fn ()

type MyInt = int

type MyStr = string

type MyFloat = f64

struct Vec {
	x int
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x}
}

fn (a Vec) < (b Vec) bool {
	return a.x < b.x
}

struct Eq {
	x int
}

fn (a Eq) == (b Eq) bool {
	return a.x == b.x
}

struct OpAdd {
	x int
}

fn (a OpAdd) + (b OpAdd) OpAdd {
	return OpAdd{a.x + b.x}
}

struct OpSub {
	x int
}

fn (a OpSub) - (b OpSub) OpSub {
	return OpSub{a.x - b.x}
}

struct OpMul {
	x int
}

fn (a OpMul) * (b OpMul) OpMul {
	return OpMul{a.x * b.x}
}

struct OpDiv {
	x int
}

fn (a OpDiv) / (b OpDiv) OpDiv {
	return OpDiv{a.x / b.x}
}

struct OpMod {
	x int
}

fn (a OpMod) % (b OpMod) OpMod {
	return OpMod{a.x % b.x}
}

struct OpPow {
	x int
}

fn (a OpPow) ** (b OpPow) OpPow {
	return OpPow{a.x * b.x}
}

struct OpLt {
	x int
}

fn (a OpLt) < (b OpLt) bool {
	return a.x < b.x
}

struct OpLtEq {
	x int
}

fn (a OpLtEq) < (b OpLtEq) bool {
	return a.x < b.x
}

fn (a OpLtEq) == (b OpLtEq) bool {
	return a.x == b.x
}

struct OpAll {
	x int
}

fn (a OpAll) + (b OpAll) OpAll {
	return OpAll{a.x + b.x}
}

fn (a OpAll) - (b OpAll) OpAll {
	return OpAll{a.x - b.x}
}

fn (a OpAll) * (b OpAll) OpAll {
	return OpAll{a.x * b.x}
}

fn (a OpAll) / (b OpAll) OpAll {
	return OpAll{a.x / b.x}
}

fn (a OpAll) % (b OpAll) OpAll {
	return OpAll{a.x % b.x}
}

fn (a OpAll) ** (b OpAll) OpAll {
	return OpAll{a.x * b.x}
}

fn (a OpAll) < (b OpAll) bool {
	return a.x < b.x
}

fn (a OpAll) == (b OpAll) bool {
	return a.x == b.x
}

struct OpIdx {
	data []int
}

fn (o OpIdx) [] (i int) int {
	return o.data[i]
}

struct OpIdxSet {
mut:
	data []int
}

fn (o OpIdxSet) [] (i int) int {
	return o.data[i]
}

fn (mut o OpIdxSet) []= (i int, v int) {
	o.data[i] = v
}

type Meters = int

fn (a Meters) + (b Meters) Meters {
	return Meters(int(a) + int(b))
}

type AliasAdd = OpAdd
'

// The types, each as a constraint of its own; `[T Named]` is the interface
// constraint, the rest are sets of one type.
const operator_types = ['i8', 'i16', 'int', 'i64', 'u8', 'u16', 'u32', 'u64', 'isize', 'usize',
	'rune', 'f32', 'f64', 'string', 'bool', 'Color', 'Perm', 'User', 'Sum', 'IntArr', 'IntArr2',
	'StrMap', 'Fn0', 'MyInt', 'MyStr', 'MyFloat', 'Vec', 'Eq', 'OpAdd', 'OpSub', 'OpMul', 'OpDiv',
	'OpMod', 'OpPow', 'OpLt', 'OpLtEq', 'OpAll', 'OpIdx', 'OpIdxSet', 'Meters', 'AliasAdd', 'Named',
	'voidptr']

// Pointers are left to the checker: an operator on them is not checked.
const unchecked_types = ['voidptr']

const binary_ops = ['+', '-', '*', '/', '%', '**', '<', '>', '<=', '>=', '==', '!=', '&', '|',
	'^', '<<', '>>', '>>>', '&&', '||']

const order_ops = ['<', '>', '<=', '>=']

const assign_ops = ['+=', '-=', '*=', '/=', '%=', '**=', '&=', '|=', '^=', '<<=', '>>=', '>>>=']

// The other operand of each operation: another value of the type, a literal,
// or a value of a builtin type, named after the parameters of the functions.
const right_operands = ['b', '1', '1.5', "'x'", 'true', '`a`', 'ki', 'kf', 'ks', 'kb', 'kr', 'kl',
	'ku', 'kg']

const left_operands = ['1', '1.5', "'x'", 'true', '`a`', 'ki', 'kf', 'ks', 'kb', 'kr', 'kl', 'ku',
	'kg']

const value_params = 'ki int, kf f64, ks string, kb bool, kr rune, kl i64, ku u8, kg f32'

struct OperatorCase {
	name  string // what the line does, for a failure
	line  string
	order bool // an order between operands: `<`, `>`, `<=`, `>=`
}

fn operator_cases() ([]OperatorCase, []OperatorCase, []OperatorCase) {
	mut binary := []OperatorCase{}
	for rhs in right_operands {
		for op in binary_ops {
			binary << OperatorCase{'a ${op} ${rhs}', '\t_ = a ${op} ${rhs}', op in order_ops}
		}
	}
	for lhs in left_operands {
		for op in binary_ops {
			binary << OperatorCase{'${lhs} ${op} a', '\t_ = ${lhs} ${op} a', op in order_ops}
		}
	}
	mut unary := []OperatorCase{}
	for op in ['-', '!', '~'] {
		unary << OperatorCase{'${op}a', '\t_ = ${op}a', false}
	}
	for index in ['0', "'k'"] {
		unary << OperatorCase{'a[${index}]', '\t_ = a[${index}]', false}
	}
	mut assign := []OperatorCase{}
	for rhs in right_operands {
		for op in assign_ops {
			assign << OperatorCase{'c ${op} ${rhs}', '\tc ${op} ${rhs}', false}
		}
	}
	assign << OperatorCase{'c++', '\tc++', false}
	assign << OperatorCase{'c--', '\tc--', false}
	return binary, unary, assign
}

// operator_program writes the operations on a value of `typ`, or with
// `constrained` on a `T` whose constraint is `typ`, and where each case is.
fn operator_program(typ string, constrained bool) (string, map[int]OperatorCase) {
	binary, unary, assign := operator_cases()
	mut lines := operator_prelude.split('\n')
	mut where := map[int]OperatorCase{}
	param := if constrained { 'T' } else { typ }
	generic := if !constrained {
		''
	} else if typ == 'Named' {
		'[T Named]'
	} else {
		'[T Only]'
	}
	if constrained && typ != 'Named' {
		lines << 'constraint Only = ${typ}'
	}
	for group, cases in [binary, unary, assign] {
		name := ['binary', 'unary', 'assign'][group]
		lines << 'fn ${name}${generic}(a ${param}, b ${param}, ${value_params}) {'
		if group == 2 {
			lines << '\tmut c := a'
		}
		for c in cases {
			where[lines.len + 1] = c
			lines << c.line
		}
		if group == 2 {
			lines << '\t_ = c'
		}
		lines << '}'
	}
	lines << 'fn main() {}'
	return lines.join('\n') + '\n', where
}

// rejected_lines are the lines of `path` that the check reports an error on.
fn rejected_lines(path string) map[int]bool {
	res := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check -nocolor -checker-fixture ${os.quoted_path(path)}')
	assert !res.output.contains(' more errors'), res.output
	mut lines := map[int]bool{}
	for line in res.output.split_into_lines() {
		if !line.contains(': error: ') {
			continue
		}
		location := line.all_before(': error: ').split(':')
		if location.len >= 3 {
			lines[location[location.len - 2].int()] = true
		}
	}
	return lines
}

fn check_operator_program(dir string, name string, source string) map[int]bool {
	path := os.join_path(dir, name, 'main.v')
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
	return rejected_lines(path)
}

fn test_an_operator_on_a_constrained_value_follows_the_checker_for_every_type() {
	dir := os.join_path(os.vtmp_dir(), 'v3_constraint_operators_${os.getpid()}')
	defer {
		os.rmdir_all(dir) or {}
	}
	// Without #28854 the checker takes an order between operands that have none,
	// as `true < false`, and a constrained `T` is stricter there.
	probe := check_operator_program(dir, 'probe', 'module main\n\nfn main() {\n\tprintln(true < false)\n}\n')
	checks_order := probe.len > 0
	mut failures := []string{}
	mut compared := 0
	for typ in operator_types {
		concrete_source, where := operator_program(typ, false)
		constrained_source, constrained_where := operator_program(typ, true)
		concrete := check_operator_program(dir, 'concrete_${compared}', concrete_source)
		constrained := check_operator_program(dir, 'constrained_${compared}', constrained_source)
		// The constrained program has one line more, its `constraint`, before the cases.
		offset := if typ == 'Named' { 0 } else { 1 }
		for line, c in where {
			assert constrained_where[line + offset].name == c.name
			by_type := line in concrete
			by_constraint := (line + offset) in constrained
			compared++
			if typ in unchecked_types {
				if by_constraint {
					failures << '${typ}: `${c.name}` is reported on a `T`, but pointers are not checked'
				}
				continue
			}
			if c.order && !checks_order {
				if by_type && !by_constraint {
					failures << '${typ}: `${c.name}` is rejected on a value, not on a `T`'
				}
				continue
			}
			if by_type != by_constraint {
				failures << '${typ}: `${c.name}` is ${if by_type { 'rejected' } else { 'taken' }} on a value, but ${if by_constraint {
					'rejected'
				} else {
					'taken'
				}} on a `T`'
			}
		}
	}
	assert compared > 30000, compared.str()
	assert failures.len == 0, '${failures.len} of ${compared} operations differ:\n' +
		failures#[..60].join('\n')
}
