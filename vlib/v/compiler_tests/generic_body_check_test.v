// vtest build: !windows
module main

import os

// A check tells what the body of a generic function does wrong without its
// type parameters, as it does for any other function. What depends on them is
// told by the checks against their constraints, once.

const work_dir = os.join_path(os.vtmp_dir(), 'generic_body_check_${os.getpid()}')

fn testsuite_begin() {
	os.mkdir_all(work_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(work_dir) or {}
}

// check checks `source` as the main.v of a directory of its own, as an editor
// does, and returns the lines of its errors and warnings.
fn check(name string, source string) []string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	res := os.execute('cd ${os.quoted_path(dir)} && ${os.quoted_path(@VEXE)} -new-compiler -check -nocolor .')
	return res.output.split_into_lines().filter(it.starts_with('main.v:')
		&& (it.contains(': error: ') || it.contains(': warning: ')))
}

fn test_a_generic_body_reports_what_does_not_depend_on_its_type_parameters() {
	errors := check('independent', "module main

type Numeric = int | f64

interface Named {
	name string
}

struct User {
	name string
}

fn takes_int(n int) int {
	return n
}

fn test[T Numeric](value T) string {
	\$if T is f64 {
		return 'float'
	} \$else {
		return 'number'
	}
	asd
}

fn named[T Named](x T) string {
	sum := 1 + 'a'
	println(sum)
	return x.name
}

fn plain[T](x T) T {
	n := takes_int('s')
	println(n)
	return x
}

fn wrong_return[T Named](x T) string {
	println(x.name)
	return 5
}

fn main() {
	println(test(1))
	println(named(User{'a'}))
	println(plain(1))
	println(wrong_return(User{'b'}))
}
")
	assert errors.len == 4, errors.str()
	assert errors[0].starts_with('main.v:23:2: error: unexpected name `asd`'), errors[0]
	assert errors[1].starts_with('main.v:27:9: error: operator `+` cannot concatenate `int` and `string`'), errors[1]
	assert errors[2].starts_with('main.v:33:17: error: cannot use `string` as `int` in argument 1 to `takes_int`'), errors[2]
	assert errors[3].starts_with('main.v:40:9: error: cannot use `int literal` as type `string` in return argument'), errors[3]
}

fn test_a_generic_body_reports_nothing_the_type_parameters_decide() {
	// Each statement below depends on `T`: the checks against the constraints
	// say what is wrong there, and nothing is wrong here.
	errors := check('dependent', "module main

type Numeric = int | f64

interface Named {
	name string
}

struct User {
	name string
}

fn total_length[T Named](xs []T) int {
	lengths := xs.map(it.name.len)
	mut sum := 0
	for n in lengths {
		sum += n
	}
	return sum
}

fn describe[T Numeric](x T) string {
	\$if T is int {
		return x.hex()
	}
	return x.str()
}

fn first[T Named](xs []T) string {
	for x in xs {
		return x.name
	}
	return ''
}

fn main() {
	println(total_length([User{'a'}]))
	println(describe(10))
	println(first([User{'b'}]))
}
")
	assert errors == []string{}
}

fn test_an_error_of_a_type_parameter_is_told_once_by_its_constraint() {
	errors := check('constraint', "module main

interface Named {
	name string
}

struct User {
	name string
}

fn named[T Named](x T) string {
	return x.nombre
}

fn main() {
	println(named(User{'a'}))
}
")
	assert errors.len == 1, errors.str()
	assert errors[0].contains('type `T` has no field named `nombre`: its constraint `Named` does not declare it'), errors[0]
}
