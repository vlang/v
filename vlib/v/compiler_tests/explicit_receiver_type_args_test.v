module main

import os

// A method of a generic struct can repeat the type parameter of its receiver in
// its own list, `fn (b Box[T]) own[T]() T`: that `T` is the receiver's, fixed by
// the `Box` the method is called on. A call that spells it (`b.own[int]()`) has
// to spell that type, and one that spells another (`b.own[string]()` for a
// `Box[int]`) is an error of V, where before V3 went on with `string` and the C
// compiler failed, or took the embedded receiver for a `Box[string]`.

const receiver_prelude = "module main

struct Box[T] {
	item T
}

fn (b Box[T]) own[T]() T {
	return b.item
}

fn (b &Box[T]) own_ref[T]() T {
	return b.item
}

fn (b Box[T]) pair[T, U](u U) string {
	return '\${b.item} \${u}'
}

fn (b Box[T]) map[U](f fn (T) U) Box[U] {
	return Box[U]{
		item: f(b.item)
	}
}

struct Holder {
	Box[int]
}

type IntBox = Box[int]

fn inside[T](b Box[T]) T {
	return b.own[T]()
}
"

// receiver_source is a program of receiver_prelude with the declarations
// `decls` and a `main` of `body`.
fn receiver_source(decls string, body string) string {
	return receiver_prelude + decls + '\nfn main() {\n' + body + '}\n'
}

// line_of is the line of `source` that reads `text`.
fn line_of(source string, text string) int {
	line := source.split('\n').index(text) + 1
	assert line > 0, '`${text}` is not a line of the program'
	return line
}

fn write_receiver_source(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_explicit_receiver_type_args_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn check_receiver_source(name string, source string) os.Result {
	path := write_receiver_source(name, source)
	defer {
		os.rmdir_all(os.dir(path)) or {}
	}
	return os.exec([@VEXE, '-new-compiler', '-check', '-nocolor', path])
}

fn build_receiver_source(name string, source string) os.Result {
	path := write_receiver_source(name, source)
	defer {
		os.rmdir_all(os.dir(path)) or {}
	}
	return os.exec([@VEXE, '-new-compiler', '-nocolor', '-o', path.all_before_last('.'), path])
}

// error_lines returns `line:col: message` for every error of an output.
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
		lines << '${location[location.len - 2]}:${location[location.len - 1]}: ${line.all_after(': error: ')}'
	}
	return lines
}

const box_of_int = '\tb := Box[int]{\n\t\titem: 3\n\t}\n'

// other_own asks every instance for the `own` of a `Box[int]`: only the `int`
// one has it.
const other_own = '
fn other_own[T](b Box[T]) int {
	return b.own[int]()
}
'

fn test_an_explicit_type_that_the_receiver_does_not_fix_is_an_error() {
	source := receiver_source('', box_of_int + '\tprintln(b.own[string]())\n')
	res := check_receiver_source('direct', source)
	assert res.exit_code == 1, res.output
	line := line_of(source, '\tprintln(b.own[string]())')
	assert error_lines(res.output) == [
		'${line}:15: `own` takes `T` from its receiver `Box[int]`: `T` is `int`, not `string`',
	], res.output
}

fn test_an_explicit_type_that_an_embedded_receiver_does_not_fix_is_an_error() {
	source := receiver_source('', '\th := Holder{\n\t\tBox: Box[int]{\n\t\t\titem: 4\n\t\t}\n\t}\n\tprintln(h.own[string]())\n')
	res := check_receiver_source('embedded', source)
	assert res.exit_code == 1, res.output
	line := line_of(source, '\tprintln(h.own[string]())')
	// The one error, and not `cannot use receiver `Holder` as `Box[string]``.
	assert error_lines(res.output) == [
		'${line}:15: `own` takes `T` from its receiver `Box[int]` (embedded in `Holder`): `T` is `int`, not `string`',
	], res.output
}

fn test_an_explicit_type_that_the_receiver_does_not_fix_is_an_error_among_the_methods_own() {
	source := receiver_source('', box_of_int + "\tprintln(b.pair[string, string]('x'))\n")
	res := check_receiver_source('pair', source)
	assert res.exit_code == 1, res.output
	line := line_of(source, "\tprintln(b.pair[string, string]('x'))")
	assert error_lines(res.output) == [
		'${line}:16: `pair` takes `T` from its receiver `Box[int]`: `T` is `int`, not `string`',
	], res.output
}

fn test_an_explicit_type_that_a_reference_receiver_does_not_fix_is_an_error() {
	source := receiver_source('', '\tmut b := Box[int]{\n\t\titem: 3\n\t}\n\tprintln(b.own_ref[string]())\n')
	res := check_receiver_source('reference', source)
	assert res.exit_code == 1, res.output
	line := line_of(source, '\tprintln(b.own_ref[string]())')
	assert error_lines(res.output) == [
		'${line}:19: `own_ref` takes `T` from its receiver `Box[int]`: `T` is `int`, not `string`',
	], res.output
}

fn test_an_explicit_type_that_an_alias_receiver_does_not_fix_is_an_error() {
	source := receiver_source('', '\tb := IntBox{\n\t\titem: 3\n\t}\n\tprintln(b.own[string]())\n')
	res := check_receiver_source('alias', source)
	assert res.exit_code == 1, res.output
	line := line_of(source, '\tprintln(b.own[string]())')
	assert error_lines(res.output) == [
		'${line}:15: `own` takes `T` from its receiver `IntBox`: `T` is `int`, not `string`',
	], res.output
}

fn test_an_instance_whose_receiver_fixes_another_type_is_an_error() {
	source := receiver_source(other_own, "\tprintln(other_own(Box[string]{\n\t\titem: 'a'\n\t}))\n")
	res := check_receiver_source('instance', source)
	assert res.exit_code == 1, res.output
	line := line_of(source, '\treturn b.own[int]()')
	assert error_lines(res.output) == [
		'${line}:14: `own` takes `T` from its receiver `Box[string]`: `T` is `string`, not `int`',
	], res.output
}

fn test_a_build_reports_them_in_v_instead_of_the_c_compiler() {
	direct := build_receiver_source('build', receiver_source('', box_of_int +
		'\tprintln(b.own[string]())\n'))
	assert direct.exit_code == 1, direct.output
	assert !direct.output.contains('C compilation'), direct.output
	assert direct.output.contains('`own` takes `T` from its receiver `Box[int]`'), direct.output
	instance := build_receiver_source('build_instance', receiver_source(other_own,
		"\tprintln(other_own(Box[string]{\n\t\titem: 'a'\n\t}))\n"))
	assert instance.exit_code == 1, instance.output
	assert !instance.output.contains('C compilation'), instance.output
	assert instance.output.contains('`own` takes `T` from its receiver `Box[string]`'), instance.output
}

fn test_explicit_types_that_the_receiver_fixes_are_fine() {
	body := box_of_int + '\tmut r := Box[int]{\n\t\titem: 5\n\t}\n' +
		'\th := Holder{\n\t\tBox: Box[int]{\n\t\t\titem: 4\n\t\t}\n\t}\n' +
		'\ta := IntBox{\n\t\titem: 6\n\t}\n' +
		"\tprintln(b.own[int]())\n\tprintln(h.own[int]())\n\tprintln(b.pair[int, string]('x'))\n" +
		'\tprintln(r.own_ref[int]())\n\tprintln(a.own[int]())\n\tprintln(other_own(b))\n' +
		"\tprintln(b.own())\n\tprintln(b.pair('y'))\n\tprintln(inside(Box[string]{ item: 'a' }))\n" +
		'\tprintln(b.map[string](fn (x int) string {\n\t\treturn x.str()\n\t}).item)\n'
	res := check_receiver_source('fine', receiver_source(other_own, body))
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

// A generic method named with its type arguments is a value of the type of that
// method, as a generic function named so is: a variable it initializes has
// that type, and a misuse of it is an error.
fn test_a_method_value_has_the_type_of_the_method_it_names() {
	source := receiver_source('', box_of_int + '\tf := b.own[int]\n\tx := f + 1\n\tprintln(x)\n')
	res := check_receiver_source('value_type', source)
	assert res.exit_code == 1, res.output
	line := line_of(source, '\tx := f + 1')
	assert error_lines(res.output) == [
		'${line}:7: mismatched types `fn () int` and `int literal`',
		'${line}:7: infix expr: cannot use `int literal` (right expression) as `fn () int`',
	], res.output
}

// A method named with its type arguments and without a call is checked as its
// call is: `b.own[string]` for a `Box[int]` too.
fn test_a_method_value_that_writes_another_type_than_the_receiver_fixes_is_an_error() {
	source := receiver_source('', box_of_int + '\tf := b.own[string]\n\tprintln(f())\n')
	res := check_receiver_source('value', source)
	assert res.exit_code == 1, res.output
	line := line_of(source, '\tf := b.own[string]')
	assert error_lines(res.output) == [
		'${line}:12: `own` takes `T` from its receiver `Box[int]`: `T` is `int`, not `string`',
	], res.output
}
