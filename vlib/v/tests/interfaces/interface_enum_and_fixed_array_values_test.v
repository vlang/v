// Enum values (plain, backed and flag enums) and fixed arrays boxed into interfaces carry
// their own type tag, so method dispatch, `is`/`as`, `type_name()` and printing work as
// for other value types.
import os

interface Namer {
	name() string
}

interface Empty {}

interface Stepper {
mut:
	step() int
}

enum Color {
	red
	green
	blue
}

fn (c Color) name() string {
	return 'color:${c}'
}

enum Small as u8 {
	a = 3
	b = 200
}

fn (s Small) name() string {
	return 'small:${s}'
}

@[flag]
enum Perm {
	read
	write
	exec
}

fn (p Perm) name() string {
	return 'perm'
}

enum Custom {
	x
	y
}

fn (c Custom) name() string {
	return 'custom'
}

fn (c Custom) str() string {
	return 'custom-${int(c)}'
}

enum Counter {
	zero
	one
	two
}

fn (mut c Counter) step() int {
	c = match c {
		.zero { .one }
		else { .two }
	}
	return if c == .one { 1 } else { 2 }
}

struct Dog {
	n string
}

fn (d Dog) name() string {
	return 'dog'
}

fn box_namer[T](x T) Namer {
	return Namer(x)
}

fn describe(n Namer) string {
	return match n {
		Color { 'Color ${n}' }
		Small { 'Small ${n}' }
		Perm { 'Perm ${n}' }
		Dog { 'Dog' }
		else { 'other ${n.type_name()}' }
	}
}

fn test_enum_method_dispatch() {
	assert Namer(Color.green).name() == 'color:green'
	assert Namer(Small.b).name() == 'small:b'
	assert Namer(Perm.read | .exec).name() == 'perm'
	assert Namer(Custom.y).name() == 'custom'
	assert box_namer(Color.blue).name() == 'color:blue'
	mut names := []string{}
	for n in [Namer(Color.red), Small.a, Dog{}] {
		names << n.name()
	}
	assert names == ['color:red', 'small:a', 'dog']
}

fn test_enum_mut_method_dispatch() {
	mut s := Stepper(Counter.zero)
	assert s.step() == 1
	assert s.step() == 2
	assert '${s}' == 'Stepper(two)'
}

fn test_enum_printing_and_type_name() {
	assert '${Namer(Color.green)}' == 'Namer(green)'
	assert '${Namer(Small.b)}' == 'Namer(b)'
	assert '${Namer(Perm.read | .exec)}' == 'Namer(Perm{.read | .exec})'
	assert '${Namer(Custom.y)}' == 'Namer(custom-1)'
	assert '${[Namer(Color.red), Small.a]}' == '[Namer(red), Namer(a)]'
	assert Namer(Color.green).type_name() == 'Color'
	assert Namer(Small.b).type_name() == 'Small'
	assert Namer(Perm.write).type_name() == 'Perm'
	assert '${Empty(Color.green)}' == 'Empty(green)'
	assert '${Empty(Small.a)}' == 'Empty(a)'
	assert '${Empty(Perm.write)}' == 'Empty(Perm{.write})'
	assert Empty(Color.green).type_name() == 'Color'
	assert Empty(Perm.write).type_name() == 'Perm'
}

fn test_enum_is_and_as() {
	assert describe(Color.green) == 'Color green'
	assert describe(Small.b) == 'Small b'
	assert describe(Perm.exec) == 'Perm Perm{.exec}'
	assert describe(Custom.x) == 'other Custom'
	assert describe(Dog{}) == 'Dog'
	n := Namer(Small.b)
	assert n is Small
	assert n !is Color
	assert n as Small == .b
	e := Empty(Color.blue)
	assert e is Color
	assert e !is Small
	assert e as Color == .blue
}

fn test_fixed_arrays_in_empty_interface() {
	a := Empty([1, 2, 3]!)
	assert '${a}' == 'Empty([1, 2, 3])'
	assert a.type_name() == '[3]int'
	assert a is [3]int
	assert a !is []int
	f := a as [3]int
	assert f[2] == 3
	s := Empty(['x', 'y']!)
	assert '${s}' == "Empty(['x', 'y'])"
	assert s.type_name() == '[2]string'
	if s is [2]string {
		assert s[1] == 'y'
	} else {
		assert false
	}
	nested := Empty([[1, 2]!, [3, 4]!]!)
	assert '${nested}' == 'Empty([[1, 2], [3, 4]])'
	assert nested.type_name() == '[2][2]int'
	dogs := Empty([Dog{'a'}]!)
	assert dogs.type_name() == '[1]Dog'
	assert (dogs as [1]Dog)[0].n == 'a'
}

fn run_child(executable string, args []string) (int, string) {
	mut child := os.new_process(executable)
	child.set_args(args)
	child.set_redirect_stdio_merged()
	child.run()
	output := child.stdout_slurp()
	child.wait()
	code := child.code
	child.close()
	return code, output
}

// test_failed_as_panics runs the failing casts in a separate program, since they panic.
fn test_failed_as_panics() {
	dir := os.join_path(os.vtmp_dir(), 'interface_enum_fixed_array_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	program := os.join_path(dir, 'interface_enum_fixed_array')
	os.write_file(source, '
import os

interface Namer {
	name() string
}

interface Empty {}

enum Color {
	red
}

fn (c Color) name() string {
	return "color"
}

struct Dog {}

fn (d Dog) name() string {
	return "dog"
}

fn main() {
	match os.args[1] {
		"enum" {
			d := Namer(Color.red) as Dog
			println(d)
		}
		"fixed" {
			c := Empty([1, 2]!) as Color
			println(c)
		}
		else {}
	}
}
')!
	code, output := run_child(@VEXE, ['-o', program, source])
	assert code == 0, output
	for mode, message in {
		'enum':  'as cast: cannot cast `main.Color` to `main.Dog`'
		'fixed': 'as cast: cannot cast `[2]int` to `main.Color`'
	} {
		exit_code, panic_output := run_child(program, [mode])
		assert exit_code != 0, '${mode}: ${panic_output}'
		assert panic_output.contains(message), '${mode}: ${panic_output}'
	}
}
