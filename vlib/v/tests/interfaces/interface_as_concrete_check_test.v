// `iface as Concrete` and `iface as &Concrete` check the dynamic type of the value at
// run time, and panic like a failed sum type `as` cast when it is another type.
import os

interface Animal {
	name() string
}

struct Dog {
	n string
}

fn (d Dog) name() string {
	return d.n
}

struct Cat {
	lives int
	n     string
}

fn (c Cat) name() string {
	return c.n
}

struct RefDog {
mut:
	n string
}

fn (d &RefDog) name() string {
	return d.n
}

struct MyError {
	Error
	code int
}

fn fail_with_my_error() ! {
	return MyError{
		code: 7
	}
}

fn test_as_value_of_the_stored_type() {
	a := Animal(Dog{'rex'})
	d := a as Dog
	assert d.n == 'rex'
	c := Animal(Cat{9, 'tom'}) as Cat
	assert c.lives == 9
	assert c.n == 'tom'
}

fn test_as_pointer_reads_the_stored_object() {
	mut a := Animal(&RefDog{'rex'})
	mut d := a as &RefDog
	assert d.n == 'rex'
	d.n = 'max'
	assert a.name() == 'max'
	v := Animal(Dog{'fido'})
	p := v as &Dog
	assert p.n == 'fido'
}

fn test_as_inside_smartcast() {
	a := Animal(Dog{'rex'})
	mut seen := false
	if a is Dog {
		d := a as Dog
		assert d.n == 'rex'
		p := a as &Dog
		assert p.n == 'rex'
		seen = true
	}
	assert seen
}

fn test_as_inside_match_branch() {
	a := Animal(Dog{'rex'})
	names := match a {
		Cat { (a as Cat).n }
		Dog { (a as Dog).n }
		else { '' }
	}
	assert names == 'rex'
}

fn test_as_in_short_circuit_is_not_evaluated() {
	a := Animal(Cat{1, 'tom'})
	is_dog := a is Dog
	ok := is_dog && (a as Dog).n == 'rex'
	assert !ok
}

fn test_ierror_as_concrete() {
	fail_with_my_error() or {
		e := err as MyError
		assert e.code == 7
		p := err as &MyError
		assert p.code == 7
		return
	}
	assert false
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
	dir := os.join_path(os.vtmp_dir(), 'interface_as_concrete_check_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	program := os.join_path(dir, 'interface_as_concrete_check')
	os.write_file(source, '
import os

interface Animal {
	name() string
}

struct Dog {
	n string
}

fn (d Dog) name() string {
	return d.n
}

struct Cat {
	lives int
}

fn (c Cat) name() string {
	return "cat"
}

struct MyError {
	Error
}

struct OtherError {
	Error
}

fn fail() ! {
	return OtherError{}
}

fn main() {
	a := Animal(Dog{"rex"})
	match os.args[1] {
		"value" {
			c := a as Cat
			println(c.lives)
		}
		"pointer" {
			c := a as &Cat
			println(c.lives)
		}
		"error" {
			fail() or {
				e := err as MyError
				println(e.msg())
			}
		}
		else {
			println((a as Dog).n)
		}
	}
}
')!
	code, output := run_child(@VEXE, ['-o', program, source])
	assert code == 0, output
	ok_code, ok_output := run_child(program, ['ok'])
	assert ok_code == 0, ok_output
	assert ok_output.trim_space() == 'rex'
	for mode, message in {
		'value':   'as cast: cannot cast `main.Dog` to `main.Cat`'
		'pointer': 'as cast: cannot cast `main.Dog` to `main.Cat`'
		'error':   'as cast: cannot cast `main.OtherError` to `main.MyError`'
	} {
		exit_code, panic_output := run_child(program, [mode])
		assert exit_code != 0, '${mode}: ${panic_output}'
		assert panic_output.contains(message), '${mode}: ${panic_output}'
	}
}
