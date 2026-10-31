import os
import v.cmdexec

// A local or a parameter that has the name of a const of its module is the
// local for `&` too, also in a `$for` body: its address is not a reference to
// the const.
const shadowed_const_program = "const routes = 5
const count = 10

struct Counter {
mut:
	n int
}

const counter = Counter{123}

struct Fields {
	a int
	b string
}

fn address_of_parameter(routes []string) int {
	p := &routes
	return p.len
}

fn generic_field_routes[T]() []string {
	mut out := []string{}
	\$for field in T.fields {
		routes := ['a', 'b', 'c']
		p := &routes
		out << '\${field.name}:\${p.len}'
	}
	return out
}

fn field_routes() []string {
	mut out := []string{}
	\$for field in Fields.fields {
		\$if field.typ is string {
			routes := {
				'x': 1
			}
			p := &routes
			out << '\${field.name}:\${p.len}'
		} \$else {
			routes := [1, 2]
			p := &routes
			out << '\${field.name}:\${p.len}'
		}
	}
	return out
}

fn consts() string {
	return '\${routes} \${count} \${counter.n}'
}

fn main() {
	routes := [1, 2, 3]
	p := &routes
	println(p.len)
	count := 7
	pc := &count
	println(*pc)
	mut counter := Counter{1}
	mut counters := []&Counter{}
	counters << &counter
	counters[0].n = 2
	println(counter.n)
	println(address_of_parameter(['x']))
	println(generic_field_routes[Fields]())
	println(field_routes())
	println(consts())
}
"

fn test_the_address_of_a_local_that_shadows_a_const() {
	root := os.join_path(os.vtmp_dir(), 'const_shadowed_by_local_address_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	program := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	os.write_file(source, shadowed_const_program)!
	// An ordinary program: the shadowing locals give warnings, and a test file
	// lowers its consts in its own way.
	build := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-no-retry-compilation', '-nocache',
		'-gc', 'none', '-o', program, source], 120_000)
	assert build.exit_code == 0, build.output
	run := cmdexec.run_with_timeout(program, []string{}, 10_000)
	assert run.exit_code == 0, run.output
	assert run.output.replace('\r\n', '\n').split_into_lines() == ['3', '7', '2', '1', "['a:3', 'b:3']",
		"['a:2', 'b:1']", '5 10 123'], run.output
}
