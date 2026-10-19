import os

const shared_locals_vexe = @VEXE

const shared_locals_aliases_module = "module aliases

pub struct Holder {
pub:
	name string
}

pub type Names = []string

pub fn (values Names) clone() []Holder {
	return [Holder{name: values[0]}]
}

pub fn (mut values Names) mutate() []Holder {
	values[0] = 'mutated'
	return [Holder{name: values[0]}]
}

pub type WrappedNames = Names

pub fn make() WrappedNames {
	return WrappedNames(Names(['first']))
}
"

fn shared_locals_run(name string, files map[string]string) string {
	root := os.join_path(os.vtmp_dir(), 'shared_array_locals_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	for rel, source in files {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source) or { panic(err) }
	}
	bin := os.join_path(root, 'main_bin')
	compile := os.exec([shared_locals_vexe, '-o', bin, os.join_path(root, 'main.v')])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// The elements of a `[]shared T` local or parameter are lock wrappers. The program needs
// no `import sync`, and closures that capture the array keep its element type.
fn test_shared_array_locals_parameters_and_captures() {
	out := shared_locals_run('plain', {
		'main.v': 'struct Foo {
mut:
	x int
}

fn (f Foo) get() int {
	return f.x
}

fn (mut f Foo) bump() int {
	f.x++
	return f.x
}

fn query(items []shared Foo) {
	rlock items[0] {
		println(items[0].get())
	}
	lock items[0] {
		println(items[0].bump())
	}
	cb := fn [items] () {
		rlock items[0] {
			println(items[0].get())
		}
	}
	cb()
}

fn main() {
	mut items := []shared Foo{}
	items << Foo{
		x: 1
	}
	lock items[0] {
		println(items[0].bump())
	}
	callback := fn [items] () {
		lock items[0] {
			println(items[0].bump())
		}
	}
	callback()
	query(items)
	println(items.len)
}
'
	})
	assert out.split_into_lines() == ['2', '3', '3', '4', '4', '1']
}

// An element type spelled through an import alias names the same lock wrapper in every
// function, and the methods that the alias inherits from another alias are called on it.
fn test_shared_elements_of_an_imported_alias() {
	out := shared_locals_run('alias', {
		'aliases/aliases.v': shared_locals_aliases_module
		'main.v':            'module main

import aliases as renamed

struct Items {
mut:
	values []shared renamed.WrappedNames
}

struct Holder {
	values shared renamed.WrappedNames
}

fn read(items []shared renamed.WrappedNames) string {
	rlock items[0] {
		return items[0].clone()[0].name
	}
	return ""
}

fn main() {
	mut items := []shared renamed.WrappedNames{}
	items << renamed.make()
	rlock items[0] {
		println(items[0].clone()[0].name)
	}
	callback := fn [items] () {
		lock items[0] {
			println(items[0].mutate()[0].name)
		}
	}
	callback()
	println(read(items))
	mut collection := Items{}
	collection.values << renamed.make()
	rlock (collection.values)[0] {
		println((collection.values)[0].clone()[0].name)
	}
	holders := {
		"primary": Holder{
			values: renamed.make()
		}
	}
	lock holders["primary"].values {
		println(holders["primary"].values.mutate()[0].name)
	}
	rlock (holders["primary"]).values {
		println((holders["primary"]).values.clone()[0].name)
	}
}
'
	})
	assert out.split_into_lines() == ['first', 'mutated', 'mutated', 'first', 'mutated', 'mutated']
}

// `type WrappedNames = Names` inherits `Names.clone()`. It wins over the array's builtin
// `clone()` for an index or a dereference too, not only for a variable.
fn test_methods_inherited_through_another_alias() {
	out := shared_locals_run('inherited', {
		'aliases/aliases.v': shared_locals_aliases_module
		'main.v':            'module main

import aliases

fn main() {
	names := aliases.make()
	p := &names
	arr := [aliases.make()]
	println(names.clone()[0].name)
	println(arr[0].clone()[0].name)
	println(p.clone()[0].name)
	println((*p).clone()[0].name)
}
'
	})
	assert out.split_into_lines() == ['first', 'first', 'first', 'first']
}
