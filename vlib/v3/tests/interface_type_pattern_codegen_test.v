import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_interface_type_pattern_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_interface_type_patterns_resolve_imported_concrete_types() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_interface_type_pattern_${os.getpid()}')
	mod_dir := os.join_path(root, 'ifacepkg')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'ifacepkg.v'), "module ifacepkg

pub interface Thing {
	name() string
}

pub struct Foo {}

pub fn (f Foo) name() string {
	return 'foo'
}

pub struct Bar {}

pub fn (b Bar) name() string {
	return 'bar'
}

pub fn make_foo() Thing {
	return Foo{}
}

pub fn make_bar() Thing {
	return Bar{}
}

pub fn is_foo(x Thing) bool {
	return x is Foo
}

pub fn classify(x Thing) string {
	match x {
		Foo {
			return 'foo'
		}
		Bar {
			return 'bar'
		}
		else {
			return 'else'
		}
	}
}
") or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'import ifacepkg
import ifacepkg { Foo }

interface Named {
	name string
}

struct User {
	name string
	age  int
}

fn describe_named(n Named) string {
	return match n {
		User { n.name + ":" + int_str(n.age) }
		else { n.name }
	}
}

interface Labelled {
	label string
}

struct Box {
	label string
	size  int
}

struct Other {
	label string
}

type BoxSum = Box | Other

fn describe_labelled(x Labelled) string {
	return match x {
		Box { x.label + ":" + int_str(x.size) }
		else { x.label }
	}
}

fn main() {
	foo := ifacepkg.make_foo()
	bar := ifacepkg.make_bar()
	println(ifacepkg.is_foo(foo).str())
	println(ifacepkg.is_foo(bar).str())
	println((foo is ifacepkg.Foo).str())
	println((bar is ifacepkg.Foo).str())
	println((foo is Foo).str())
	println((bar is Foo).str())
	println(ifacepkg.classify(foo))
	println(ifacepkg.classify(bar))
	println(describe_named(User{
		name: "Ada"
		age: 37
	}))
	println(describe_labelled(Box{
		label: "b"
		size: 4
	}))
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_interface_type_pattern_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('._typ =='), c_code
	assert !c_code.contains('return x._object != NULL;'), c_code
	assert !c_code.contains('x == Foo'), c_code
	assert !c_code.contains('x == Bar'), c_code
	assert !c_code.contains('n.age'), c_code
	assert !c_code.contains('x.Box'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true\nfalse\ntrue\nfalse\ntrue\nfalse\nfoo\nbar\nAda:37\nb:4'
}

fn test_interface_type_patterns_resolve_import_aliases() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_interface_type_pattern_alias_${os.getpid()}')
	mod_dir := os.join_path(root, 'shapes')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'shapes.v'), 'module shapes

pub interface Shape {
	area() int
}

pub struct Rect {
pub:
	w int
}

pub fn (r Rect) area() int {
	return r.w
}

pub struct Circle {
pub:
	r int
}

pub fn (c Circle) area() int {
	return c.r
}

pub fn make_rect() Shape {
	return Rect{
		w: 3
	}
}

pub fn make_circle() Shape {
	return Circle{
		r: 4
	}
}
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'import shapes as sh
import shapes { Shape }

fn is_rect(s Shape) bool {
	return s is sh.Rect
}

fn classify(s Shape) string {
	return match s {
		sh.Rect { "rect" }
		else { "other" }
	}
}

fn main() {
	rect := sh.make_rect()
	circle := sh.make_circle()
	println(is_rect(rect).str())
	println(is_rect(circle).str())
	println(classify(rect))
	println(classify(circle))
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_interface_type_pattern_alias_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('._typ =='), c_code
	assert !c_code.contains('return true;\\n}\\n\\nstring classify'), c_code
	assert !c_code.contains('s == shapes__Rect'), c_code
	assert !c_code.contains('s == sh__Rect'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true\nfalse\nrect\nother'
}
