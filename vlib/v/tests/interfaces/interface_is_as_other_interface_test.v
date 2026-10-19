// Tests and casts of an interface value against another interface are checked at
// run time against the dynamic type of the value. They are valid even when no type
// in the program implements both interfaces: the test is then always false.
import os

interface Named {
	name() string
}

interface Sized {
	size() int
}

// No implementer of `Named` implements `Tagged`.
interface Tagged {
	tag() string
}

// `Named` does not carry the `count` field.
interface Counted {
	count int
	name() string
}

// Plain implements only `Named`.
struct Plain {}

fn (p Plain) name() string {
	return 'plain'
}

// Box implements `Named` and `Sized`.
struct Box {
	n int
}

fn (b Box) name() string {
	return 'box'
}

fn (b Box) size() int {
	return b.n
}

// Bag implements `Named`, `Sized` and `Counted`.
struct Bag {
	count int
}

fn (b Bag) name() string {
	return 'bag'
}

fn (b Bag) size() int {
	return b.count * 10
}

// Label implements `Tagged`, but not `Named`.
struct Label {}

fn (l Label) tag() string {
	return 'label'
}

// Fault is an error that implements `Tagged`.
struct Fault {
	Error
}

fn (f Fault) msg() string {
	return 'fault'
}

fn (f Fault) tag() string {
	return 'fault tag'
}

fn fail(fault bool) ! {
	if fault {
		return Fault{}
	}
	return error('plain')
}

fn error_tag(fault bool) string {
	fail(fault) or {
		if err is Tagged {
			return err.tag()
		}
		// No error type implements `Sized`.
		return match err {
			Sized { 'sized ${err.size()}' }
			else { err.msg() }
		}
	}
	return ''
}

fn is_sized(x Named) bool {
	return x is Sized
}

fn is_tagged(x Named) bool {
	return x is Tagged
}

fn is_not_tagged(x Named) bool {
	return x !is Tagged
}

fn is_tagged_ptr(x &Named) bool {
	return x is Tagged
}

fn counted_is_tagged(x Counted) bool {
	return x is Tagged
}

fn is_generic[T](x Named) bool {
	return x is T
}

fn if_size(x Named) int {
	if x is Sized {
		return x.size()
	}
	return -1
}

fn if_tag(x Named) string {
	if x is Tagged {
		return x.tag()
	}
	return 'untagged ${x.name()}'
}

fn if_mut_size(mut x Named) int {
	if mut x is Sized {
		return x.size()
	}
	return -1
}

fn if_count(x Named) int {
	if x is Counted {
		return x.count
	}
	return -1
}

fn match_kind(x Named) string {
	return match x {
		Tagged { 'tag ${x.tag()}' }
		Sized { 'size ${x.size()}' }
		else { 'name ${x.name()}' }
	}
}

fn as_size(x Named) int {
	s := x as Sized
	return s.size()
}

fn as_tag_after_is(x Named) string {
	if x is Tagged {
		t := x as Tagged
		return t.tag()
	}
	return 'none'
}

fn test_is_against_another_interface() {
	plain := Named(Plain{})
	box := Named(Box{3})
	bag := Named(Bag{4})
	assert !is_sized(plain)
	assert is_sized(box)
	assert is_sized(bag)
	for x in [plain, box, bag] {
		assert !is_tagged(x)
		assert is_not_tagged(x)
		assert !is_tagged_ptr(&x)
		assert x !is Tagged
	}
	assert !counted_is_tagged(Bag{2})
	assert Tagged(Label{}).tag() == 'label'
}

fn test_generic_is_against_another_interface() {
	assert !is_generic[Sized](Plain{})
	assert is_generic[Sized](Box{1})
	assert !is_generic[Tagged](Box{1})
	assert !is_generic[Counted](Box{1})
	assert is_generic[Counted](Bag{1})
}

fn test_smartcast_to_another_interface() {
	assert if_size(Plain{}) == -1
	assert if_size(Box{3}) == 3
	assert if_size(Bag{4}) == 40
	assert if_tag(Box{3}) == 'untagged box'
	mut plain := Named(Plain{})
	mut bag := Named(Bag{5})
	assert if_mut_size(mut plain) == -1
	assert if_mut_size(mut bag) == 50
	assert if_count(Box{3}) == -1
	assert if_count(Bag{6}) == 6
	assert match_kind(Plain{}) == 'name plain'
	assert match_kind(Box{2}) == 'size 2'
	assert match_kind(Bag{2}) == 'size 20'
}

fn test_as_to_another_interface() {
	assert as_size(Box{7}) == 7
	assert as_size(Bag{1}) == 10
	assert as_tag_after_is(Bag{1}) == 'none'
}

fn test_error_against_another_interface() {
	assert error_tag(true) == 'fault tag'
	assert error_tag(false) == 'plain'
	fail(true) or {
		assert err !is Sized
		t := err as Tagged
		assert t.tag() == 'fault tag'
	}
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

// test_failed_as_panics compiles a separate program: whether a type implements both
// interfaces is a property of the whole program.
fn test_failed_as_panics() {
	dir := os.join_path(os.vtmp_dir(), 'interface_as_other_interface_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	program := os.join_path(dir, 'interface_as_other_interface')
	os.write_file(source, '
import os

interface Named {
	name() string
}

interface Sized {
	size() int
}

interface Tagged {
	tag() string
}

interface Counted {
	count int
}

struct Plain {}

fn (p Plain) name() string {
	return "plain"
}

struct Box {}

fn (b Box) name() string {
	return "box"
}

fn (b Box) size() int {
	return 5
}

struct Label {
	count int
}

fn (l Label) tag() string {
	return "label"
}

fn fail() ! {
	return error("plain")
}

fn main() {
	x := Named(Plain{})
	match os.args[1] {
		"sized" {
			s := x as Sized
			println(s.size())
		}
		"tagged" {
			t := x as Tagged
			println(t.tag())
		}
		"counted" {
			c := x as Counted
			println(c.count)
		}
		"error" {
			fail() or {
				t := err as Tagged
				println(t.tag())
			}
		}
		else {
			println("\${x is Sized} \${x is Tagged} \${x is Counted}")
			println("\${Named(Box{}) is Sized} \${Named(Box{}) is Tagged}")
		}
	}
}
')!
	code, output := run_child(@VEXE, ['-o', program, source])
	assert code == 0, output
	is_code, is_output := run_child(program, ['is'])
	assert is_code == 0, is_output
	assert is_output.trim_space() == 'false false false\ntrue false'
	for mode, message in {
		'sized':   'as cast: cannot cast `main.Plain` to `main.Sized`'
		'tagged':  'as cast: cannot cast `main.Plain` to `main.Tagged`'
		'counted': 'as cast: cannot cast `main.Plain` to `main.Counted`'
		'error':   'as cast: cannot cast `MessageError` to `main.Tagged`'
	} {
		exit_code, panic_output := run_child(program, [mode])
		assert exit_code != 0, '${mode}: ${panic_output}'
		assert panic_output.contains(message), panic_output
	}
}
