import os

// A self-referential interface -- one with a method whose return type is the
// interface itself -- used to send the interface satisfaction check into
// unbounded recursion, exhausting the checker's stack before it could report
// anything. Declaring the interface was enough on its own; no implementing
// type, cast or call was required.
interface Cloner {
	clone() Cloner
}

struct Leaf {
	id int
}

fn (l Leaf) clone() Cloner {
	return Leaf{
		id: l.id + 1
	}
}

fn test_self_referential_interface_method() {
	c := Cloner(Leaf{
		id: 1
	})
	next := c.clone()
	assert next is Leaf
	if next is Leaf {
		assert next.id == 2
	}
}

// Mutually self-referential interfaces close the same cycle indirectly.
interface Alpha {
	to_beta() Beta
}

interface Beta {
	to_alpha() Alpha
	tag() string
}

struct Both {}

fn (b Both) to_beta() Beta {
	return Both{}
}

fn (b Both) to_alpha() Alpha {
	return Both{}
}

fn (b Both) tag() string {
	return 'both'
}

fn test_mutually_self_referential_interfaces() {
	a := Alpha(Both{})
	b := a.to_beta()
	assert b.tag() == 'both'
	assert b.to_alpha() is Both
}

// Two distinct interfaces of the same shape, each with a method returning its
// own interface type, close the cycle through the interface-to-interface
// comparison rather than through a concrete type. Name equality cannot stop it,
// because the two return types have different names. The compiler compares
// declared interfaces against each other while building its implementation
// index, so declaring both is enough to reach this.
interface FirstCloner {
	clone() FirstCloner
}

interface SecondCloner {
	clone() SecondCloner
}

struct FirstLeaf {
	id int
}

fn (f FirstLeaf) clone() FirstCloner {
	return FirstLeaf{
		id: f.id + 1
	}
}

struct SecondLeaf {
	id int
}

fn (s SecondLeaf) clone() SecondCloner {
	return SecondLeaf{
		id: s.id + 1
	}
}

fn test_two_same_shaped_self_referential_interfaces() {
	first := FirstCloner(FirstLeaf{
		id: 1
	})
	second := SecondCloner(SecondLeaf{
		id: 10
	})
	next_first := first.clone()
	next_second := second.clone()
	assert next_first is FirstLeaf
	assert next_second is SecondLeaf
	if next_first is FirstLeaf {
		assert next_first.id == 2
	}
	if next_second is SecondLeaf {
		assert next_second.id == 11
	}
}

// Interfaces whose methods take their own interface type as a parameter reach
// the same comparison through the parameter rather than the return type.
interface FirstAccepts {
	accepts(FirstAccepts) bool
}

interface SecondAccepts {
	accepts(SecondAccepts) bool
}

struct AcceptsLeaf {}

fn (a AcceptsLeaf) accepts(other FirstAccepts) bool {
	return true
}

fn test_same_shaped_self_referential_interface_parameters() {
	first := FirstAccepts(AcceptsLeaf{})
	assert first.accepts(AcceptsLeaf{})
}

// Treating an in-progress pair as satisfied must not let a type through that
// fails a different requirement of the same interface.
fn test_self_referential_interface_requirements_still_enforced() {
	tmp := os.join_path(os.temp_dir(), 'self_referential_interface_${os.getpid()}')
	os.mkdir_all(tmp)!
	defer {
		os.rmdir_all(tmp) or {}
	}
	source := os.join_path(tmp, 'main.v')
	os.write_file(source, "interface Incomplete {
	clone() Incomplete
	tag() string
}

struct Complete {}

fn (c Complete) clone() Incomplete {
	return Complete{}
}

fn (c Complete) tag() string {
	return 'complete'
}

struct Partial {}

fn (p Partial) clone() Incomplete {
	return Complete{}
}

fn main() {
	println(Incomplete(Partial{}))
}
")!
	output := os.join_path(tmp, 'main.c')
	result := os.exec([@VEXE, '-o', output, source])
	assert result.exit_code != 0, result.output
	assert result.output.contains("doesn't implement method `tag`"), result.output
}

// A pair that is still being proved must not count as proven: otherwise any type with
// `clone() Self`, such as `string`, implements `Cloner` through a covariant return, and
// same-shaped interfaces implement each other, none of which cgen can dispatch.
fn test_self_referential_interface_cycle_is_not_assumed() {
	tmp := os.join_path(os.temp_dir(), 'self_referential_interface_cycle_${os.getpid()}')
	os.mkdir_all(tmp)!
	defer {
		os.rmdir_all(tmp) or {}
	}
	source := os.join_path(tmp, 'main.v')
	os.write_file(source, "interface Cloner {
	clone() Cloner
}

interface OtherCloner {
	clone() OtherCloner
}

struct Leaf {}

fn (l Leaf) clone() Cloner {
	return Leaf{}
}

fn main() {
	c := Cloner(Leaf{})
	println(OtherCloner(c))
	println(Cloner('abc'))
}
")!
	output := os.join_path(tmp, 'main.c')
	result := os.exec([@VEXE, '-o', output, source])
	assert result.exit_code != 0, result.output
	assert result.output.contains('incorrectly implements method `clone` of interface `Cloner`'), result.output
}

// The recursion happened while the checker built its interface implementation
// index, so these declarations alone are enough to exercise it: the file does
// not compile at all unless the cycle terminates. The shapes below reach the
// comparison through a tuple, an Option and a function return respectively.
interface TupleCloner {
	clone() (TupleCloner, int)
}

interface OptionCloner {
	clone() ?OptionCloner
}

interface FnCloner {
	clone() fn () FnCloner
}
