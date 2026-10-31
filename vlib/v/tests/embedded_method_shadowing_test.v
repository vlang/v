struct Base {
mut:
	v int
}

fn (mut b Base) init() {
	b.v = 7
}

fn (b &Base) name() string {
	return 'base'
}

struct Derived {
	Base
mut:
	w int
}

fn (mut d Derived) init() {
	d.Base.init()
	d.w = 1
}

struct Leaf {
	Derived
mut:
	z int
}

fn (mut l Leaf) init() {
	l.Derived.init()
	l.z = 2
}

struct Plain {
	Derived
}

struct Other {
mut:
	v2 int
}

fn (mut o Other) init() {
	o.v2 = 5
}

struct Both {
	Derived
	Other
}

fn (mut b Both) init() {
	b.Derived.init()
	b.Other.init()
}

// A method of a struct hides the methods of the same name of the structs it embeds,
// at any depth.
fn test_own_method_hides_embedded_methods() {
	mut leaf := Leaf{}
	leaf.init()
	assert leaf.v == 7
	assert leaf.w == 1
	assert leaf.z == 2
	assert leaf.name() == 'base'
}

// A method of an embedded struct hides those of the structs that one embeds.
fn test_embedded_method_hides_deeper_embedded_methods() {
	mut plain := Plain{}
	plain.init()
	assert plain.v == 7
	assert plain.w == 1
}

fn test_own_method_hides_methods_of_several_embedded_structs() {
	mut both := Both{}
	both.init()
	assert both.v == 7
	assert both.w == 1
	assert both.v2 == 5
}
