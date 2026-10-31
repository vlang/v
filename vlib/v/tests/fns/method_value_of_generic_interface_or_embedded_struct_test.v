// A method named without a call is a value that calls it, also when the type of
// its receiver declares it through a generic interface, `Shelf[User]`, or
// through a struct that the receiver's struct embeds. The check rejected both,
// `type has no field named`, which a call of the method passed.

interface Shelf[T] {
	get() T
}

interface Keeper[T] {
mut:
	keep(item T) int
}

struct User {
	name string
}

struct UserShelf {
	user User
}

fn (s UserShelf) get() User {
	return s.user
}

struct UserKeeper {
mut:
	kept []User
}

fn (mut k UserKeeper) keep(item User) int {
	k.kept << item
	return k.kept.len
}

fn shelf_getter(s Shelf[User]) fn () User {
	return s.get
}

fn name_of(get fn () User) string {
	return get().name
}

fn call_it(f fn () string) string {
	return f()
}

fn keep_twice(mut k Keeper[User]) int {
	keep := k.keep
	keep(User{'a'})
	return keep(User{'b'})
}

struct Base {
mut:
	id int
}

fn (b Base) describe() string {
	return 'base ${b.id}'
}

fn (mut b Base) bump() {
	b.id++
}

struct Plain {
	Base
	name string
}

struct Outer {
	Plain
}

struct Own {
	Base
}

fn (o Own) describe() string {
	return 'own ${o.id}'
}

struct Wrap[T] {
	Base
	item T
}

type Bare = Plain

struct Holder[T] {
	item T
}

fn (h Holder[T]) get() T {
	return h.item
}

struct IntHolder {
	Holder[int]
}

fn test_a_method_of_a_generic_interface_named_without_a_call_calls_it() {
	shelf := UserShelf{User{'ana'}}
	getter := shelf_getter(shelf)
	assert getter().name == 'ana'
}

fn test_a_method_of_a_generic_interface_is_passed_as_an_argument() {
	s := Shelf[User](UserShelf{User{'bea'}})
	assert name_of(s.get) == 'bea'
	shelves := [s]
	assert name_of(shelves[0].get) == 'bea'
}

fn test_a_method_of_a_generic_interface_takes_its_parameters_with_the_type_arguments() {
	mut keeper := UserKeeper{}
	assert keep_twice(mut keeper) == 2
	assert keeper.kept.map(it.name) == ['a', 'b']
}

fn test_a_method_of_an_embedded_struct_named_without_a_call_calls_it() {
	p := Plain{
		Base: Base{7}
		name: 'p'
	}
	describe := p.describe
	assert describe() == 'base 7'
	o := Outer{
		Plain: p
	}
	outer_describe := o.describe
	assert outer_describe() == 'base 7'
	q := &Plain{
		Base: Base{8}
	}
	pointer_describe := q.describe
	assert pointer_describe() == 'base 8'
}

fn test_a_mut_method_of_an_embedded_struct_changes_the_embedded_value() {
	mut p := &Plain{
		Base: Base{1}
	}
	bump := p.bump
	bump()
	bump()
	assert p.id == 3
}

fn test_a_method_of_an_embedded_generic_struct_named_without_a_call_calls_it() {
	mut h := IntHolder{}
	h.Holder = Holder[int]{
		item: 5
	}
	get := h.get
	assert get() == 5
}

fn test_a_method_of_a_struct_embedded_in_a_generic_struct_named_without_a_call_calls_it() {
	w := Wrap[int]{
		Base: Base{2}
		item: 9
	}
	describe := w.describe
	assert describe() == 'base 2'
	assert call_it(w.describe) == 'base 2'
}

fn test_a_method_of_the_struct_itself_wins_over_the_embedded_one() {
	o := Own{
		Base: Base{1}
	}
	describe := o.describe
	assert describe() == 'own 1'
}

fn test_a_method_of_an_embedded_struct_named_through_an_alias_calls_it() {
	b := Bare(Plain{
		Base: Base{5}
	})
	describe := b.describe
	assert describe() == 'base 5'
}
