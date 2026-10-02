// A method of a struct alias named without a call is a value that calls the
// method of the alias, as its call does. The value was emitted as a field of the
// aliased struct, which the C compiler rejected.

struct Plain {
mut:
	id int
}

type Alias = Plain

fn (a Alias) describe() string {
	return 'alias ${a.id}'
}

fn (mut a Alias) bump() {
	a.id++
}

struct Base {
	id int
}

fn (b Base) describe() string {
	return 'base ${b.id}'
}

struct WithBase {
	Base
}

type Shadow = WithBase

fn (s Shadow) describe() string {
	return 'shadow ${s.id}'
}

fn call_it(f fn () string) string {
	return f()
}

fn test_a_method_of_a_struct_alias_named_without_a_call_calls_it() {
	a := Alias(Plain{3})
	describe := a.describe
	assert describe() == 'alias 3'
	assert call_it(a.describe) == 'alias 3'
}

fn test_a_mut_method_of_a_struct_alias_named_without_a_call_changes_it() {
	mut a := &Alias(&Plain{
		id: 1
	})
	bump := a.bump
	bump()
	assert a.id == 2
}

fn test_the_method_of_the_alias_comes_before_the_one_its_struct_promotes() {
	s := Shadow(WithBase{
		Base: Base{7}
	})
	describe := s.describe
	assert describe() == 'shadow 7'
}
