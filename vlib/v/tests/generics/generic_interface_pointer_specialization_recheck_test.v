interface Issue28471Speaker {
mut:
	speak() string
}

struct Issue28471Dog {
mut:
	n int
}

fn (mut d Issue28471Dog) speak() string {
	d.n++
	return 'woof'
}

struct Issue28471Box[T] {
	item T
}

fn issue28471_apply_one_interface_then_pointer[T](b Issue28471Box[T], f fn (mut T) string) string {
	mut it := b.item
	return f(mut it)
}

fn issue28471_apply_one_pointer_then_interface[T](b Issue28471Box[T], f fn (mut T) string) string {
	mut it := b.item
	return f(mut it)
}

fn issue28471_apply_two_interface_then_pointer[T, R](b Issue28471Box[T], f fn (mut T) R) R {
	mut it := b.item
	return f(mut it)
}

fn issue28471_apply_two_pointer_then_interface[T, R](b Issue28471Box[T], f fn (mut T) R) R {
	mut it := b.item
	return f(mut it)
}

fn test_generic_interface_then_pointer_specializations_keep_mutation_target() {
	mut d := Issue28471Dog{}
	speaker_box := Issue28471Box[Issue28471Speaker]{
		item: Issue28471Speaker(&d)
	}
	dog_box := Issue28471Box[&Issue28471Dog]{
		item: &d
	}

	assert issue28471_apply_one_interface_then_pointer[Issue28471Speaker](speaker_box, fn (mut it Issue28471Speaker) string {
		return it.speak()
	}) == 'woof'
	assert issue28471_apply_one_interface_then_pointer[&Issue28471Dog](dog_box, fn (mut it &Issue28471Dog) string {
		return it.speak()
	}) == 'woof'
	assert d.n == 2
}

fn test_generic_pointer_then_interface_specializations_keep_mutation_target() {
	mut d := Issue28471Dog{}
	speaker_box := Issue28471Box[Issue28471Speaker]{
		item: Issue28471Speaker(&d)
	}
	dog_box := Issue28471Box[&Issue28471Dog]{
		item: &d
	}

	assert issue28471_apply_one_pointer_then_interface[&Issue28471Dog](dog_box, fn (mut it &Issue28471Dog) string {
		return it.speak()
	}) == 'woof'
	assert issue28471_apply_one_pointer_then_interface[Issue28471Speaker](speaker_box, fn (mut it Issue28471Speaker) string {
		return it.speak()
	}) == 'woof'
	assert d.n == 2
}

fn test_generic_two_param_interface_then_pointer_specializations_keep_mutation_target() {
	mut d := Issue28471Dog{}
	speaker_box := Issue28471Box[Issue28471Speaker]{
		item: Issue28471Speaker(&d)
	}
	dog_box := Issue28471Box[&Issue28471Dog]{
		item: &d
	}

	assert issue28471_apply_two_interface_then_pointer[Issue28471Speaker, string](speaker_box, fn (mut it Issue28471Speaker) string {
		return it.speak()
	}) == 'woof'
	assert issue28471_apply_two_interface_then_pointer[&Issue28471Dog, string](dog_box, fn (mut it &Issue28471Dog) string {
		return it.speak()
	}) == 'woof'
	assert d.n == 2
}

fn test_generic_two_param_pointer_then_interface_specializations_keep_mutation_target() {
	mut d := Issue28471Dog{}
	speaker_box := Issue28471Box[Issue28471Speaker]{
		item: Issue28471Speaker(&d)
	}
	dog_box := Issue28471Box[&Issue28471Dog]{
		item: &d
	}

	assert issue28471_apply_two_pointer_then_interface[&Issue28471Dog, string](dog_box, fn (mut it &Issue28471Dog) string {
		return it.speak()
	}) == 'woof'
	assert issue28471_apply_two_pointer_then_interface[Issue28471Speaker, string](speaker_box, fn (mut it Issue28471Speaker) string {
		return it.speak()
	}) == 'woof'
	assert d.n == 2
}
