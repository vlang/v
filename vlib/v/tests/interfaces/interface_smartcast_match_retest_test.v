// A `match` on a value that an earlier `is` narrowed tests its type again.
interface Animal {
	name() string
}

struct Dog {
	n string
}

fn (d Dog) name() string {
	return d.n
}

struct Cat {}

fn (c Cat) name() string {
	return 'cat'
}

type Pet = Cat | Dog

type Num = int | string

fn after_noreturn_else(a Animal) string {
	if a is Dog {
		assert a.n.len > 0
	} else {
		panic('not a dog')
	}
	return match a {
		Dog { 'dog ${a.n}' }
		else { 'other' }
	}
}

fn after_negated_early_return(a Animal) string {
	if a !is Dog {
		return 'early'
	}
	return match a {
		Cat { 'cat' }
		Dog { 'dog ${a.n}' }
		else { 'other' }
	}
}

fn after_assert(a Animal) string {
	assert a is Dog
	mut s := ''
	match a {
		Dog { s = 'dog ${a.n}' }
		else { s = 'other' }
	}
	t := match a {
		Dog { a.n }
		else { '' }
	}
	return '${s} ${t}'
}

fn inside_branch(a Animal) string {
	if a is Dog {
		return match a {
			Dog { 'inner ${a.n}' }
			else { 'inner other' }
		}
	}
	return 'outer'
}

fn sum_after_early_return(p Pet) string {
	if p is Cat {
		return 'cat'
	}
	return match p {
		Cat { 'cat again' }
		Dog { 'dog ${p.n}' }
	}
}

fn narrowed_value_match(n Num) string {
	if n is int {
		return match n {
			1 { 'one' }
			else { 'int ${n}' }
		}
	}
	return 'string'
}

fn test_interface_match_after_noreturn_else() {
	assert after_noreturn_else(Animal(Dog{'rex'})) == 'dog rex'
}

fn test_interface_match_after_negated_early_return() {
	assert after_negated_early_return(Animal(Dog{'max'})) == 'dog max'
	assert after_negated_early_return(Animal(Cat{})) == 'early'
}

fn test_interface_match_after_assert() {
	assert after_assert(Animal(Dog{'fido'})) == 'dog fido fido'
}

fn test_interface_match_inside_is_branch() {
	assert inside_branch(Animal(Dog{'bo'})) == 'inner bo'
	assert inside_branch(Animal(Cat{})) == 'outer'
}

fn test_sum_type_match_after_early_return() {
	assert sum_after_early_return(Pet(Dog{'rex'})) == 'dog rex'
	assert sum_after_early_return(Pet(Cat{})) == 'cat'
}

fn test_value_match_on_narrowed_variant() {
	assert narrowed_value_match(Num(1)) == 'one'
	assert narrowed_value_match(Num(5)) == 'int 5'
	assert narrowed_value_match(Num('s')) == 'string'
}
