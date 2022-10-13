struct Poss1 {}

struct Poss2 {}

type Possibilities = Poss1 | Poss2

// comment out this attribute to make error go away BUT now you need to remember to use &PossOwner everytime you create new instance :(
[heap]
pub struct PossOwner {
pub:
	name    string
	details Possibilities
}

fn (t PossOwner) get_file(path string) ?(PossOwner, Poss1) {
	match t.details {
		Poss1 { return t, t.details }
		else { return error('not a file') }
	}
}

fn (item PossOwner) check() ?int {
	assert item.name == 'x'

	return 0
}

fn somefun() ?int {
	t := PossOwner{
		name: 'x'
		details: Poss1{}
	}

	path_info, _ := t.get_file('') or {
		println('NOTOK1')
		return 1
	}

	_ := path_info.check() or {
		println('NOTOK2')
		return 2
	}

	return 0
}

fn test_assign_optional_of_multi_return_with_heap() {
	somefun() or { return }
	println('success')
}
