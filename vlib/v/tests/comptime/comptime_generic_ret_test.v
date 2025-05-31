struct Parent {
pub mut:
	id    int
	name  string
	child Child
	other Other
}

struct Child {
pub mut:
	id  int
	age int
}

struct Other {
pub mut:
	id   int
	name string
}

interface IdInterface {
mut:
	id int
}

fn insert_ids[T](val T) !T {
	mut clone := val

	$for field in T.fields {
		$if field.typ is $struct {
			clone.$(field.name) = insert_ids(val.$(field.name))!
		}
	}
	$if T is IdInterface {
		clone.id = 1
	} $else {
		return error('${T.name} does not have an id field!')
	}
	return clone
}

fn test_main() {
	inserted := insert_ids(Parent{
		name: 'test'
	})!
	assert inserted.child.id == 1
	assert inserted.other.id == 1
}
