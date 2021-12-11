module main

struct Abc {
    a byte
    b byte
    c byte
}

fn decode<T>() T {
    mut x := T{}
    $for field in T.fields {
        $if field.typ is byte {
            x.$(field.name) = 1
        } $else {
            x.$(field.name) = "test"
        }
        x.$(field.name) = 5
    }
    return x
}

fn main() {
    abc := decode<Abc>()
	assert abc.a == abc.b == abc.c == 5
}