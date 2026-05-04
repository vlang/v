type ProcedureHandler = fn () !string

struct Procedure[T, U] {
	value    T
	function fn (T) !U = unsafe { nil }
}

fn (p Procedure[T, U]) handle() !string {
	result := p.function(p.value) or { return err }
	return encode[U](result)
}

fn encode[T](value T) string {
	$if T is Dog {
		return 'dog:${value.name}'
	}
	$if T is Cat {
		return 'cat:${value.name}'
	}
	return 'unknown'
}

struct Dog {
	name string
}

struct NewDog {
	name string
}

fn make_dog(value NewDog) !Dog {
	return Dog{
		name: value.name
	}
}

struct Cat {
	name string
}

struct NewCat {
	name string
}

fn make_cat(value NewCat) !Cat {
	return Cat{
		name: value.name
	}
}

fn test_generic_fn_field_multi_instance_specialization() {
	dog := Procedure[NewDog, Dog]{
		value:    NewDog{'leo'}
		function: make_dog
	}
	cat := Procedure[NewCat, Cat]{
		value:    NewCat{'milo'}
		function: make_cat
	}
	handlers := [ProcedureHandler(dog.handle), ProcedureHandler(cat.handle)]
	assert (handlers[0]() or { panic(err) }) == 'dog:leo'
	assert (handlers[1]() or { panic(err) }) == 'cat:milo'
}
