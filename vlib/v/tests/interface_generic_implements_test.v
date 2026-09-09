interface Foo[T] {
	val T
	foo() T
}

struct Bar[T] implements Foo[T] {
	val T
}

fn (b Bar[T]) foo() T {
	return b.val
}

fn test_main() {
	b := Bar{
		val: 0
	}
	assert b.foo() == 0
}

interface Any {}

interface AInterface {
	a int
}

struct StructA {
mut:
	a int
}

struct Container {
mut:
	any Any
}

fn (mut c Container) set[T](obj Any) {
	c.any = obj
}

fn (c &Container) get[T]() !&T {
	if c.any is T {
		return c.any
	}
	return error('ops')
}

fn test_generic_interface_value_can_be_returned_as_interface_ref() {
	a := &StructA{
		a: 100
	}
	b := &StructA{
		a: 200
	}
	mut c := &Container{
		any: a
	}
	c.set[StructA](b)
	c.set[AInterface](b)
	d := c.get[StructA]() or { panic(err) }
	e := c.get[AInterface]() or { panic(err) }
	assert d.a == 200
	assert e.a == 200
}

interface FilterModel {
	model string
}

interface FilterCar {
	name string
}

struct FilterCarRecord {
	name  string
	model string
}

fn collect_matching[T](mut cars []FilterCar) []T {
	mut matches := []T{}
	for mut car in cars {
		if mut car is T {
			matches << car
		}
	}
	return matches
}

fn test_generic_interface_to_interface_smartcast() {
	mut cars := []FilterCar{}
	cars << FilterCarRecord{
		name:  'Roadster'
		model: 'Tesla'
	}
	cars << FilterCarRecord{
		name:  'Corolla'
		model: 'Toyota'
	}

	models := collect_matching[FilterModel](mut cars)

	assert models.len == 2
	assert models[0].model == 'Tesla'
	assert models[1].model == 'Toyota'
}
