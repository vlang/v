// Interface casts in const initializers box their source in `_vinit`; they must
// not become a zero C static initializer (vlang/v#30674).
struct BigError {
	Error
	cap int
}

fn (e BigError) msg() string {
	return 'big ${e.cap}'
}

struct EmptyError {
	Error
}

fn (e EmptyError) msg() string {
	return 'empty'
}

interface Shape {
	area() int
}

struct Square {
	side int
}

fn (s Square) area() int {
	return s.side * s.side
}

interface Named {
	name string
	greet() string
}

struct Person {
	name string
	age  int
}

fn (p Person) greet() string {
	return '${p.name} is ${p.age}'
}

struct ErrorHolder {
	err IError
}

struct ShapeHolder {
	shape Shape
}

const big_err = IError(BigError{
	cap: 65536
})
const empty_err = IError(EmptyError{})
const big_err_alias = big_err
const square = Shape(Square{
	side: 7
})
const person = Named(Person{
	name: 'bob'
	age:  3
})
const explicit_holder = ErrorHolder{
	err: IError(BigError{
		cap: 11
	})
}
const implicit_holder = ErrorHolder{
	err: BigError{
		cap: 12
	}
}
const shape_holder = ShapeHolder{
	shape: Square{
		side: 4
	}
}
const fixed_errors = [IError(BigError{
	cap: 21
}), IError(EmptyError{})]!
const fixed_shapes = [Shape(Square{
	side: 2
}), Shape(Square{
	side: 3
})]!
const error_list = [IError(BigError{
	cap: 31
}), IError(EmptyError{})]

fn fail_with_const() ! {
	return big_err
}

fn test_ierror_cast_const() {
	fail_with_const() or {
		assert err.msg() == 'big 65536'
		assert err is BigError
		return
	}
	assert false
}

fn test_empty_struct_ierror_cast_const() {
	assert empty_err.msg() == 'empty'
	assert empty_err is EmptyError
}

fn test_const_referencing_interface_const() {
	assert big_err_alias.msg() == 'big 65536'
}

fn test_user_interface_cast_const() {
	assert square.area() == 49
	assert person.name == 'bob'
	assert person.greet() == 'bob is 3'
}

fn test_interface_field_in_const_struct() {
	assert explicit_holder.err.msg() == 'big 11'
	assert implicit_holder.err.msg() == 'big 12'
	assert shape_holder.shape.area() == 16
}

fn test_arrays_of_interface_casts_in_const() {
	assert fixed_errors[0].msg() == 'big 21'
	assert fixed_errors[1].msg() == 'empty'
	assert fixed_shapes[0].area() + fixed_shapes[1].area() == 13
	assert error_list.map(it.msg()) == ['big 31', 'empty']
}
