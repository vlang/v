// `mut app.field` for a reference field passes the stored pointer to a `mut`
// struct parameter; a value field passes its address. Both must reach the same
// object in a specialized generic call as in an ordinary one.
struct Box[T] {
mut:
	name string
	val  T
}

struct Plain {
mut:
	name string
}

struct App {
mut:
	boxed &Box[int]
	value Box[int]
	plain &Plain
	boxes []&Box[int]
}

fn rename[T](mut b Box[T], name string) string {
	old := b.name
	b.name = name
	return old
}

fn rename_plain[T](mut p Plain, name T) string {
	old := p.name
	p.name = '${name}'
	return old
}

fn rename_int(mut b Box[int], name string) string {
	old := b.name
	b.name = name
	return old
}

fn replace_slot[T](mut slot &Box[T], name string) {
	slot = &Box[T]{
		name: name
	}
}

fn new_app() App {
	return App{
		boxed: &Box[int]{
			name: 'boxed'
		}
		value: Box[int]{
			name: 'value'
		}
		plain: &Plain{
			name: 'plain'
		}
		boxes: [&Box[int]{
			name: 'first'
		}]
	}
}

fn test_reference_field_of_heap_struct() {
	mut app := &App{
		...new_app()
	}
	assert rename(mut app.boxed, 'generic') == 'boxed'
	assert app.boxed.name == 'generic'
	assert rename_int(mut app.boxed, 'plain fn') == 'generic'
	assert app.boxed.name == 'plain fn'
	assert rename_plain(mut app.plain, 7) == 'plain'
	assert app.plain.name == '7'
}

fn test_reference_field_of_value_struct() {
	mut app := new_app()
	assert rename(mut app.boxed, 'generic') == 'boxed'
	assert app.boxed.name == 'generic'
}

fn test_value_field_is_passed_by_address() {
	mut app := new_app()
	assert rename(mut app.value, 'changed') == 'value'
	assert app.value.name == 'changed'
}

fn test_reference_array_element() {
	mut app := new_app()
	assert rename(mut app.boxes[0], 'element') == 'first'
	assert app.boxes[0].name == 'element'
}

fn test_mut_reference_parameter_still_receives_the_slot() {
	mut app := new_app()
	replace_slot(mut app.boxed, 'replaced')
	assert app.boxed.name == 'replaced'
}
