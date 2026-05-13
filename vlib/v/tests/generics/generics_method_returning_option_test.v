import json

pub struct NotificationMessage[T] {
pub:
	method string
	params T
}

struct Abc {}

pub fn (x &Abc) notification_at[T]() !NotificationMessage[T] {
	return json.decode(NotificationMessage[T], '{}')
}

pub fn (x &Abc) generic_method[T](method_name string) !NotificationMessage[T] {
	return x.notification_at[T]()
}

struct Res {}

pub fn (mut x Abc) diagnostics() !Res {
	got := x.generic_method[Res]('xyz')!
	return got.params
}

fn test_generic_method_returning_option() {
	mut a := Abc{}
	a.diagnostics()!
	assert true
}

interface Component {
	is_component()
}

struct ActionRow {
	components []Component
}

fn (_ ActionRow) is_component() {}

fn (ar ActionRow) walk[T](f fn (T) bool) ?T {
	for c in ar.components {
		if c is T {
			if f(c) {
				return c
			}
		} else if c is ActionRow {
			if d := c.walk(f) {
				return d
			}
		}
	}
	return none
}

struct TextInput {
	custom_id string
	value     ?string
}

fn (_ TextInput) is_component() {}

fn test_recursive_interface_method_inference() {
	ar := ActionRow{
		components: [
			TextInput{
				custom_id: 'foo'
				value:     'bar'
			},
		]
	}
	d := ar.walk(fn (ti TextInput) bool {
		return ti.custom_id == 'foo'
	}) or { panic('expected TextInput') }
	assert d.custom_id == 'foo'
	assert d.value or { '' } == 'bar'
}
